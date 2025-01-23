#'-----------------------------------------------------------------------------
# 
# Input files:
# 
# Resources:
#  - CCSR codes: 
#     https://github.com/HHS-AHRQ/MEPS/blob/master/Quick_Reference_Guides/meps_ccsr_conditions.csv
# 
#  - MEPS-HC Public Use Files: 
#     https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp
# 
#  - MEPS-HC data tools: 
#     https://datatools.ahrq.gov/meps-hc
#'-----------------------------------------------------------------------------  


# Load libraries
library(dplyr)
library(tidyr)
library(survey)
library(haven)
library(MEPS)


# Set global options 

options(survey.lonely.psu="adjust") # survey option for lonely PSUs
options(dplyr.width = Inf) # so columns won't be truncated when printing
options(digits = 10) # so big numbers aren't defaulted to scientific notation


# Load datasets ---------------------------------------------------------------

# IP   = Hospital stay, inpatient stay file (record = hospital stay)
# COND = Medical conditions file (record = medical condition)
# CLNK = Conditions-event link file (crosswalk between conditions and events)
# FYC  = Full year consolidated file (record = MEPS sample person)


# Option 1 - load data files using read_MEPS from the MEPS package
ip19  = read_MEPS(year = 2019, type = "IP")
cond19 = read_MEPS(year = 2019, type = "COND")
clnk19 = read_MEPS(year = 2019, type = "CLNK")

# Keep only needed variables --------------------------------------------------

#  Browse variables using MEPS-HC data tools variable explorer: 
#  -> http://datatools.ahrq.gov/meps-hc#varExp


ip19x   = ip19 %>% 
    select( DUPERSID, EVNTIDX, PERWT19F, VARPSU, VARSTR)

cond19x = cond19 %>% 
    select(DUPERSID, CONDIDX, ICD10CDX, CCSR1X:CCSR3X)

fyc19x  = fyc19 %>% 
    select(DUPERSID, PERWT19F, VARSTR, VARPSU,RACE)

# Filter COND file to only condition that can end-up in hospitalization based on the PQI index -----------------------

conditions = cond19x %>% 
    # filter(ICD10CDX=='E11')
    filter(ICD10CDX %in% c('E11', 'I10','J44', "J45", "I50",'J18', 'N39' ))  # Include both diabetes and hypertension

# view ICD10-CCSR combinations for mental health
conditions %>% 
    count(ICD10CDX, CCSR1X, CCSR2X, CCSR3X) %>% 
    print(n = 100)

# Filter CLNK file to only Inpatient-stay visits --------------------------------
#  EVENTYPE:
#   1 = "Office-based"
#   2 = "Outpatient" 
#   3 = "Emergency room"
#   4 = "Inpatient stay"
#   7 = "Home health"
#   8 = "Prescribed medicine"                                            

clnk_ob = clnk19 %>% 
    filter(EVENTYPE == 4)


# Merge datasets --------------------------------------------------------------

# Merge conditions file with the conditions-event link file (CLNK)

cd_clnk = inner_join(
    conditions,
    clnk_ob,
    by = c("DUPERSID", "CONDIDX"),
    relationship= "many-to-many")

# De-duplicate by event ID ('EVNTIDX'), since someone can have multiple events 
cd_clnk_nodup = cd_clnk %>%  
    group_by(DUPERSID, EVNTIDX, EVENTYPE) %>% 
    slice(1) %>%  # Keep the first row for each group
    ungroup()

# Merge on event files --------------------------------------------------------
ip_condition = inner_join(ip19x, cd_clnk_nodup) %>% 
    # Add indicator variables for all visits to help with counting later:
    mutate(
        ip_diabetes = ifelse(ICD10CDX == 'E11', 1, 0),     # Indicator for hospitalization due to diabetes
        ip_hypertension = ifelse(ICD10CDX == 'I10', 1, 0),#  Indicator for hospitalization due to hypertension
        ip_copd = ifelse(ICD10CDX %in% c('J44', "J45") , 1, 0), # Indicator for hospitalization due to COPD or ASthma
        ip_chf = ifelse(ICD10CDX == 'I50', 1, 0),#  Indicator for hospitalization due to Congestive Heart Failure (
        ip_pneumonia = ifelse(ICD10CDX == 'J18', 1, 0),#  Indicator for hospitalization due to Bacterial Pneumonia (
        ip_uti = ifelse(ICD10CDX == 'N39', 1, 0),# Indicator for hospitalization due to  Urinary Tract Infection
        ip_hosp =1
    )

# Merge on FYC file for complete Strata, PSUs ---------------------------------

ip_cd_fyc = full_join(
    ip_condition %>% mutate(cd_ip = 1), 
    fyc19x %>% mutate(fyc = 1)) 

# Person-level estimates ------------------------------------------------------

# Aggregate to person-level 
pers_cd = ip_cd_fyc %>% 
    group_by(DUPERSID, VARSTR, VARPSU, PERWT19F) %>% 
    summarize(
        ip_visit_diabetes = max(ip_diabetes) , # Proportion hospitalized for diabetes
        ip_visit_hypertension = max(ip_hypertension),
        ip_visit_copd = max(ip_copd),
        ip_visit_chf = max(ip_chf),
        ip_visit_pneumonia = max(ip_pneumonia), 
        ip_visit_uti= max(ip_uti),
        avoidble_hosp = mean(ip_hosp),
        
    ) %>%
    
    replace_na(
        list(avoidable_hospitalization = 0 )
    ) %>% # replace missings with 0s
    
    mutate(across(starts_with("ip"), ~ replace_na(.x, 0)))  # Replace missing values with 0 for all `visit_*` columns
%>% 
    set_variable_labels(
        ip_visit_diabetes = "Hospital Stay Due to Diabetes",
        ip_visit_hypertension = "Hospital Stay Due to Hypertension",
        ip_visit_copd = "Hospital Stay Due to COPD",
        ip_visit_chf = "Hospital Stay Due to CHF",
        ip_visit_pneumonia = "Hospital Stay Due to Pneumonia",
        ip_visit_uti = "Hospital Stay Due to UTI",
        avoidable_hospitalization = "Avoidable Hospital Stay"
    )

# Merge back into fyc

fyc19 <- fyc19 %>%
    left_join(pers_cd, by = c("DUPERSID" , 'VARSTR', 'VARPSU', 'PERWT19F'))
