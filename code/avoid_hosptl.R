library(foreign)
library(devtools)
library(survey) # handeling survey package
library(tidyverse)
library(haven)
library(labelled)
library(broom)
library(MEPS)  # readin MEPS data
library(readxl) # reading 
library(fixest)

#####################
# read condition data
##################### 
yearset<- 2011:2019
condsets <- vector("list", length(2016:2019))

for( i in seq_along(yearset)) {
    
    condsets[[i]] = read_MEPS(year = yearset[[i]] , type = "COND")
    
}

COND16 <- condsets[[1]]
COND17 <- condsets[[2]]
COND18 <- condsets[[3]]
COND19 <- condsets[[4]]




#######################################
# Create variable Avoidable Mortality 
# for years 2016-2019 based on ICD 10 
# #######################################


#for each DUPERSID check if ICD10CDX is among c("E11") if ICD 10 was among this then check if IPNUM > 0 then AvdHOSP == 1 if 


fycsets <- list(fyc16, fyc17, fyc18, fyc19)


for (i in seq_along(fycsets)) {
    
    condata <- condsets[[i+5]]
    fycdata <- fycsets[[i]]
    
    
    condata <- condata %>%
        mutate (
            ip_diabetes = ifelse(ICD10CDX == 'E11' & IPNUM > 0, 1, 0),     # Indicator for hospitalization due to diabetes
            ip_hypertension = ifelse(ICD10CDX == 'I10' & IPNUM > 0, 1, 0),#  Indicator for hospitalization due to hypertension
            ip_copd = ifelse(ICD10CDX %in% c('J44', 'J45') & IPNUM > 0 , 1, 0), # Indicator for hospitalization due to COPD or ASthma
            ip_chf = ifelse(ICD10CDX == 'I50' & IPNUM > 0, 1, 0),#  Indicator for hospitalization due to Congestive Heart Failure (
            ip_pneumonia = ifelse(ICD10CDX == 'J18' & IPNUM > 0, 1, 0),#  Indicator for hospitalization due to Bacterial Pneumonia (
            ip_uti = ifelse(ICD10CDX == 'N39' & IPNUM > 0, 1, 0),# Indicator for hospitalization due to  Urinary Tract Infection
            ip_AvdHOSP = ifelse(ICD10CDX %in% c('E11', 'I10', 'J44', 'J45', 'I50', 'J18', 'N39') & IPNUM > 0, 1, 0)
        )
    
    #class(COND18$ICD10CDX)
    data_aggregated = condata %>% 
        group_by(DUPERSID, VARSTR, VARPSU ) %>% 
        summarize(
            ip_diabetes = max(ip_diabetes) , # Proportion hospitalized for diabetes
            ip_hypertension = max(ip_hypertension),
            ip_copd = max(ip_copd),
            ip_chf = max(ip_chf),
            ip_pneumonia = max(ip_pneumonia), 
            ip_uti= max(ip_uti),
            ip_AvdHOSP = max(ip_AvdHOSP),
            
        ) %>% # replace missings with 0s
        mutate(across(starts_with("ip"), ~ replace_na(.x, 0))
        )%>%   # Replace missing values with 0 for all `visit_*` columns
        set_variable_labels(
            ip_diabetes = "Hospital Stay Due to Diabetes",
            ip_hypertension = "Hospital Stay Due to Hypertension",
            ip_copd = "Hospital Stay Due to COPD",
            ip_chf = "Hospital Stay Due to CHF",
            ip_pneumonia = "Hospital Stay Due to Pneumonia",
            ip_uti = "Hospital Stay Due to UTI",
            ip_AvdHOSP = "Avoidable Hospital Stay"
        )
    
    
    
    fycjoined <- fycdata %>%          # Merge Avoidable Hospitalization with annual data
        
        left_join(data_aggregated
        ) %>%
        mutate(across(starts_with("ip"), ~ replace_na(.x, 0)))
    
    
    fycsets[[i]] <- fycjoined
    
}



agfyc16<-fycsets[[1]]
agfyc17<-fycsets[[2]]
agfyc18<-fycsets[[3]]
agfyc19<-fycsets[[4]]



#######################################
# Create variable Avoidable Mortality 
# for years 2016-2019 based on ICD 9 
# #######################################

fycsets <- list(fyc11, fyc12, fyc13, fyc14,fyc15)


for (i in seq_along(fycsets)) {
    
    condata <- condsets[[i]]
    fycdata <- fycsets[[i]]
    
    
    condata <- condata %>%
        mutate (
            ip_diabetes = ifelse(ICD9CODX == '250' & IPNUM > 0, 1, 0),     # Indicator for hospitalization due to diabetes
            ip_hypertension = ifelse(ICD9CODX %in% c('401', '402', '403') & IPNUM > 0, 1, 0),#  Indicator for hospitalization due to hypertension
            ip_copd = ifelse(ICD9CODX %in% c('491','490', '496', '466','492', '494') & IPNUM > 0 , 1, 0), # Indicator for hospitalization due to COPD or ASthma
            ip_chf = ifelse(ICD9CODX %in% c('428','398') & IPNUM > 0, 1, 0),#  Indicator for hospitalization due to Congestive Heart Failure (
            ip_pneumonia = ifelse(ICD9CODX %in% c('480', '481', '482', '483', '486','486') & IPNUM > 0, 1, 0),#  Indicator for hospitalization due to Bacterial Pneumonia (
            ip_uti = ifelse(ICD9CODX %in% c('599') & IPNUM > 0, 1, 0),# Indicator for hospitalization due to  Urinary Tract Infection
            ip_AvdHOSP = ifelse(ICD9CODX  %in% c('250', '401', '402', '403', '491', '490', '492', '496', '466', '494',
                                                 '428', '398', '480', '481', '482', '483', '486', '599') & IPNUM > 0, 1, 0 )
        )
    
    #class(COND18$ICD10CDX)
    data_aggregated = condata %>% 
        group_by(DUPERSID, VARSTR, VARPSU ) %>% 
        summarize(
            ip_diabetes = max(ip_diabetes) , # Proportion hospitalized for diabetes
            ip_hypertension = max(ip_hypertension),
            ip_copd = max(ip_copd),
            ip_chf = max(ip_chf),
            ip_pneumonia = max(ip_pneumonia), 
            ip_uti= max(ip_uti),
            ip_AvdHOSP = max(ip_AvdHOSP),
            
        ) %>% # replace missings with 0s
        mutate(across(starts_with("ip"), ~ replace_na(.x, 0))
        )%>%   # Replace missing values with 0 for all `visit_*` columns
        set_variable_labels(
            ip_diabetes = "Hospital Stay Due to Diabetes",
            ip_hypertension = "Hospital Stay Due to Hypertension",
            ip_copd = "Hospital Stay Due to COPD",
            ip_chf = "Hospital Stay Due to CHF",
            ip_pneumonia = "Hospital Stay Due to Pneumonia",
            ip_uti = "Hospital Stay Due to UTI",
            ip_AvdHOSP = "Avoidable Hospital Stay"
        )
    
    
    
    fycjoined <- fycdata %>%          # Merge Avoidable Hospitalization with annual data
        
        left_join(data_aggregated
        ) %>%
        mutate(across(starts_with("ip"), ~ replace_na(.x, 0)))
    
    
    fycsets[[i]] <- fycjoined
    
}



agfyc11<-fycsets[[1]]
agfyc12<-fycsets[[2]]
agfyc13<-fycsets[[3]]
agfyc14<-fycsets[[4]]
agfyc15<-fycsets[[5]]





