library(tidyverse)
library(MEPS)

yearset <- 2011:2019
condsets <- vector("list", length(yearset))
fyc_names <- paste0("fyc", 11:19)  # Creates "fyc11", "fyc12", ..., "fyc15"
fycsets <- mget(fyc_names)
# Define ICD codes
icd10_codes <- list(
    diabetes = "E11",
    hypertension = "I10",
    copd = c("J44", "J45"),
    chf = "I50",
    pneumonia = "J18",
    uti = "N39"
)
icd9_codes <- list(
    diabetes = "250",
    hypertension = c("401", "402", "403"),
    copd = c("491", "490", "496", "466", "492", "494"),
    chf = c("428", "398"),
    pneumonia = c("480", "481", "482", "483", "486"),
    uti = "599"
)

# Process Condition Data
process_condsets <- function(condata, fycdata, icd_col, codes) {
    condata <- condata %>%
        mutate(
            ip_diabetes = ifelse(.data[[icd_col]] %in% codes$diabetes & IPNUM > 0, 1, 0),
            ip_hypertension = ifelse(.data[[icd_col]] %in% codes$hypertension & IPNUM > 0, 1, 0),
            ip_copd = ifelse(.data[[icd_col]] %in% codes$copd & IPNUM > 0, 1, 0),
            ip_chf = ifelse(.data[[icd_col]] %in% codes$chf & IPNUM > 0, 1, 0),
            ip_pneumonia = ifelse(.data[[icd_col]] %in% codes$pneumonia & IPNUM > 0, 1, 0),
            ip_uti = ifelse(.data[[icd_col]] %in% codes$uti & IPNUM > 0, 1, 0),
            ip_AvdHOSP = ifelse(.data[[icd_col]] %in% unlist(codes) & IPNUM > 0, 1, 0)
        ) %>%
        group_by(DUPERSID, VARSTR, VARPSU) %>%
        summarize(across(starts_with("ip"), max, na.rm = TRUE)) %>%
        mutate(across(starts_with("ip"), ~ replace_na(.x, 0)))
    
    fycdata %>%
        left_join(condata) %>%
        mutate(across(starts_with("ip"), ~ replace_na(.x, 0)))
}

# Loop through years and process

for (i in seq_along(yearset)) {
    condsets[[i]] <- read_MEPS(year = yearset[i], type = "COND")
    
    if (yearset[i] < 2016) {
        fycsets[[i]] <- process_condsets(condsets[[i]], fycsets[[i]], "ICD9CODX", icd9_codes)
    } else {
        fycsets[[i]] <- process_condsets(condsets[[i]], fycsets[[i]], "ICD10CDX", icd10_codes)
    }
    assign(paste0('fyc', substring(as.character(yearset[[i]]),3,4)),fycsets[[i]])
    
}

# Assign names for better tracking
# Access aggregated data
#year<-11:19
# (i in seq_along(11:19)) {
#paste0('agfyc', substring(yearset[[i]],2) <- fycsets[[i]]

table(agfyc12$ip_AvdHOSP)

#   yearset[[1]]


