
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


