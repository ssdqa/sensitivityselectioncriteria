## code to prepare `ssc_domain_file` dataset goes here

ssc_domain_file <- tidyr::tibble(domain = c('inpatient_conditions', 'drugs'),
                                 domain_tbl = c('diagnosis', 'drug_exposure'),
                                 concept_field = c('dx', 'drug_concept_id'),
                                 date_field = c('admit_date', 'drug_start_date'),
                                 vocabulary_field = c('dx_type', NA),
                                 filter_logic = c('enc_type %in% c(IP, EI)', NA))

usethis::use_data(ssc_domain_file, overwrite = TRUE)
