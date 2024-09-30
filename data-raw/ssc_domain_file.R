## code to prepare `ssc_domain_file` dataset goes here

ssc_domain_file <- tidyr::tibble(domain = c('inpatient_conditions', 'drugs'),
                                 domain_tbl = c('condition_occurrence', 'drug_exposure'),
                                 concept_field = c('condition_concept_id', 'drug_concept_id'),
                                 date_field = c('condition_start_date', 'drug_start_date'),
                                 filter_logic = c('condition_type_concept_id %in% c(2000000092, 2000000093, 2000000094)', NA))

usethis::use_data(ssc_domain_file, overwrite = TRUE)
