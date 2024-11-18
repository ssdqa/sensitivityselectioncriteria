
## Testing error functionality
test_that('only single & multi are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(ssc_process(base_cohort = cht,
                           alt_cohorts = list(cht),
                           multi_or_single_site = 'test',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet = 'omop'))
})


test_that('only anomaly & exploratory are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(ssc_process(base_cohort = cht,
                           alt_cohorts = list(cht),
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'test',
                           omop_or_pcornet = 'omop'))
})

test_that('only omop & pcornet are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(ssc_process(base_cohort = cht,
                           alt_cohorts = list(cht),
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet = 'test'))
})


## Generally checking that code runs
test_that('ssc exp nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'ssc_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = '2009-01-01',
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  altc1 <- cohort %>% head(10) %>%
    mutate(start_date = '2009-01-01',
           end_date = as.Date('2020-01-01'),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  ssc_domain_input <- dplyr::tibble(domain = c('diagnoses', 'inpatient visits'),
                                    domain_tbl = c('condition_occurrence',
                                                   'visit_occurrence'),
                                    concept_field = c('condition_concept_id',
                                                      'visit_concept_id'),
                                    date_field = c('condition_start_date',
                                                   'visit_start_date'),
                                    vocabulary_field = c(NA, NA),
                                    filter_logic = c(NA, "visit_concept_id == 9202"))

  ssc_outcome_input <- read_codeset('dx_hypertension') %>%
    mutate(variable = 'hypertension',
           domain = 'diagnoses')

  expect_no_error(ssc_process(base_cohort = cohort,
                              alt_cohorts = list(altc1),
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              domain_tbl = ssc_domain_input,
                              domain_select = ssc_domain_input %>% distinct(domain) %>% pull(),
                              outcome_concepts = ssc_outcome_input,
                              #specialty_concepts = read_codeset('hematology_specialty'),
                              person_tbl = cdm_tbl('person'),
                              visit_tbl = cdm_tbl('visit_occurrence')))

})


test_that('ssc anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'ssc_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = '2009-01-01',
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  altc1 <- cohort %>% head(10) %>%
    mutate(start_date = '2009-01-01',
           end_date = as.Date('2020-01-01'),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  ssc_domain_input <- dplyr::tibble(domain = c('diagnoses', 'inpatient visits'),
                                    domain_tbl = c('condition_occurrence',
                                                   'visit_occurrence'),
                                    concept_field = c('condition_concept_id',
                                                      'visit_concept_id'),
                                    date_field = c('condition_start_date',
                                                   'visit_start_date'),
                                    vocabulary_field = c(NA, NA),
                                    filter_logic = c(NA, "visit_concept_id == 9202"))

  ssc_outcome_input <- read_codeset('dx_hypertension') %>%
    mutate(variable = 'hypertension',
           domain = 'diagnoses')

  expect_no_error(ssc_process(base_cohort = cohort,
                              alt_cohorts = list(altc1),
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'anomaly',
                              domain_tbl = ssc_domain_input,
                              domain_select = ssc_domain_input %>% distinct(domain) %>% pull(),
                              outcome_concepts = ssc_outcome_input,
                              #specialty_concepts = read_codeset('hematology_specialty'),
                              person_tbl = cdm_tbl('person'),
                              visit_tbl = cdm_tbl('visit_occurrence')))

})


test_that('ssc anom nt -- pcornet', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'ssc_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = '2009-01-01',
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  altc1 <- cohort %>% head(10) %>%
    mutate(start_date = '2009-01-01',
           end_date = as.Date('2020-01-01'),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  ssc_domain_input <- dplyr::tibble(domain = c('diagnoses', 'inpatient visits'),
                                    domain_tbl = c('condition_occurrence',
                                                   'visit_occurrence'),
                                    concept_field = c('condition_concept_id',
                                                      'visit_concept_id'),
                                    date_field = c('condition_start_date',
                                                   'visit_start_date'),
                                    vocabulary_field = c(NA, NA),
                                    filter_logic = c(NA, "visit_concept_id == 9202"))

  ssc_outcome_input <- read_codeset('dx_hypertension') %>%
    mutate(variable = 'hypertension',
           domain = 'diagnoses')

  expect_error(ssc_process(base_cohort = cohort,
                          alt_cohorts = list(altc1),
                          omop_or_pcornet = 'pcornet',
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'anomaly',
                          domain_tbl = ssc_domain_input,
                          domain_select = ssc_domain_input %>% distinct(domain) %>% pull(),
                          outcome_concepts = ssc_outcome_input,
                          #specialty_concepts = read_codeset('hematology_specialty'),
                          person_tbl = cdm_tbl('person'),
                          visit_tbl = cdm_tbl('visit_occurrence')))

})
