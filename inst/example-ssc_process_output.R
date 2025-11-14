
#' Source setup file
source(system.file('setup.R', package = 'sensitivityselectioncriteria'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'ssc_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)

#' Build mock base study cohort
base_cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000),
                #RSQLite does not store date objects,
                #hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Build mock alternate study cohort
alt_cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  head(100) %>%
  dplyr::mutate(start_date = as.Date(-5000),
                #RSQLite does not store date objects,
                #hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Prepare input tables
ssc_domain_tbl <- dplyr::tibble(domain = c('all conditions', 'outpatient visits'),
                                domain_tbl = c('condition_occurrence', 'visit_occurrence'),
                                concept_field = c('condition_concept_id', 'visit_concept_id'),
                                date_field = c('condition_start_date', 'visit_start_date'),
                                vocabulary_field = c(NA, NA),
                                filter_logic = c(NA, 'visit_concept_id == 9202'))

ssc_outcome_tbl <- read_codeset('dx_hypertension') %>%
  dplyr::mutate(variable = 'hypertension', domain = 'all conditions')

#' Execute `ssc_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
ssc_process_example <- ssc_process(base_cohort = base_cohort,
                                   alt_cohorts = list('Sample Alternate' = alt_cohort),
                                   omop_or_pcornet = 'omop',
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   domain_tbl = ssc_domain_tbl,
                                   domain_select = c('all conditions', 'outpatient visits'),
                                   outcome_concepts = ssc_outcome_tbl) %>%
  suppressMessages()

ssc_process_example

#' Execute `ssc_output` function
ssc_output_example <- ssc_output(process_output = ssc_process_example,
                                 alt_cohort_filter = 'Sample Alternate') %>%
  suppressMessages()

ssc_output_example[[1]]
ssc_output_example[[2]]

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(ssc_output_example[[2]])
