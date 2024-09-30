


#' Sensitivity to Selection Criteria
#'
#' @param base_cohort a dataframe including all patients who meet the base cohort definition
#'                    should have the columns site | person_id | start_date | end_date
#' @param alt_cohorts a list or named list of dataframes with patients meeting alternative
#'                    cohort definitions; if names are not provided, numbers will be assigned to each
#'                    cohort definition for labelling purposes
#' @param multi_or_single_site direction to determine what kind of check to run
#'                             string that is either `multi` or `single`
#' @param anomaly_or_exploratory direction to determine what kind of check to run; a string
#'                               that is either `anomaly` or `exploratory`
#' @param person_tbl CDM `person` table
#' @param visit_tbl CDM `visit_occurrence` table
#' @param provider_tbl CDM `provider` table
#' @param black_codes list of codes that indicate that a patient is Black/African-American
#'                    defaults to standard OMOP vocabulary -- `8516`
#' @param white_codes list of codes that indicate that a patient is White/Caucasian
#'                    defaults to standard OMOP vocabulary -- `8527`
#' @param asian_codes list of codes that indicate that a patient is Asian
#'                    defaults to standard OMOP vocabulary -- `8515`
#' @param mixrace_codes list of codes that indicate that a patient is Mixed Race
#'                      defaults to standard OMOP vocabulary -- `44814659`
#' @param unknown_codes list of codes that indicate that a patient's race is Unknown
#'                      defaults to standard OMOP vocabulary -- `44814660`, `44814650`, `44814653`
#' @param other_codes list of codes that indicate that a patient's race is Unknown
#'                    defaults to -- `44814649`, `8657`, `8557`
#' @param hispanic_codes list of codes that indicate that a patient is Hispanic or Latino
#'                       defaults to standard OMOP vocabulary -- `38003563`
#' @param female_codes list of codes that inidicate that a patient is Female
#'                     defaults to standard OMOP vocabulary -- `8532`
#' @param specialty_concepts a concept set with provider specialty concepts of interest
#'                           to be used to identify specialty visits
#' @param outcome_concepts a concept set with the following columns:
#'                         `concept_id` | `concept_code` | `concept_name` | `vocabulary_id` | `variable` | `domain`
#'
#'                         where domain matches a domain listed in the `domain_defs` table
#' @param domain_tbl a table with domain definitions with the following columns:
#'                   domain, domain_tbl, concept_field, date_field, filter_logic
#' @param domain_select a vector of domain names that should be used in the computation
#'                      of facts per domain per year of follow up
#'
#'                      this vector does NOT need to include domains used in the computation for
#'                      outcomes, as those will be accessed from the table directly
#'
#' @return a dataframe summarizing fact and demographic distributions for each of the provided
#'         cohort definitions; for both anomaly detection outputs, the standardized mean difference
#'         between the base cohort and each alternate cohort definition is computed for each
#'         of the variables of interest
#'
#' @export
#'
#' @import ssdqa.gen
#' @import argos
#' @import dplyr
#' @import cli
#' @importFrom stringr str_wrap
#'
ssc_process <- function(base_cohort,
                        alt_cohorts,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        person_tbl = cdm_tbl('person'),
                        visit_tbl = cdm_tbl('visit_occurrence'),
                        provider_tbl = cdm_tbl('provider'),
                        black_codes = c('8516'),
                        white_codes = c('8527'),
                        asian_codes = c('8515'),
                        mixrace_codes = c('44814659'),
                        unknown_codes = c('44814660', '44814650', '44814653'),
                        other_codes = c('44814649', '8657', '8557'),
                        hispanic_codes = c('38003563'),
                        female_codes = c('8532'),
                        specialty_concepts = NULL,
                        outcome_concepts = NULL,
                        domain_tbl = sensitivityselectioncriteria::ssc_domain_file,
                        domain_select = c('inpatient_visits', 'outpatient_visits', 'emergency_visits',
                                          'other_visits', 'all_px', 'prescription_medications',
                                          'all_conds')){

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'ssc',
                                             as.list(environment())))

  ## Generate patient level output
  pt_lv_chars <- compare_cohort_def(base_cohort = base_cohort,
                                    alt_cohorts = alt_cohorts,
                                    multi_or_single_site = multi_or_single_site,
                                    person_tbl = person_tbl,
                                    visit_tbl = visit_tbl,
                                    provider_tbl = provider_tbl,
                                    black_codes = black_codes,
                                    white_codes = white_codes,
                                    asian_codes = asian_codes,
                                    mixrace_codes = mixrace_codes,
                                    unknown_codes = unknown_codes,
                                    other_codes = other_codes,
                                    hispanic_codes = hispanic_codes,
                                    female_codes = female_codes,
                                    specialty_concepts = specialty_concepts,
                                    outcome_concepts = outcome_concepts,
                                    domain_defs = domain_tbl,
                                    domain_select = domain_select)

  pt_lv_chars <- pt_lv_chars %>% replace_site_col()

  #if(!time){

    if(anomaly_or_exploratory == 'exploratory'){

      ssc_tbl <- compute_cohort_summaries(cohort_def_output = pt_lv_chars)

    }else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'single'){

      ssc_tbl <- compare_cohort_smd(cohort_def_output = pt_lv_chars)

    }else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'multi'){

      #ssc_tbl <- compare_cohort_mse(cohort_def_output = pt_lv_chars)
      ssc_tbl <- compare_cohort_smd(cohort_def_output = pt_lv_chars)

    }

  #}

  cli::cli_inform(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                       output function in ssc_output: ', output_type, 'nt.')))

  return(ssc_tbl)
}
