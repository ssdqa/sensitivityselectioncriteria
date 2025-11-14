

#' Sensitivity to Selection Criteria -- OMOP
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
#' @param provider_tbl the CDM table with provider & provider specialty information
#'                     only used if `specialty_concepts` are provided
#'                     if provider_tbl & care_site_tbl are both not null, provider specialty is
#'                     prioritized
#' @param care_site_tbl the CDM table with care site / facility & care site / facility specialty information
#'                      only used if `specialty_concepts` are provided
#'                      if provider_tbl & care_site_tbl are both not null, provider specialty is
#'                      prioritized
#' @param demographic_mappings table defining how demographic elements should be defined
#'                             if NULL, the default demographic mappings for the CDM will be used
#'                             (`ssc_omop_demographics`)
#'                             otherwise, the user provided table will be used
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
#' @keywords internal
#'
ssc_process_omop <- function(base_cohort,
                             alt_cohorts,
                             multi_or_single_site = 'single',
                             anomaly_or_exploratory = 'exploratory',
                             person_tbl = cdm_tbl('person'),
                             visit_tbl = cdm_tbl('visit_occurrence'),
                             provider_tbl = cdm_tbl('provider'),
                             care_site_tbl = cdm_tbl('care_site'),
                             demographic_mappings = sensitivityselectioncriteria::ssc_omop_demographics,
                             specialty_concepts = NULL,
                             outcome_concepts = NULL,
                             domain_tbl = sensitivityselectioncriteria::ssc_domain_file,
                             domain_select = sensitivityselectioncriteria::ssc_domain_file %>%
                               distinct(domain) %>% pull()){

  ## Generate patient level output
  pt_lv_chars <- compare_cohort_def_omop(base_cohort = base_cohort,
                                         alt_cohorts = alt_cohorts,
                                         multi_or_single_site = multi_or_single_site,
                                         person_tbl = person_tbl,
                                         visit_tbl = visit_tbl,
                                         provider_tbl = provider_tbl,
                                         care_site_tbl = care_site_tbl,
                                         demographic_mappings = demographic_mappings,
                                         specialty_concepts = specialty_concepts,
                                         outcome_concepts = outcome_concepts,
                                         domain_defs = domain_tbl,
                                         domain_select = domain_select)

  pt_lv_chars <- pt_lv_chars %>% replace_site_col()

  #if(!time){

    if(anomaly_or_exploratory == 'exploratory'){

      ssc_tbl <- compute_cohort_summaries(cohort_def_output = pt_lv_chars,
                                          demographic_vector = demographic_mappings %>%
                                            distinct(demographic) %>% pull())
      if(multi_or_single_site == 'single'){
        ssc_tbl[[1]] <- ssc_tbl[[1]] %>% select(-c(allsite_median, allsite_q1, allsite_q3))
      }

    }else if(anomaly_or_exploratory == 'anomaly'){

      ssc_tbl <- compare_cohort_smd(cohort_def_output = pt_lv_chars,
                                    demographic_vector = demographic_mappings %>%
                                      distinct(demographic) %>% pull())

    }

  #}

  return(ssc_tbl)
}
