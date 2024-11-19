
#' Sensitivity to Selection Criteria -- OMOP
#'
#' @param base_cohort a dataframe including all patients who meet the base cohort definition
#'                    should have the columns site | person_id | start_date | end_date
#' @param alt_cohorts a list or named list of dataframes with patients meeting alternative
#'                    cohort definitions; if names are not provided, numbers will be assigned to each
#'                    cohort definition for labelling purposes
#' @param omop_or_pcornet Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - `omop`: run the [ssc_process_omop()] function against an OMOP CDM instance
#' - `pcornet`: run the [ssc_process_pcornet()] function against a PCORnet CDM instance
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
#'                             (either `ssc_omop_demographics` or `ssc_pcornet_demographics`)
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
                         omop_or_pcornet,
                         multi_or_single_site = 'single',
                         anomaly_or_exploratory = 'exploratory',
                         person_tbl = cdm_tbl('person'),
                         visit_tbl = cdm_tbl('visit_occurrence'),
                         provider_tbl = NULL,
                         care_site_tbl = NULL,
                         demographic_mappings = NULL,
                         # black_codes = c('8516'),
                         # white_codes = c('8527'),
                         # asian_codes = c('8515'),
                         # mixrace_codes = c('44814659'),
                         # unknown_codes = c('44814660', '44814650', '44814653'),
                         # other_codes = c('44814649', '8657', '8557'),
                         # hispanic_codes = c('38003563'),
                         # female_codes = c('8532'),
                         specialty_concepts = NULL,
                         outcome_concepts = NULL,
                         domain_tbl = sensitivityselectioncriteria::ssc_domain_file,
                         domain_select = sensitivityselectioncriteria::ssc_domain_file %>%
                          distinct(domain) %>% pull()){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue'),
                            inform = list(color = 'green')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}

  ## parameter summary output
  param_args <- as.list(environment())

  param_args <- param_args[!names(param_args) %in% c('visit_tbl', 'provider_tbl', 'person_tbl',
                                                     'base_cohort', 'alt_cohorts', 'care_site_tbl')]

  output_type <- suppressWarnings(param_summ(check_string = 'ssc',
                                             param_args))

  if(tolower(omop_or_pcornet) == 'omop'){

    if(is.null(demographic_mappings)){demographic_mappings <- sensitivityselectioncriteria::ssc_omop_demographics}

    ssc_rslt <- ssc_process_omop(base_cohort = base_cohort,
                                 alt_cohorts = alt_cohorts,
                                 multi_or_single_site = multi_or_single_site,
                                 anomaly_or_exploratory = anomaly_or_exploratory,
                                 person_tbl = person_tbl,
                                 visit_tbl = visit_tbl,
                                 provider_tbl = provider_tbl,
                                 care_site_tbl = care_site_tbl,
                                 demographic_mappings = demographic_mappings,
                                 # black_codes = black_codes,
                                 # white_codes = white_codes,
                                 # asian_codes = asian_codes,
                                 # mixrace_codes = mixrace_codes,
                                 # unknown_codes = unknown_codes,
                                 # other_codes = other_codes,
                                 # hispanic_codes = hispanic_codes,
                                 # female_codes = female_codes,
                                 specialty_concepts = specialty_concepts,
                                 outcome_concepts = outcome_concepts,
                                 domain_tbl = domain_tbl,
                                 domain_select = domain_select)

  }else if(tolower(omop_or_pcornet) == 'pcornet'){
    if(is.null(demographic_mappings)){demographic_mappings <- sensitivityselectioncriteria::ssc_pcornet_demographics}

    ssc_rslt <- ssc_process_pcornet(base_cohort = base_cohort,
                                    alt_cohorts = alt_cohorts,
                                    multi_or_single_site = multi_or_single_site,
                                    anomaly_or_exploratory = anomaly_or_exploratory,
                                    person_tbl = person_tbl,
                                    visit_tbl = visit_tbl,
                                    provider_tbl = provider_tbl,
                                    care_site_tbl = care_site_tbl,
                                    demographic_mappings = demographic_mappings,
                                    # black_codes = black_codes,
                                    # white_codes = white_codes,
                                    # asian_codes = asian_codes,
                                    # mixrace_codes = mixrace_codes,
                                    # unknown_codes = unknown_codes,
                                    # other_codes = other_codes,
                                    # hispanic_codes = hispanic_codes,
                                    # female_codes = female_codes,
                                    specialty_concepts = specialty_concepts,
                                    outcome_concepts = outcome_concepts,
                                    domain_tbl = domain_tbl,
                                    domain_select = domain_select)

  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}


  cli::cli_inform(paste0(col_green('Based on your chosen parameters, we recommend using the following
                       output function in ssc_output: '), col_blue(style_bold(output_type,'nt.'))))

  return(ssc_rslt)

}
