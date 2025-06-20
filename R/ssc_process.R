
#' Sensitivity to Selection Criteria
#'
#' This is a plausibility module that will characterize user-provided base cohort
#' and alternate cohort definitions using a selection of continuous and categorical
#' variables. It will then use these values to compare each alternate definition to
#' the base cohort and evaluate how changes in the cohort definition impact the
#' patient population. Users have the option to provide definitions for several
#' variable types:
#' - `demographic_mappings`: define a set of demographic variables of interest
#' - `domain_tbl`: define domain definitions to assess patient fact density + utilization
#' (similar to Patient Facts module)
#' - `specialty_concepts`: define a set of specialty concepts to evaluate specialty
#' care sought out by the cohort members
#' - `outcome_concepts`: define study outcome variables
#' Sample versions of these input files are available with `sensitivityselectioncriteria::`.
#' This module is compatible with both the OMOP and PCORnet CDMs based on the user's
#' selection.
#'
#' @param base_cohort *tabular input* | a dataframe including all patients who meet the base cohort definition
#'                    should have the columns site | person_id | start_date | end_date
#' @param alt_cohorts *list of tabular inputs* | a list (can be named or unnamed) of dataframes with patients meeting alternative
#'                    cohort definitions; if names are not provided, numbers will be assigned to each
#'                    cohort definition for labelling purposes
#' @param omop_or_pcornet *string* | Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - `omop`: run the [ssc_process_omop()] function against an OMOP CDM instance
#' - `pcornet`: run the [ssc_process_pcornet()] function against a PCORnet CDM instance
#' @param multi_or_single_site *string* | direction to determine what kind of check to run
#'                             string that is either `multi` or `single`
#' @param anomaly_or_exploratory *string* | direction to determine what kind of check to run; a string
#'                               that is either `anomaly` or `exploratory`
#' @param person_tbl *tabular input* | CDM `person` or `demographic` table
#' @param visit_tbl *tabular input* | CDM `visit_occurrence`, `visit_detail`, or `encounter` table
#' @param provider_tbl *tabular input* | the CDM table with provider & provider specialty information
#'                     only used if `specialty_concepts` are provided
#'                     if provider_tbl & care_site_tbl are both not null, provider specialty is
#'                     prioritized
#' @param care_site_tbl *tabular input* | the CDM table with care site / facility & care site / facility specialty information
#'                      only used if `specialty_concepts` are provided
#'                      if provider_tbl & care_site_tbl are both not null, provider specialty is
#'                      prioritized
#' @param demographic_mappings *tabular input* | table defining how demographic elements should be defined
#'                             if NULL, the default demographic mappings for the CDM will be used
#'                             (either `ssc_omop_demographics` or `ssc_pcornet_demographics`)
#'                             otherwise, the user provided table will be used
#' @param specialty_concepts *tabular input* | a concept set with provider specialty concepts of interest
#'                           to be used to identify specialty visits
#' @param outcome_concepts *tabular input* | a concept set with the following columns:
#' - `concept_id` | *integer* | required for OMOP; the concept_id of interest
#' - `concept_code` | *character* | required for PCORnet; the code of interest
#' - `concept_name` | *character* | optional; the descriptive name of the concept
#' - `vocabulary_id` | *character* | required for PCORnet; the vocabulary of the code - should match what is listed in the domain table's vocabulary_field
#' - `variable` | *character* | required; a string label grouping one concept code into a larger variable definition
#' - `domain` | *character* | the CDM table (matching a domain listed in the `domain_defs` table) where the
#' outcome should be identified
#' @param domain_tbl *tabular input* | a table with domain definitions with the following columns:
#'                   domain, domain_tbl, concept_field, date_field, filter_logic
#' @param domain_select *string or vector* | a vector of domain names that should be used in the computation
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
#' @example inst/example-ssc_process_output.R
#'
#' @export
#'
#' @import squba.gen
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
                                    specialty_concepts = specialty_concepts,
                                    outcome_concepts = outcome_concepts,
                                    domain_tbl = domain_tbl,
                                    domain_select = domain_select)

  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}

  if(anomaly_or_exploratory == 'exploratory'){
    ssc_rslt[[1]] <- ssc_rslt[[1]] %>% mutate(output_function = paste0(output_type$string, 'cs'))
  }else{
    ssc_rslt <- ssc_rslt %>% mutate(output_function = paste0(output_type$string, 'cs'))
  }

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
              '`ssc_output` function. Here are the parameters you will need:', '', output_type$vector, '',
              'See ?ssc_output for more details.'), padding = c(0,1,0,1),
            header = cli::col_cyan('Output Function Details')))

  return(ssc_rslt)

}
