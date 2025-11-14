
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
#' @param base_cohort *tabular input* || **required**
#'
#'   A table representing all patients who meet the base cohort definition, used
#'   as the "gold standard" for comparative analyses. This table should have at least:
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer* / *character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#'   Note that the start and end dates included in this table will be used to
#'   limit the search window for the analyses in this module.
#'
#' @param alt_cohorts *tablular input or list of tabular inputs* || **required**
#'
#'   A table or list of tables (can be named or unnamed) representing patients
#'   meeting alternative cohort definitions, which will be assessed against the
#'   base cohort. These tables should have the same structure as the `base_cohort.`
#'
#'   If an unnamed list is provided, numbers will be assigned to each cohort
#'   definition for labeling purposes. For named lists, the names will be used to
#'   label the alternate cohorts.
#'
#' @param omop_or_pcornet *string* || **required**
#'
#'   A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#'    - `omop`: run the [ssc_process_omop()] function against an OMOP CDM instance
#'    - `pcornet`: run the [ssc_process_pcornet()] function against a PCORnet CDM instance
#'
#' @param multi_or_single_site *string* || defaults to `single`
#'
#'   A string, either `single` or `multi`, indicating whether a single-site or
#'   multi-site analysis should be executed
#'
#' @param anomaly_or_exploratory *string* || defaults to `exploratory`
#'
#'   A string, either `anomaly` or `exploratory`, indicating what type of results
#'   should be produced.
#'
#'   Exploratory analyses give a high level summary of the data to examine the
#'   fact representation within the cohort. Anomaly detection analyses are
#'   specialized to identify outliers within the cohort.
#'
#' @param person_tbl *tabular input* || defaults to `cdm_tbl('person')`
#'
#'   The CDM `person` or `demographic` table that contains basic demographic information
#'   (sex, race, etc.) about the cohort members
#'
#' @param visit_tbl *tabular input* || defaults to `cdm_tbl('visit_occurrence')`
#'
#'   The CDM `visit_occurrence`, `visit_detail`, or `encounter` table that contains
#'   information about the patients' healthcare encounters
#'
#' @param provider_tbl *tabular input* || defaults to `NULL`
#'
#'   The CDM table with provider & provider specialty information. This table is
#'   only used if a `specialty_concepts` concept set is provided.
#'
#' @param care_site_tbl *tabular input* || defaults to `NULL`
#'
#'   The CDM table with care site/facility & related specialty information. This table is
#'   only used if a `specialty_concepts` concept set is provided.
#'
#' @param demographic_mappings *tabular input* || defaults to `NULL`
#'
#'   A table defining how demographic elements should be defined. If left as NULL,
#'   the default demographic mappings for the CDM will be used, which can be viewed
#'   in `ssc_omop_demographics` and `ssc_pcornet_demographics`. If a different table is
#'   provided by the user, those definition will be used instead. This table should
#'   minimally contain:
#'   - `demographic` | *character* | the label for the demographic category (ex: female, asian_race)
#'   - `concept_field` | *character* | the field in the CDM where this information is stored
#'   - `field_values` | *character* | the concept or concepts that are used to define the demographic category
#'
#' @param specialty_concepts *tabular input* || defaults to `NULL`
#'
#'   A concept set with concepts representing the specialties of interest to be
#'   identified for the cohort. This table should minimally contain either
#'   the `concept_id` field (OMOP) or the `concept_code` field (PCORnet)
#'
#' @param outcome_concepts *tabular input* || defaults to `NULL`
#'
#'   A concept set used to define any relevant outcome variables to be assessed
#'   in each cohort. This input should contain one of following:
#'   - `concept_id` | *integer* | the concept_id of interest (required for OMOP)
#'   - `concept_code` | *character* | the code of interest (required for PCORnet)
#'
#'   And both of:
#'   - `variable` | *character* | a string label grouping one concept code into a larger variable definition
#'   - `domain` | *character* | the name of the CDM table where the concept can be found
#'
#'   For certain PCORnet applications, it should also contain
#'   - `vocabulary_id` | *character* | the vocabulary of the code, which should match what is listed in the domain table's `vocabulary_field`
#'
#'   To see an example of this file structure, see `?sensitivityselectioncriteria::ssc_outcome_file`
#'
#' @param domain_tbl *tabular input* || defaults to the internal `ssc_domain_file`
#'
#'   A table or CSV file defining the domains to be assessed both for facts per patient year
#'   computations & for any outcome variables. This input should contain:
#'   - `domain` | *character* | a string label for the domain
#'   - `domain_tbl` | *character* | the name of the CDM table where this domain is defined
#'   - `concept_field` | *character*| the string name of the field in the domain table where the concepts are located
#'   - `date_field` | *character* |  the name of the field in the domain table with the date that should be used for temporal filtering
#'   - `vocabulary_field` | *character* | for PCORnet applications, the name of the field in the domain table with a vocabulary identifier to differentiate concepts from one another (ex: dx_type); can be set to NA for OMOP applications
#'   - `filter_logic` | *character* | logic to be applied to the domain_tbl in order to achieve the definition of interest; should be written as if you were applying it in a `dplyr::filter` command in R
#'
#'   To see an example of this file structure, see `?sensitivityselectioncriteria::ssc_domain_file`
#'
#' @param domain_select *string or vector* || defaults to all internal `ssc_domain_file` domains
#'
#'   A string or vector of domain names, matching those listed in `domain_tbl`,
#'   that should be used in the computation of facts per patient year
#'
#'   This vector does NOT need to include domains used in the computation for
#'   outcomes, as those will be accessed from the `domain_tbl` separately
#'
#' @return This function will return a dataframe summarizing fact and
#'         demographic distributions for each of the provided cohort definitions.
#'         For a more detailed description of output specific to each check type,
#'         see the PEDSpace metadata repository
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
    ssc_rslt[[1]] <- ssc_rslt[[1]] %>% mutate(output_function = paste0(output_type$string))
  }else{
    ssc_rslt <- ssc_rslt %>% mutate(output_function = paste0(output_type$string))
  }

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
              '`ssc_output` function. Here are the parameters you will need:', '', output_type$vector, '',
              'See ?ssc_output for more details.'), padding = c(0,1,0,1),
            header = cli::col_cyan('Output Function Details')))

  return(ssc_rslt)

}
