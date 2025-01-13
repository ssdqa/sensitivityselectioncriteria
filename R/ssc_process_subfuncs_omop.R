
#' Reformat cohort definition tables
#'
#' @param base_cohort table with the base cohort & inclusion criteria; must include
#'                    site, person_id, start_date, end_date
#' @param alt_cohorts list of each table with an alternative inclusion criteria definition
#'                 applied to the base_cohort
#' @param multi_or_single_site direction to determine what kind of check to run
#'                             string that is either `multi` or `single`
#' @param person_tbl the CDM table that contains patient information
#' @param visit_tbl the CDM table that contains visit/encounter information
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
#' @param domain_defs a table with domain definitions with the following columns:
#'                   domain, domain_tbl, concept_field, date_field, filter_logic
#' @param domain_select a vector of domain names that should be used in the computation
#'                      of facts per domain per year of follow up
#'
#'                      this vector does NOT need to include domains used in the computation for
#'                      outcomes, as those will be accessed from the table directly
#'
#' @return one combined dataframe with one row per patient in the original cohort
#'         with flags to show which cohort definitions apply to the patient.
#'
#'         flag columns will include `base_cohort` and `alt_cohort_#` columns with numbers
#'         appended that correspond to the tables position in the `def_tbls` list
#'
#'         `base_cohort` should equal 1 for all patients
#'
#' @importFrom purrr reduce
#' @importFrom tidyr replace_na
#'

compare_cohort_def_omop <- function(base_cohort,
                                   alt_cohorts,
                                   multi_or_single_site,
                                   person_tbl = cdm_tbl('person'),
                                   visit_tbl = cdm_tbl('visit_occurrence'),
                                   provider_tbl = NULL,
                                   care_site_tbl = NULL,
                                   demographic_mappings = sensitivityselectioncriteria::ssc_omop_demographics,
                                   specialty_concepts = NULL,
                                   outcome_concepts = NULL,
                                   domain_defs = sensitivityselectioncriteria::ssc_domain_file,
                                   domain_select = sensitivityselectioncriteria::ssc_domain_file %>%
                                     distinct(domain) %>% pull()){

  ## Filter to necessary domains
  domain_defs_filt <- domain_defs %>% filter(domain %in% domain_select)

  ## Add flags to cohorts
  bc_flag <- base_cohort %>%
    mutate(cohort_id = 'base_cohort') %>%
    collect()

  def_flag_list <- list()

  name_test <- names(alt_cohorts)

  if(is.null(name_test)){
    num_seq <- seq(1:length(alt_cohorts))
    names(alt_cohorts) <- num_seq
  }

  for(i in 1:length(alt_cohorts)){

    number <- names(alt_cohorts[i])

    def_flag <- alt_cohorts[[i]] %>%
      mutate(cohort_id = paste0('alt_cohort_', number)) %>%
      collect()

    def_flag_list[[i]] <- def_flag

  }

  def_flag_final <- purrr::reduce(.x = def_flag_list,
                                  .f = dplyr::union)

  ## Combine cohorts with labels, prep for analysis
  cohort_combo <- dplyr::union(bc_flag, def_flag_final)

  # Add site check
  site_filter <- check_site_type(cohort = cohort_combo,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list

  cohort_prep <- prepare_cohort(copy_to_new(df = cohort_filter),
                                omop_or_pcornet = 'omop') #%>% copy_to_new(df = .)


  fact_list <- list()

  ## Domain summaries
  domain_summary <- compute_domains_ssc(cohort = cohort_prep,
                                        site_col = site_col,
                                        grouped_list = c(site_col, 'person_id','start_date','end_date',
                                                         'fu', 'cohort_id'),
                                        domain_tbl = domain_defs_filt)

  fact_list[['domain']] <- domain_summary %>% collect()

  ## Demographic summaries
  demo_summary <- compute_demographic_summary_omop(cohort_tbl = cohort_prep,
                                                  site_col = site_col,
                                                  person_tbl = person_tbl,
                                                  visit_tbl = visit_tbl,
                                                  demographic_mappings = demographic_mappings)

  fact_list[['demo']] <- demo_summary

  ## Specialty summaries
  if(!is.null(specialty_concepts)){

    if(!is.null(provider_tbl) || !is.null(care_site_tbl)){

      spec_visits <- find_specialty_visits_omop(cohort = cohort_prep,
                                               site_col = site_col,
                                               specialty_concepts = specialty_concepts,
                                               grouped_list = c(site_col, 'person_id','start_date','end_date',
                                                                'fu', 'cohort_id'),
                                               provider_tbl = provider_tbl,
                                               care_site_tbl = care_site_tbl,
                                               visit_tbl = visit_tbl)

      fact_list[['spec']] <- spec_visits %>% collect() %>% distinct()

    }else{cli::cli_alert_warning('Neither provider nor care site/facility cdm tables were provided - skipped specialty visits')}
  }

  ## Outcome summaries
  if(!is.null(outcome_concepts)){

    outcome_cts <- find_outcomes_ssc(cohort = cohort_prep,
                                     site_col = site_col,
                                     domain_tbl = domain_defs,
                                     outcome_concepts = outcome_concepts)

    fact_list[['outcome']] <- outcome_cts

  }

  fact_list_final <- reduce(.x = fact_list,
                            .f = left_join) %>%
    mutate(across(where(is.numeric), ~replace_na(.,0))) %>%
    mutate(across(where(is.logical), ~replace_na(.,FALSE)))

  ## Output patient level data, will be summarized later
  return(fact_list_final)

}


#' Compute patient level fact summary
#'
#' @param cohort_tbl table with the original cohort & inclustion criteria; must include
#'                   site, person_id, start_date, end_date
#' @param site_col the column in the cohort_tbl with the site name(s)
#' @param person_tbl CDM `person` table
#' @param visit_tbl CDM `visit_occurrence` table
#' @param demographic_mappings table defining how demographic elements should be defined; defaults
#' to `ssc_omop_demographics`; any additional demographic elements must be found in the person_tbl
#'
#' @return one dataframe with one row for each patient with columns
#'         to show which facts apply to each patient:
#'
#'         fu, age_cohort_entry, age_first_visit, and each of the demographic
#'         elements found in demographic_mappings
#'
#' @importFrom stringr str_split
#' @importFrom rlang :=
#'

compute_demographic_summary_omop <- function(cohort_tbl,
                                            site_col,
                                            person_tbl = cdm_tbl('person'),
                                            visit_tbl = cdm_tbl('visit_occurrence'),
                                            demographic_mappings = sensitivityselectioncriteria::ssc_omop_demographics
                                            ){

  demo_list <- split(demographic_mappings, seq(nrow(demographic_mappings)))
  demo_rslt <- list()

  for(i in 1:length(demo_list)){

    vals <- demo_list[[i]]$field_values %>%
      str_replace_all(" ", "") %>% str_split(',') %>% unlist()

    demographic <- person_tbl %>%
      inner_join(cohort_tbl) %>%
      mutate(demo_col = ifelse(!!sym(demo_list[[i]]$concept_field) %in% vals, TRUE, FALSE)) %>%
      select(!!sym(site_col), person_id, start_date, end_date, fu, cohort_id, demo_col) %>%
      rename(!!sym(demo_list[[i]]$demographic) := demo_col) %>%
      collect()

    demo_rslt[[i]] <- demographic

  }

  demo_final <- purrr::reduce(.x = demo_rslt,
                              .f = left_join)

  new_person <- build_birth_date(cohort = cohort_tbl,
                                 person_tbl = person_tbl)

  age_ced <- new_person %>%
    mutate(age_cohort_entry = as.numeric(as.Date(start_date) - birth_date),
           age_cohort_entry = round(age_cohort_entry / 365.25, 2)) %>%
    distinct(!!sym(site_col), person_id, age_cohort_entry)

  if('visit_start_date' %in% colnames(visit_tbl)){date_col <- 'visit_start_date'}else{
    date_col <- 'visit_detail_start_date'
  }

  age_first_visit <- visit_tbl %>%
    select(person_id, !!sym(date_col)) %>%
    inner_join(cohort_tbl) %>%
    # inner_join(select(visit_tbl, person_id, visit_start_date)) %>%
    group_by(!!sym(site_col), person_id, cohort_id) %>%
    summarise(min_visit = min(!!sym(date_col))) %>%
    collect() %>%
    left_join(new_person) %>%
    mutate(age_first_visit = as.numeric(as.Date(min_visit) - birth_date),
           age_first_visit = round(age_first_visit / 365.25, 2)) %>%
    distinct(!!sym(site_col), person_id, age_first_visit) #%>%
    #collect()

  summ_tbl <- demo_final %>%
    left_join(age_first_visit) %>%
    left_join(age_ced)


}


#' Compute specialty visits per person year
#'
#' @param cohort the combined cohort with patients from the base and alternate
#'               cohort definitions, with a cohort_id label to differentiate
#' @param site_col the column in the cohort_tbl with the site name(s)
#' @param specialty_concepts a concept set with provider specialty concepts of interest
#'                           to be used to identify specialty visits
#' @param grouped_list a list of variables for grouping
#' @param provider_tbl the CDM table with provider & provider specialty information
#'                     only used if `specialty_concepts` are provided
#'                     if provider_tbl & care_site_tbl are both not null, provider specialty is
#'                     prioritized
#' @param care_site_tbl the CDM table with care site / facility & care site / facility specialty information
#'                      only used if `specialty_concepts` are provided
#'                      if provider_tbl & care_site_tbl are both not null, provider specialty is
#'                      prioritized
#' @param visit_tbl The CDM table with visit/encounter information
#'
#' @return a dataframe with the number of specialist visits per year of follow up
#'         for each of the patients in the cohort
#'
#' @importFrom tidyr tibble
#'
find_specialty_visits_omop <- function(cohort,
                                      site_col,
                                      specialty_concepts,
                                      grouped_list,
                                      provider_tbl = NULL,
                                      care_site_tbl = NULL,
                                      visit_tbl = cdm_tbl('visit_occurrence')){

  spec_db <- copy_to_new(df = specialty_concepts)

  if('visit_detail_id' %in% colnames(visit_tbl)){
    id_col <- 'visit_detail_id'
    visit_name <- 'visit_detail'
  }else{
    id_col <- 'visit_occurrence_id'
    visit_name <- 'visit_occurrence'
  }

  if(is.null(care_site_tbl) && !is.null(provider_tbl)){
    spec_visits <- visit_tbl %>%
      inner_join(cohort) %>%
      inner_join(provider_tbl %>% select(provider_id, specialty_concept_id)) %>%
      inner_join(spec_db, by = c('specialty_concept_id' = 'concept_id')) %>%
      select(all_of(grouped_list), cohort_id, !!sym(id_col)) %>% compute_new()
  }else if(!is.null(care_site_tbl) && is.null(provider_tbl)){
    spec_visits <- visit_tbl %>%
      inner_join(cohort) %>%
      inner_join(care_site_tbl %>% select(care_site_id, specialty_concept_id)) %>%
      inner_join(spec_db, by = c('specialty_concept_id' = 'concept_id')) %>%
      select(all_of(grouped_list), cohort_id, !!sym(id_col)) %>% compute_new()
  }else if(!is.null(care_site_tbl) && !is.null(provider_tbl)){
    spec_visits <- visit_tbl %>%
      inner_join(cohort) %>%
      left_join(provider_tbl %>% select(provider_id, specialty_concept_id) %>%
                  rename('pv_spec' = specialty_concept_id)) %>%
      left_join(care_site_tbl %>% select(care_site_id, specialty_concept_id) %>%
                  rename('cs_spec' = specialty_concept_id)) %>%
      mutate(specialty_concept_id = ifelse(is.na(pv_spec), cs_spec, pv_spec)) %>%
      select(-c(cs_spec, pv_spec)) %>%
      inner_join(spec_db, by = c('specialty_concept_id' = 'concept_id')) %>%
      select(all_of(grouped_list), cohort_id, !!sym(id_col)) %>% compute_new()
  }

  if(visit_name == 'visit_occurrence'){
    domain_tbl <- tibble('domain' = 'specialty_visits',
                         'domain_tbl' = 'visit_occurrence',
                         'concept_field' = 'visit_concept_id',
                         'date_field' = 'visit_start_date',
                         'filter_logic' = NA)
  }else{
    domain_tbl <- tibble('domain' = 'specialty_visits',
                         'domain_tbl' = 'visit_detail',
                         'concept_field' = 'visit_detail_concept_id',
                         'date_field' = 'visit_detail_start_date',
                         'filter_logic' = NA)
  }

  spec_visit_ppy <- compute_domains_ssc(cohort = spec_visits,
                                        site_col = site_col,
                                        grouped_list = grouped_list,
                                        domain_tbl = domain_tbl)


}

