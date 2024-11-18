
#' Compute domain facts per person year
#'
#' @param cohort the combined cohort with patients from the base and alternate
#'               cohort definitions, with a cohort_id label to differentiate
#' @param site_col the column in cohort with the site name(s)
#' @param grouped_list a list of variables for grouping
#' @param domain_tbl a table with domain definitions that contains the following columns:
#'                   domain, domain_tbl, concept_field, date_field, filter_logic
#'
#' @return a dataframe with fact counts of the user-provided domains per year of follow-up
#'         within the cohort period for each of the patients in the cohort
#'
#' @importFrom rlang parse_expr
#'
compute_domains_ssc <- function(cohort,
                                site_col,
                                grouped_list,
                                domain_tbl) {

  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))

  if('person_id' %in% names(cohort)){person_col <- 'person_id'}else{person_col <- 'patid'}

  for (i in 1:length(domain_list)) {

    domain_name = domain_list[[i]]$domain
    message(paste0('Starting domain ', domain_list[[i]]$domain))

    ## checks to see if the table needs to be filtered in any way;
    ## allow for one filtering operation

    domain_tbl_use <- cdm_tbl(domain_list[[i]]$domain_tbl)

    if(! is.na(domain_list[[i]]$filter_logic)) {
      domain_tbl_use <- domain_tbl_use %>%
        filter(!! rlang::parse_expr(domain_list[[i]]$filter_logic))
    } else {domain_tbl_use <- domain_tbl_use}

    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    ssc <-
      domain_tbl_use %>%
      inner_join(cohort) %>%
      filter(!!sym(domain_list[[i]]$date_field) >= start_date,
             !!sym(domain_list[[i]]$date_field) <= end_date) %>%
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_fact_ct=n()) %>%
      ungroup() %>%
      mutate(domain=domain_name) %>%
      mutate(k_mult = case_when(fu < 0.1 ~ 100,
                                fu >= 0.1 & fu < 1 ~ 10,
                                TRUE ~ 1),
             fact_ppy=ifelse(fu != 0,round(total_fact_ct/(fu * k_mult),2),0)) %>%
      #select(-c(total_strat_ct, k_mult)) %>%
      select(!!sym(person_col),
             domain,
             fact_ppy) %>%
      pivot_wider(names_from=domain,
                  values_from=fact_ppy,
                  names_glue = "{domain}_ppy") %>%
      #right_join(cohort) %>%
      #relocate(person_id) %>%
      compute_new()

    domain_results[[domain_name]] <- ssc
  }

  domain_results_left_join <-
    reduce(.x=domain_results,
           .f=left_join) %>% right_join(select(cohort, site_col, !!sym(person_col), cohort_id))
}


#' Find presence of outcomes in cohort definitions
#'
#' @param cohort the combined cohort with patients from the base and alternate
#'               cohort definitions, with a cohort_id label to differentiate
#' @param site_col the column in the cohort_tbl with the site name(s)
#' @param domain_tbl a table with domain definitions with the following columns:
#'                   domain, domain_tbl, concept_field, date_field, filter_logic
#' @param outcome_concepts a concept set with the following columns:
#'                         `concept_id` | `concept_code` | `concept_name` | `vocabulary_id` | `domain`
#'
#'                         where domain matches a domain listed in the domain_tbl
#'
#' @return a dataframe with indicators for each patient identifying whether or not they
#'         have the outcome(s) of interest
#'
#' @importFrom tidyr pivot_wider
#' @importFrom purrr set_names
#'
find_outcomes_ssc <- function(cohort,
                              site_col,
                              domain_tbl,
                              outcome_concepts){

  concept_domain <- outcome_concepts %>%
    distinct(domain) %>% pull()

  if('person_id' %in% names(cohort)){person_col <- 'person_id'}else{person_col <- 'patid'}
  if('person_id' %in% names(cohort)){concept_col <- 'concept_id'}else{concept_col <- 'concept_code'}

  otcm_list <- list()

  for(i in 1:length(concept_domain)){

    domain_info <- domain_tbl %>% filter(domain == concept_domain[i])

    colnm <- domain_info$concept_field

    join_cols <- purrr::set_names(concept_col, colnm)

    if(!is.na(domain_info$vocabulary_field)){
      join_cols2 <- set_names('vocabulary_id', vocab_col)
      join_cols <- join_cols %>% append(join_cols2)
    }

    outcome_db <- copy_to_new(df = outcome_concepts)

    outcome_present <- cdm_tbl(domain_info$domain_tbl) %>%
      inner_join(cohort) %>%
      filter(!!sym(domain_info$date_field) >= start_date,
             !!sym(domain_info$date_field) <= end_date) %>%
      #rename('join_col' = colnm) %>%
      inner_join(outcome_db, by = join_cols) %>%
      distinct(!!sym(site_col), !!sym(person_col), variable) %>% collect() %>%
      mutate(has_outcome = TRUE)


    otcm_list[[i]] <- outcome_present

  }

  outcome_final <- reduce(.x = otcm_list,
                          .f = union)

  outcome_pivot <- outcome_final %>%
    pivot_wider(names_from = variable,
                values_from = has_outcome) %>%
    mutate(across(where(is.logical), ~replace_na(.,FALSE)))

}


#' Compute definition level summary
#'
#' @param cohort_def_output the output of
#'
#' @return a dataframe with summary output for each cohort definition. for demographics and outcomes,
#'         proportions are computed. for other variables, median fact counts ppy are computed
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unite
#' @importFrom stats median
#'
compute_cohort_summaries <- function(cohort_def_output){

  if('person_id' %in% names(cohort_def_output)){person_col <- 'person_id'}else{person_col <- 'patid'}

  ## Get patient totals
  cohort_totals <- cohort_def_output %>% group_by(site, cohort_id) %>%
    summarise(cohort_total_pt = n())

  ## Flip table
  cohort_flip <- cohort_def_output %>%
    pivot_longer(cols = !c(site, !!sym(person_col), start_date, end_date, cohort_id),
                 names_to = 'cohort_characteristic',
                 values_to = 'fact_value') %>%
    apply_cohort_labels()

  ## Summarise patient level data into medians (ppy vars) and proportions (demographic & outcome vars)
  find_medians <- cohort_flip %>%
    filter(fact_group %in% c('Cohort Details', 'Utilization', 'Clinical Facts')) %>%
    select(-!!sym(person_col)) %>% group_by(site, cohort_id, cohort_characteristic, fact_group) %>%
    summarise(fact_summary = median(fact_value)) %>%
    mutate(cohort_characteristic = paste0('median_', cohort_characteristic)) %>%
    left_join(cohort_totals)

  find_props <- cohort_flip %>%
    filter(fact_group %in% c('Demographics', 'Outcomes')) %>%
    group_by(site, cohort_id, cohort_characteristic, fact_group) %>%
    summarise(fact_summary = sum(fact_value)) %>%
    left_join(cohort_totals) %>%
    group_by(site, cohort_id, cohort_characteristic, fact_group) %>%
    summarise(fact_summary = (fact_summary/cohort_total_pt)) %>%
    mutate(cohort_characteristic = paste0('prop_', cohort_characteristic)) %>%
    left_join(cohort_totals)

  summ_tbl <- find_medians %>% union(find_props) %>% distinct()

  ## Find overlap between cohorts
  cht_overlap <- cohort_def_output %>%
    select(site, !!sym(person_col), cohort_id) %>%
    pivot_wider(names_from = cohort_id,
                values_from = cohort_id) %>%
    relocate(base_cohort, .after = !!sym(person_col)) %>%
    unite('cohort_group', base_cohort:last_col(), na.rm = TRUE,
          sep = "&") %>%
    group_by(site, cohort_group) %>%
    summarise(group_ct = n()) %>%
    ungroup()

  otpt <- list('summary_values' = summ_tbl,
               'cohort_overlap' = cht_overlap)

  return(otpt)

}


#' Compute Standardized Mean Difference between cohort definitions
#'
#' @param cohort_def_output the output of compare_cohort_def
#'
#' @return a summarized dataframe where the standardized mean difference between each
#'         alternate cohort definition and the base definition is computed for each of
#'         the computed variables
#'
#' @import smd
#' @importFrom stringr str_remove
#'
compare_cohort_smd <- function(cohort_def_output){

  prep_tbl <- cohort_def_output %>%
    select(-c(start_date, end_date)) %>%
    mutate(across(where(is.logical), ~replace_na(.,FALSE)))

  if('person_id' %in% names(cohort_def_output)){person_col <- 'person_id'}else{person_col <- 'patid'}

  var_vec <- names(prep_tbl)[!names(prep_tbl) %in% c('site', person_col, 'cohort_id')]

  alt_cht_list <- prep_tbl %>% filter(grepl('alt', cohort_id)) %>%
    distinct(cohort_id) %>%
    pull()

  smd_list <- list()

  for(i in alt_cht_list){
    alt_exists_check <- prep_tbl %>%
      group_by(site) %>%
      filter(cohort_id %in% c('base_cohort', i)) %>%
      distinct(site, cohort_id) %>%
      summarise(n_cht = n()) %>%
      filter(n_cht < 2) %>% ungroup() %>% pull(site)

    smd_tbl <- prep_tbl %>%
      filter(!site %in% alt_exists_check) %>%
      group_by(site) %>%
      filter(cohort_id %in% c('base_cohort', i)) %>%
      summarize_at(
        .vars = vars(var_vec),
        .funs = list(smd = ~ smd(., g = cohort_id)$estimate)
      )

    smd_list[[i]] <- smd_tbl

  }

  smd_reduce <- bind_rows(smd_list, .id = 'cohort_id') %>%
    pivot_longer(cols = !c(site, cohort_id),
                 names_to = 'cohort_characteristic',
                 values_to = 'smd_vs_baseline') %>%
    #cross_join(distinct(cohort_def_output, site)) %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, '_smd')) %>%
    apply_cohort_labels()

}

#' Add labels to cohort variable groups
#'
#' @param df the output of compare_cohort_def including all variables computed
#'
#' @return returns the same input dataframe with an additional fact_group column
#'         that labels each of the computed variables for easier grouping
#'
apply_cohort_labels <- function(df){

  df %>%
    mutate(fact_group = case_when(grepl('fu|age', cohort_characteristic) ~ 'Cohort Details',
                                  grepl('visit', cohort_characteristic) ~ 'Utilization',
                                  cohort_characteristic %in% c('white', 'unknown_race', 'mixed_race', 'hispanic', 'female',
                                                               'black', 'asian', 'other_race') ~ 'Demographics',
                                  grepl('ppy', cohort_characteristic) ~ 'Clinical Facts',
                                  TRUE ~ 'Outcomes'))

}
