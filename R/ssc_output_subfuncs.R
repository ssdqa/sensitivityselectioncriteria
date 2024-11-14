
#'
#' @import ggplot2
#' @import gt
#' @import UpSetR
#' @import ggiraph
#' @import ggbump
#' @importFrom stats quantile
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom tibble deframe
NULL


#' *Single Site, Exploratory, No Time*
#'
#' @param summary_output the summary dataframe output by ssc_process
#' @param cohort_overlap the cohort overlap dataframe output by ssc_process
#' @param alt_cohort_filter an vector indicating which alternate cohorts should be
#'                          displayed; only 3 are allowed at once to maintain visibility
#'                          in the graph
#'
#' @return an UpSet graph illustrating the overlap in patients between the two cohorts
#'         a bar graph showing the difference between each alternate cohort and the base cohort
#'
ssc_ss_exp_nt <- function(summary_output,
                          cohort_overlap,
                          alt_cohort_filter){

  if(length(alt_cohort_filter) > 3){
    cli::cli_abort('Please limit alternate cohort definitions to three or less')}

  upset_prep <- cohort_overlap %>%
    ungroup() %>%
    mutate(cohort_group = str_remove_all(cohort_group, 'alt_cohort_')) %>%
    select(cohort_group, group_ct) %>%
    deframe()

  ug <- upset(fromExpression(upset_prep),
              empty.intersections = TRUE,
              main.bar.color = ssdqa_colors_standard[8],
              sets.bar.color = ssdqa_colors_standard[6],
              text.scale = 1.7,
              matrix.color = ssdqa_colors_standard[6])


  bc_cts <- summary_output %>%
    filter(cohort_id == 'base_cohort') %>%
    pivot_wider(names_from = cohort_id,
                values_from = fact_summary) %>%
    select(-cohort_total_pt)

  alt_cts <- summary_output %>%
    filter(cohort_id %in% alt_cohort_filter) %>%
    left_join(bc_cts) %>%
    mutate(diff_base = fact_summary - base_cohort,
           val_type = case_when(grepl('prop', cohort_characteristic) ~ 'Proportions',
                                grepl('median', cohort_characteristic) ~ 'Medians')) %>%
    filter(!is.na(val_type)) %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'prop_|median_'))


  dat_to_plot <- alt_cts %>%
    mutate(cohort_characteristic = case_when(cohort_characteristic == 'fu' ~ 'follow-up',
                                             TRUE ~ cohort_characteristic),
           cohort_characteristic = str_remove(cohort_characteristic, 'median_|prop_'),
           cohort_characteristic = str_remove(cohort_characteristic, '_ppy'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_id = str_remove(cohort_id, 'alt_cohort_')) %>%
    arrange(fact_group) %>% mutate(cf = factor(cohort_characteristic, levels = unique(cohort_characteristic)))


  bg <- ggplot(dat_to_plot,
               aes(x = diff_base, y = cf, fill = cohort_id)) +
    geom_col(position = 'dodge') +
    geom_label(aes(label = round(diff_base, 2)), position = position_dodge(width = 0.9),
               size = 3) +
    facet_wrap(~val_type, scales = 'free') +
    theme_minimal() +
    scale_fill_ssdqa() +
    labs(x = 'Difference from Base Cohort',
         y = 'Cohort Characteristic',
         fill = 'Alternate Cohort',
         title = 'Difference between Alternate and Base Cohorts')

  bg[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                             'tooltip' = FALSE)

  otpt <- list(ug, bg)

  return(otpt)


}



#' *Single Site, Anomaly Detection, No Time*
#'
#' @param process_output the output from ssc_process
#'
#' @return a heat map displaying the standardized mean difference between
#'         the base cohort and each of the alternate cohorts
#'
ssc_ss_anom_nt <- function(process_output){


  dat_to_plot <- process_output %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'median_|prop_'),
           cohort_characteristic = str_remove(cohort_characteristic, '_ppy'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_characteristic = case_when(cohort_characteristic == 'fu' ~ 'follow-up',
                                             TRUE ~ cohort_characteristic),
           cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = paste0('Alternate Cohort: \n', cohort_id))

  grph <- ggplot(dat_to_plot, aes(x = cohort_id, y = cohort_characteristic,
                             fill = smd_vs_baseline)) +
    geom_tile() +
    geom_text(aes(label = round(smd_vs_baseline, 2))) +
    facet_grid(rows = vars(fact_group),
               scales = 'free_y',switch = 'y', space = 'free_y',
               labeller = label_wrap_gen()) +
    theme_minimal() +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_y_discrete(expand = c(0,0)) +
    theme(panel.spacing = unit(0, 'lines'),
          strip.background = element_rect(),
          strip.placement = "outside") +
    labs(title = 'Standardized Mean Difference versus Base Cohort',
         x = '',
         y = 'Cohort Characteristic',
         fill = 'SMD')

  grph[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                               'tooltip' = FALSE)

  return(grph)


}



#' *Multi-Site, Exploratory, No Time*
#'
#' @param process_output the output from ssc_process
#' @param alt_cohort_filter an vector indicating which alternate cohorts should be
#'                          displayed; only 2 are allowed at once to maintain visibility
#'                          in the graph
#'
#' @return two bump charts - one with the continuous facts (that are represented by medians) and
#' another with the categorical facts (that are represented by proportions)
#' if two alternate cohort definitions are provided, the base cohort is placed in the middle
#' of the plot. Otherwise, the base is on the left.
#'
ssc_ms_exp_nt <- function(process_output,
                          alt_cohort_filter){

  if(length(alt_cohort_filter) > 2){
    cli::cli_abort('Please limit alternate cohort definitions to two or less at a time')}

  # Categorical variables (proportions)
  cat_vars <- process_output %>% filter(grepl('prop', cohort_characteristic),
                                        cohort_id %in% c('base_cohort', alt_cohort_filter)) %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'prop_'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = case_when(cohort_id == 'base_cohort' ~ 'Base Cohort',
                                 cohort_id != 'base_cohort' ~ paste0('Alternate Cohort: \n', cohort_id)),
           tooltip = paste0('Site: ', site,
                            '\nCohort: ', cohort_id,
                            '\nCharacteristic: ', cohort_characteristic,
                            '\nProportion: ', round(fact_summary, 3))) %>%
    arrange(fact_group) %>%
    mutate(cf = factor(cohort_characteristic, levels = unique(cohort_characteristic)))

  # Continuous variables (medians)
  cont_vars <- process_output %>% filter(grepl('median', cohort_characteristic),
                                         cohort_id %in% c('base_cohort', alt_cohort_filter)) %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'median_'),
           cohort_characteristic = str_remove(cohort_characteristic, '_ppy'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_characteristic = case_when(cohort_characteristic == 'fu' ~ 'follow-up',
                                             TRUE ~ cohort_characteristic),
           cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = case_when(cohort_id == 'base_cohort' ~ 'Base Cohort',
                                 cohort_id != 'base_cohort' ~ paste0('Alternate Cohort: \n', cohort_id)),
           tooltip = paste0('Site: ', site,
                            '\nCohort: ', cohort_id,
                            '\nCharacteristic: ', cohort_characteristic,
                            '\nMedian PPY: ', fact_summary)) %>%
    arrange(fact_group) %>%
    mutate(cf = factor(cohort_characteristic, levels = unique(cohort_characteristic)))

  # Build vectors based on number of alternate cohorts
  if(length(alt_cohort_filter) == 2){
    shape_vec <- c(15, 19, 17)
    cohort_vec <- cont_vars %>% ungroup() %>% distinct(cohort_id) %>% pull()
    cat_vars <- cat_vars %>% mutate(cohort_id = factor(cohort_id, levels = c(cohort_vec[[1]], cohort_vec[[3]],
                                                                             cohort_vec[[2]])))
    cont_vars <- cont_vars %>% mutate(cohort_id = factor(cohort_id, levels = c(cohort_vec[[1]], cohort_vec[[3]],
                                                                               cohort_vec[[2]])))
  }else if(length(alt_cohort_filter == 1)){
    shape_vec <- c(19, 17)
    cohort_vec <- cont_vars %>% ungroup() %>% distinct(cohort_id) %>% pull()
    cat_vars <- cat_vars %>% mutate(cohort_id = factor(cohort_id,
                                                       levels = c(cohort_vec[[2]], cohort_vec[[1]])))
    cont_vars <- cont_vars %>% mutate(cohort_id = factor(cohort_id,
                                                         levels = c(cohort_vec[[2]], cohort_vec[[1]])))
  }else{cli::cli_abort('Please choose at least one alternate cohort definition')}

  # Plot bump graphs
  cont_bp <- cont_vars %>%
    ggplot(aes(x = cohort_id, y = fact_summary, color = site, group = site, text = tooltip)) +
    geom_bump() +
    geom_point(size = 2, aes(shape = cohort_id)) +
    scale_shape_manual(values = shape_vec) +
    facet_wrap(~cf, scales = 'free_y',
               labeller = label_wrap_gen(width = 15)) +
    scale_color_ssdqa() +
    theme_minimal() +
    theme(strip.background = element_rect(),
          panel.border = element_rect(fill = NA)) +
    labs(y = 'Median (PPY)',
         x = 'Cohort',
         color = 'Site',
         shape = 'Cohort',
         title = 'Median Facts PPY per Site')

  # if(length(alt_cohort_filter == 2)){cont_bp <- cont_bp + geom_vline(xintercept = 1.98, linetype = 'dotted') +
  #   geom_vline(xintercept = 2.02, linetype = 'dotted')}

  cont_bp[["metadata"]] <- tibble('pkg_backend' = 'plotly_ssc',
                                  'tooltip' = TRUE)

  cat_bp <- cat_vars %>%
    ggplot(aes(x = cohort_id, y = fact_summary, color = site, group = site, text = tooltip)) +
    geom_bump() +
    geom_point(size = 2, aes(shape = cohort_id)) +
    scale_shape_manual(values = shape_vec) +
    facet_wrap(~cf, scales = 'free_y',
               labeller = label_wrap_gen(width = 15)) +
    scale_color_ssdqa() +
    theme_minimal() +
    theme(strip.background = element_rect(),
          panel.border = element_rect(fill = NA)) +
    labs(y = 'Proportion',
         x = 'Cohort',
         color = 'Site',
         shape = 'Cohort',
         title = 'Proportion of Patients per Site')

  # if(length(alt_cohort_filter == 2)){cat_bp <- cat_bp + geom_vline(xintercept = 1.98, linetype = 'dotted') +
  #   geom_vline(xintercept = 2.02, linetype = 'dotted') }

  cat_bp[["metadata"]] <- tibble('pkg_backend' = 'plotly_ssc',
                                 'tooltip' = TRUE)

  otpt <- list(cont_bp, cat_bp)

  return(otpt)

}



#' *Multi-Site, Anomaly Detection, No Time*
#'
#' @param process_output the output of ssc_process
#'
#' @return a table displaying the mean, median, and iqr standardized mean difference
#'           values for each site & alternative cohort definition vs baseline
#'         a dot plot showing the standardized mean difference value vs baseline for
#'           each site & alternative cohort definition
#'
ssc_ms_anom_nt <- function(process_output){

  tbl_sum <- process_output %>%
    group_by(cohort_id, site) %>%
    summarise(median_smd = median(smd_vs_baseline),
              mean_smd = mean(smd_vs_baseline),
              q1_smd = quantile(smd_vs_baseline)[[2]],
              q3_smd = quantile(smd_vs_baseline)[[4]]) %>%
    mutate(cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = paste0('Alternate Cohort: ', cohort_id)) %>%
    pivot_longer(cols = !c(cohort_id, site)) %>%
    pivot_wider(names_from = c(cohort_id, name),
                values_from = value,
                names_sep = '-') %>%
    ungroup() %>%
    gt::gt(rowname_col = 'site') %>%
    tab_spanner_delim(delim = '-') %>%
    cols_label(contains('mean') ~ 'Mean',
               contains('median') ~ 'Median',
               contains('q1') ~ 'Q1',
               contains('q3') ~ 'Q3') %>%
    fmt_number(decimals = 2) %>%
    opt_stylize(style = 2) %>%
    tab_style(
      style = cell_borders(
        sides = c("right"),
        weight = px(3),
        color = 'black'),
      locations = cells_body(
        columns = contains('q3')
      )) %>%
    tab_header('Site SMD Summary Table') %>%
    data_color(columns = contains('q1'),
               palette = "Blues",
               method = 'numeric') %>%
    data_color(columns = contains('median'),
               palette = 'Greens',
               method = 'numeric') %>%
    data_color(columns = contains('mean'),
               palette = 'Oranges',
               method = 'numeric') %>%
    data_color(columns = contains('q3'),
               palette = 'Purples',
               method = 'numeric')

  dat_to_plot <- process_output %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'median_|prop_'),
           cohort_characteristic = str_remove(cohort_characteristic, '_ppy'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_characteristic = case_when(cohort_characteristic == 'fu' ~ 'follow-up',
                                             TRUE ~ cohort_characteristic),
           cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = paste0('Alternate Cohort: ', cohort_id),
           #val_type = ifelse(fact_group %in% c('Demographics', 'Outcomes'), 'Proportion', 'Median PPY'),
           tooltip = paste0('Site: ', site,
                            '\nCharacteristic: ', cohort_characteristic,
                            '\nSMD (vs. Base): ', smd_vs_baseline)) %>%
    arrange(fact_group) %>%
    mutate(cf = factor(cohort_characteristic, levels = unique(cohort_characteristic)))

  grph <- ggplot(dat_to_plot, aes(y = cohort_characteristic, x = smd_vs_baseline, color = site)) +
    geom_point() +
    #facet_wrap(~cohort_id) +
    facet_grid(cols = vars(cohort_id), rows = vars(fact_group),
               scales = 'free_y',
               labeller = label_wrap_gen()) +
    geom_vline(xintercept = 0, linetype = 'dotted', alpha = 0.5) +
    theme_minimal() +
    scale_color_ssdqa() +
    scale_y_discrete(expand = c(0.1,0.1)) +
    theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          panel.spacing = unit(0, 'lines'),
          strip.background = element_rect(),
          strip.placement = "outside") +
    labs(title = 'SMD of Alternate Cohorts compared to Base',
         x = 'SMD',
         y = 'Cohort Characteristic',
         color = 'Site')

  grph[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                               'tooltip' = FALSE)

  opt <- list(tbl_sum,
              grph)

  return(opt)

}
