
test_that('errors on incorrect output_function', {

  tbl_test <- data.frame('test'= c(1, 2, 3))

  expect_error(ssc_output(process_output = tbl_test,
                          output_function = 'ssc_test'))
})


test_that('single site, exploratory, no time', {

  tbl_test1 <- dplyr::tibble(site = c('a', 'a', 'a', 'a', 'a', 'a'),
                             cohort_id = c('base_cohort', 'base_cohort', 'alt_cohort_1',
                                           'alt_cohort_1','alt_cohort_2','alt_cohort_2'),
                             cohort_characteristic = c('median_inpatient_visits', 'prop_black',
                                                       'median_inpatient_visits', 'prop_black',
                                                       'median_inpatient_visits', 'prop_black'),
                             fact_group = c('Utilization', 'Demographics', 'Utilization', 'Demographics',
                                            'Utilization', 'Demographics'),
                             fact_summary = c(50, 0.5, 20, 0.3, 15, 0.1),
                             cohort_total_pt = c(100,100,75,75,50,50),
                             output_function = c('ssc_ss_exp_cs','ssc_ss_exp_cs','ssc_ss_exp_cs',
                                                 'ssc_ss_exp_cs','ssc_ss_exp_cs','ssc_ss_exp_cs'))

  tbl_test2 <- dplyr::tibble(site = c('a', 'a', 'a'),
                             cohort_group = c('base_cohort', 'base_cohort&alt_cohort_1',
                                              'base_cohort&alt_cohort_2'),
                             group_ct = c(100, 50, 20))

  tbl_test <- list(tbl_test1, tbl_test2)

  expect_error(ssc_output(process_output = tbl_test,
                          alt_cohort_filter = NULL))

  expect_no_error(ssc_output(process_output = tbl_test,
                             alt_cohort_filter = c('alt_cohort_1','alt_cohort_2')))

})


test_that('multi site, exploratory, no time', {

  tbl_test1 <- dplyr::tibble(site = c('a', 'a', 'a', 'b', 'b', 'b'),
                             cohort_id = c('base_cohort', 'base_cohort', 'alt_cohort_1',
                                           'alt_cohort_1','alt_cohort_2','alt_cohort_2'),
                             cohort_characteristic = c('median_inpatient_visits', 'prop_black',
                                                       'median_inpatient_visits', 'prop_black',
                                                       'median_inpatient_visits', 'prop_black'),
                             fact_group = c('Utilization', 'Demographics', 'Utilization', 'Demographics',
                                            'Utilization', 'Demographics'),
                             fact_summary = c(50, 0.5, 20, 0.3, 15, 0.1),
                             cohort_total_pt = c(100,100,75,75,50,50),
                             output_function = c('ssc_ms_exp_cs','ssc_ms_exp_cs','ssc_ms_exp_cs',
                                                 'ssc_ms_exp_cs','ssc_ms_exp_cs','ssc_ms_exp_cs'))

  tbl_test2 <- dplyr::tibble(site = c('a', 'a', 'a'),
                             cohort_group = c('base_cohort', 'base_cohort&alt_cohort_1',
                                              'base_cohort&alt_cohort_2'),
                             group_ct = c(100, 50, 20))

  tbl_test <- list(tbl_test1, tbl_test2)

  expect_error(ssc_output(process_output = tbl_test,
                          alt_cohort_filter = NULL))

  expect_no_error(ssc_output(process_output = tbl_test,
                             alt_cohort_filter = c('alt_cohort_1','alt_cohort_2')))

})


test_that('single site, anomaly, no time', {

  tbl_test <- dplyr::tibble(site = c('a', 'a', 'a', 'a', 'a', 'a'),
                            cohort_id = c('alt_cohort_1', 'alt_cohort_2', 'alt_cohort_1',
                                          'alt_cohort_1','alt_cohort_2','alt_cohort_2'),
                            cohort_characteristic = c('median_inpatient_visits', 'median_inpatient_visits',
                                                      'prop_black', 'median_diagnoses',
                                                      'median_diagnoses', 'prop_black'),
                            fact_group = c('Utilization', 'Demographics', 'Utilization', 'Demographics',
                                           'Utilization', 'Demographics'),
                            smd_vs_baseline = c(0.1, 0.2, -0.1, 0.05, -0.2, 0.01),
                            output_function = c('ssc_ss_anom_cs','ssc_ss_anom_cs','ssc_ss_anom_cs',
                                                'ssc_ss_anom_cs','ssc_ss_anom_cs','ssc_ss_anom_cs'))

  expect_no_error(ssc_output(process_output = tbl_test))

})

test_that('multi site, anomaly, no time', {

  tbl_test <- dplyr::tibble(site = c('a', 'a', 'a', 'b', 'b', 'b'),
                            cohort_id = c('alt_cohort_1', 'alt_cohort_2', 'alt_cohort_1',
                                          'alt_cohort_1','alt_cohort_2','alt_cohort_2'),
                            cohort_characteristic = c('median_inpatient_visits', 'median_inpatient_visits',
                                                      'prop_black', 'median_diagnoses',
                                                      'median_diagnoses', 'prop_black'),
                            fact_group = c('Utilization', 'Demographics', 'Utilization', 'Demographics',
                                           'Utilization', 'Demographics'),
                            smd_vs_baseline = c(0.1, 0.2, -0.1, 0.05, -0.2, 0.01),
                            output_function = c('ssc_ms_anom_cs','ssc_ms_anom_cs','ssc_ms_anom_cs',
                                                'ssc_ms_anom_cs','ssc_ms_anom_cs','ssc_ms_anom_cs'))

  expect_no_error(ssc_output(process_output = tbl_test))

})
