

#' Sensitivity to Selection Criteria Output
#'
#' @param process_output the output from `ssc_process`
#' @param output_function the name of the output function to be executed; this is provided in the message
#'                        printed to the console after `scv_process` is finished running
#' @param alt_cohort_filter single site exploratory no time only -- a vector with the names of
#'                          alternate cohorts to display on the graph; this should be limited
#'                          to 3 or less to maintain good visibility on the graph
#'
#' @return a graph visualizing the differences between the alternate cohort definitions and
#'         the base cohort; see documentation for each output function for specifics
#'
#' @export
#'
ssc_output <- function(process_output,
                       output_function,
                       alt_cohort_filter){

  if(output_function == 'ssc_ss_exp_nt'){

    ssc_output <- ssc_ss_exp_nt(summary_output = process_output[[1]],
                                cohort_overlap = process_output[[2]],
                                alt_cohort_filter = alt_cohort_filter)

  }else if(output_function == 'ssc_ss_anom_nt'){

    ssc_output <- ssc_ss_anom_nt(process_output = process_output)

  }else if(output_function == 'ssc_ms_exp_nt'){

    ssc_output <- ssc_ms_exp_nt(process_output = process_output[[1]])

  }else if(output_function == 'ssc_ms_anom_nt'){

    ssc_output <- ssc_ms_anom_nt(process_output = process_output)

  }else{cli::cli_abort('Please enter a valid output_function for this check')}


}
