# *Single Site, Exploratory, Cross-Sectional*

*Single Site, Exploratory, Cross-Sectional*

## Usage

``` r
ssc_ss_exp_cs(summary_output, cohort_overlap, alt_cohort_filter)
```

## Arguments

- summary_output:

  the summary dataframe output by ssc_process

- cohort_overlap:

  the cohort overlap dataframe output by ssc_process

- alt_cohort_filter:

  an vector indicating which alternate cohorts should be displayed; only
  3 are allowed at once to maintain visibility in the graph

## Value

an UpSet graph illustrating the overlap in patients between the two
cohorts a bar graph showing the difference between each alternate cohort
and the base cohort
