# *Multi-Site, Exploratory, Cross-Sectional*

*Multi-Site, Exploratory, Cross-Sectional*

## Usage

``` r
ssc_ms_exp_cs(
  process_output,
  alt_cohort_filter,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  the output from ssc_process

- alt_cohort_filter:

  an vector indicating which alternate cohorts should be displayed; only
  2 are allowed at once to maintain visibility in the graph

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally be compared against summary
  statistics

## Value

two bump charts - one with the continuous facts (that are represented by
medians) and another with the categorical facts (that are represented by
proportions) if two alternate cohort definitions are provided, the base
cohort is placed in the middle of the plot. Otherwise, the base is on
the left.
