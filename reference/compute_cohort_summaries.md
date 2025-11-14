# Compute definition level summary

Compute definition level summary

## Usage

``` r
compute_cohort_summaries(cohort_def_output, demographic_vector)
```

## Arguments

- cohort_def_output:

  the output of compare_cohort_def

- demographic_vector:

  a vector of demographic categories provided in demographic_mappings;
  ensures all demographics are captured under the appropriate label

## Value

a dataframe with summary output for each cohort definition. for
demographics and outcomes, proportions are computed. for other
variables, median fact counts ppy are computed
