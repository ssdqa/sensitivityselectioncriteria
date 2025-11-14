# Compute Standardized Mean Difference between cohort definitions

Compute Standardized Mean Difference between cohort definitions

## Usage

``` r
compare_cohort_smd(cohort_def_output, demographic_vector)
```

## Arguments

- cohort_def_output:

  the output of compare_cohort_def

- demographic_vector:

  a vector of demographic categories provided in demographic_mappings;
  ensures all demographics are captured under the appropriate label

## Value

a summarized dataframe where the standardized mean difference between
each alternate cohort definition and the base definition is computed for
each of the computed variables
