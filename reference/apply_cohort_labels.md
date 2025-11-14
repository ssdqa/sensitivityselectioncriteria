# Add labels to cohort variable groups

Add labels to cohort variable groups

## Usage

``` r
apply_cohort_labels(df, demographic_vector)
```

## Arguments

- df:

  the output of compare_cohort_def including all variables computed

- demographic_vector:

  a vector of demographic categories provided in demographic_mappings;
  ensures all demographics are captured under the appropriate label

## Value

returns the same input dataframe with an additional fact_group column
that labels each of the computed variables for easier grouping
