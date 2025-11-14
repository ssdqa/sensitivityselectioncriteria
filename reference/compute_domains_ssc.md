# Compute domain facts per person year

Compute domain facts per person year

## Usage

``` r
compute_domains_ssc(cohort, site_col, grouped_list, domain_tbl)
```

## Arguments

- cohort:

  the combined cohort with patients from the base and alternate cohort
  definitions, with a cohort_id label to differentiate

- site_col:

  the column in cohort with the site name(s)

- grouped_list:

  a list of variables for grouping

- domain_tbl:

  a table with domain definitions that contains the following columns:
  domain, domain_tbl, concept_field, date_field, filter_logic

## Value

a dataframe with fact counts of the user-provided domains per year of
follow-up within the cohort period for each of the patients in the
cohort
