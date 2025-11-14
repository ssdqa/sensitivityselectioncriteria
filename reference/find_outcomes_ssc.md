# Find presence of outcomes in cohort definitions

Find presence of outcomes in cohort definitions

## Usage

``` r
find_outcomes_ssc(cohort, site_col, domain_tbl, outcome_concepts)
```

## Arguments

- cohort:

  the combined cohort with patients from the base and alternate cohort
  definitions, with a cohort_id label to differentiate

- site_col:

  the column in the cohort_tbl with the site name(s)

- domain_tbl:

  a table with domain definitions with the following columns: domain,
  domain_tbl, concept_field, date_field, filter_logic

- outcome_concepts:

  a concept set with the following columns: `concept_id` \|
  `concept_code` \| `concept_name` \| `vocabulary_id` \| `domain`

                          where domain matches a domain listed in the domain_tbl

## Value

a dataframe with indicators for each patient identifying whether or not
they have the outcome(s) of interest
