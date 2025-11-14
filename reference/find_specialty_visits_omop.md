# Compute specialty visits per person year

Compute specialty visits per person year

## Usage

``` r
find_specialty_visits_omop(
  cohort,
  site_col,
  specialty_concepts,
  grouped_list,
  provider_tbl = NULL,
  care_site_tbl = NULL,
  visit_tbl = cdm_tbl("visit_occurrence")
)
```

## Arguments

- cohort:

  the combined cohort with patients from the base and alternate cohort
  definitions, with a cohort_id label to differentiate

- site_col:

  the column in the cohort_tbl with the site name(s)

- specialty_concepts:

  a concept set with provider specialty concepts of interest to be used
  to identify specialty visits

- grouped_list:

  a list of variables for grouping

- provider_tbl:

  the CDM table with provider & provider specialty information only used
  if `specialty_concepts` are provided if provider_tbl & care_site_tbl
  are both not null, provider specialty is prioritized

- care_site_tbl:

  the CDM table with care site / facility & care site / facility
  specialty information only used if `specialty_concepts` are provided
  if provider_tbl & care_site_tbl are both not null, provider specialty
  is prioritized

- visit_tbl:

  The CDM table with visit/encounter information

## Value

a dataframe with the number of specialist visits per year of follow up
for each of the patients in the cohort
