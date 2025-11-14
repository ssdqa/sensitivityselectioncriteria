# Compute patient level fact summary

Compute patient level fact summary

## Usage

``` r
compute_demographic_summary_omop(
  cohort_tbl,
  site_col,
  person_tbl = cdm_tbl("person"),
  visit_tbl = cdm_tbl("visit_occurrence"),
  demographic_mappings = sensitivityselectioncriteria::ssc_omop_demographics
)
```

## Arguments

- cohort_tbl:

  table with the original cohort & inclustion criteria; must include
  site, person_id, start_date, end_date

- site_col:

  the column in the cohort_tbl with the site name(s)

- person_tbl:

  CDM `person` table

- visit_tbl:

  CDM `visit_occurrence` table

- demographic_mappings:

  table defining how demographic elements should be defined; defaults to
  `ssc_omop_demographics`; any additional demographic elements must be
  found in the person_tbl

## Value

one dataframe with one row for each patient with columns to show which
facts apply to each patient:

        fu, age_cohort_entry, age_first_visit, and each of the demographic
        elements found in demographic_mappings
