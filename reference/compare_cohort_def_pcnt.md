# Reformat cohort definition tables

Reformat cohort definition tables

## Usage

``` r
compare_cohort_def_pcnt(
  base_cohort,
  alt_cohorts,
  multi_or_single_site,
  person_tbl = cdm_tbl("demographic"),
  visit_tbl = cdm_tbl("encounter"),
  provider_tbl = NULL,
  care_site_tbl = NULL,
  demographic_mappings = sensitivityselectioncriteria::ssc_pcornet_demographics,
  specialty_concepts = NULL,
  outcome_concepts = NULL,
  domain_defs = sensitivityselectioncriteria::ssc_domain_file,
  domain_select = sensitivityselectioncriteria::ssc_domain_file %>% distinct(domain)
    %>% pull()
)
```

## Arguments

- base_cohort:

  table with the base cohort & inclusion criteria; must include site,
  person_id, start_date, end_date

- alt_cohorts:

  list of each table with an alternative inclusion criteria definition
  applied to the base_cohort

- multi_or_single_site:

  direction to determine what kind of check to run string that is either
  `multi` or `single`

- person_tbl:

  the CDM table that contains patient information

- visit_tbl:

  the CDM table that contains visit/encounter information

- provider_tbl:

  the CDM table with provider & provider specialty information only used
  if `specialty_concepts` are provided if provider_tbl & care_site_tbl
  are both not null, provider specialty is prioritized

- care_site_tbl:

  the CDM table with care site / facility & care site / facility
  specialty information only used if `specialty_concepts` are provided
  if provider_tbl & care_site_tbl are both not null, provider specialty
  is prioritized

- demographic_mappings:

  table defining how demographic elements should be defined if NULL, the
  default demographic mappings for the CDM will be used
  (`ssc_pcornet_demographics`) otherwise, the user provided table will
  be used

- specialty_concepts:

  a concept set with provider specialty concepts of interest to be used
  to identify specialty visits

- outcome_concepts:

  a concept set with the following columns: `concept_id` \|
  `concept_code` \| `concept_name` \| `vocabulary_id` \| `variable` \|
  `domain`

                          where domain matches a domain listed in the `domain_defs` table

- domain_defs:

  a table with domain definitions with the following columns: domain,
  domain_tbl, concept_field, date_field, filter_logic

- domain_select:

  a vector of domain names that should be used in the computation of
  facts per domain per year of follow up

                       this vector does NOT need to include domains used in the computation for
                       outcomes, as those will be accessed from the table directly

## Value

one combined dataframe with one row per patient in the original cohort
with flags to show which cohort definitions apply to the patient.

        flag columns will include `base_cohort` and `alt_cohort_#` columns with numbers
        appended that correspond to the tables position in the `def_tbls` list

        `base_cohort` should equal 1 for all patients
