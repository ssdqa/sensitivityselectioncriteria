# Sensitivity to Selection Criteria – PCORnet

Sensitivity to Selection Criteria – PCORnet

## Usage

``` r
ssc_process_pcornet(
  base_cohort,
  alt_cohorts,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  person_tbl = cdm_tbl("demographic"),
  visit_tbl = cdm_tbl("encounter"),
  provider_tbl = cdm_tbl("provider"),
  care_site_tbl = cdm_tbl("encounter"),
  demographic_mappings = sensitivityselectioncriteria::ssc_pcornet_demographics,
  specialty_concepts = NULL,
  outcome_concepts = NULL,
  domain_tbl = sensitivityselectioncriteria::ssc_domain_file,
  domain_select = sensitivityselectioncriteria::ssc_domain_file %>% distinct(domain)
    %>% pull()
)
```

## Arguments

- base_cohort:

  a dataframe including all patients who meet the base cohort definition
  should have the columns site \| person_id \| start_date \| end_date

- alt_cohorts:

  a list or named list of dataframes with patients meeting alternative
  cohort definitions; if names are not provided, numbers will be
  assigned to each cohort definition for labelling purposes

- multi_or_single_site:

  direction to determine what kind of check to run string that is either
  `multi` or `single`

- anomaly_or_exploratory:

  direction to determine what kind of check to run; a string that is
  either `anomaly` or `exploratory`

- person_tbl:

  CDM `demographic` table

- visit_tbl:

  CDM `encounter` table

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

- domain_tbl:

  a table with domain definitions with the following columns: domain,
  domain_tbl, concept_field, date_field, vocabulary_field, filter_logic

- domain_select:

  a vector of domain names that should be used in the computation of
  facts per domain per year of follow up

                       this vector does NOT need to include domains used in the computation for
                       outcomes, as those will be accessed from the table directly

## Value

a dataframe summarizing fact and demographic distributions for each of
the provided cohort definitions; for both anomaly detection outputs, the
standardized mean difference between the base cohort and each alternate
cohort definition is computed for each of the variables of interest
