# Sensitivity to Selection Criteria

This is a plausibility module that will characterize user-provided base
cohort and alternate cohort definitions using a selection of continuous
and categorical variables. It will then use these values to compare each
alternate definition to the base cohort and evaluate how changes in the
cohort definition impact the patient population. Users have the option
to provide definitions for several variable types:

- `demographic_mappings`: define a set of demographic variables of
  interest

- `domain_tbl`: define domain definitions to assess patient fact
  density + utilization (similar to Patient Facts module)

- `specialty_concepts`: define a set of specialty concepts to evaluate
  specialty care sought out by the cohort members

- `outcome_concepts`: define study outcome variables Sample versions of
  these input files are available with `sensitivityselectioncriteria::`.
  This module is compatible with both the OMOP and PCORnet CDMs based on
  the user's selection.

## Usage

``` r
ssc_process(
  base_cohort,
  alt_cohorts,
  omop_or_pcornet,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  person_tbl = cdm_tbl("person"),
  visit_tbl = cdm_tbl("visit_occurrence"),
  provider_tbl = NULL,
  care_site_tbl = NULL,
  demographic_mappings = NULL,
  specialty_concepts = NULL,
  outcome_concepts = NULL,
  domain_tbl = sensitivityselectioncriteria::ssc_domain_file,
  domain_select = sensitivityselectioncriteria::ssc_domain_file %>% distinct(domain)
    %>% pull()
)
```

## Arguments

- base_cohort:

  *tabular input* \|\| **required**

  A table representing all patients who meet the base cohort definition,
  used as the "gold standard" for comparative analyses. This table
  should have at least:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer* / *character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

  Note that the start and end dates included in this table will be used
  to limit the search window for the analyses in this module.

- alt_cohorts:

  *tablular input or list of tabular inputs* \|\| **required**

  A table or list of tables (can be named or unnamed) representing
  patients meeting alternative cohort definitions, which will be
  assessed against the base cohort. These tables should have the same
  structure as the `base_cohort.`

  If an unnamed list is provided, numbers will be assigned to each
  cohort definition for labeling purposes. For named lists, the names
  will be used to label the alternate cohorts.

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

  - `omop`: run the
    [`ssc_process_omop()`](https://ssdqa.github.io/sensitivityselectioncriteria/reference/ssc_process_omop.md)
    function against an OMOP CDM instance

  - `pcornet`: run the
    [`ssc_process_pcornet()`](https://ssdqa.github.io/sensitivityselectioncriteria/reference/ssc_process_pcornet.md)
    function against a PCORnet CDM instance

- multi_or_single_site:

  *string* \|\| defaults to `single`

  A string, either `single` or `multi`, indicating whether a single-site
  or multi-site analysis should be executed

- anomaly_or_exploratory:

  *string* \|\| defaults to `exploratory`

  A string, either `anomaly` or `exploratory`, indicating what type of
  results should be produced.

  Exploratory analyses give a high level summary of the data to examine
  the fact representation within the cohort. Anomaly detection analyses
  are specialized to identify outliers within the cohort.

- person_tbl:

  *tabular input* \|\| defaults to `cdm_tbl('person')`

  The CDM `person` or `demographic` table that contains basic
  demographic information (sex, race, etc.) about the cohort members

- visit_tbl:

  *tabular input* \|\| defaults to `cdm_tbl('visit_occurrence')`

  The CDM `visit_occurrence`, `visit_detail`, or `encounter` table that
  contains information about the patients' healthcare encounters

- provider_tbl:

  *tabular input* \|\| defaults to `NULL`

  The CDM table with provider & provider specialty information. This
  table is only used if a `specialty_concepts` concept set is provided.

- care_site_tbl:

  *tabular input* \|\| defaults to `NULL`

  The CDM table with care site/facility & related specialty information.
  This table is only used if a `specialty_concepts` concept set is
  provided.

- demographic_mappings:

  *tabular input* \|\| defaults to `NULL`

  A table defining how demographic elements should be defined. If left
  as NULL, the default demographic mappings for the CDM will be used,
  which can be viewed in `ssc_omop_demographics` and
  `ssc_pcornet_demographics`. If a different table is provided by the
  user, those definition will be used instead. This table should
  minimally contain:

  - `demographic` \| *character* \| the label for the demographic
    category (ex: female, asian_race)

  - `concept_field` \| *character* \| the field in the CDM where this
    information is stored

  - `field_values` \| *character* \| the concept or concepts that are
    used to define the demographic category

- specialty_concepts:

  *tabular input* \|\| defaults to `NULL`

  A concept set with concepts representing the specialties of interest
  to be identified for the cohort. This table should minimally contain
  either the `concept_id` field (OMOP) or the `concept_code` field
  (PCORnet)

- outcome_concepts:

  *tabular input* \|\| defaults to `NULL`

  A concept set used to define any relevant outcome variables to be
  assessed in each cohort. This input should contain one of following:

  - `concept_id` \| *integer* \| the concept_id of interest (required
    for OMOP)

  - `concept_code` \| *character* \| the code of interest (required for
    PCORnet)

  And both of:

  - `variable` \| *character* \| a string label grouping one concept
    code into a larger variable definition

  - `domain` \| *character* \| the name of the CDM table where the
    concept can be found

  For certain PCORnet applications, it should also contain

  - `vocabulary_id` \| *character* \| the vocabulary of the code, which
    should match what is listed in the domain table's `vocabulary_field`

  To see an example of this file structure, see
  [`?sensitivityselectioncriteria::ssc_outcome_file`](https://ssdqa.github.io/sensitivityselectioncriteria/reference/ssc_outcome_file.md)

- domain_tbl:

  *tabular input* \|\| defaults to the internal `ssc_domain_file`

  A table or CSV file defining the domains to be assessed both for facts
  per patient year computations & for any outcome variables. This input
  should contain:

  - `domain` \| *character* \| a string label for the domain

  - `domain_tbl` \| *character* \| the name of the CDM table where this
    domain is defined

  - `concept_field` \| *character*\| the string name of the field in the
    domain table where the concepts are located

  - `date_field` \| *character* \| the name of the field in the domain
    table with the date that should be used for temporal filtering

  - `vocabulary_field` \| *character* \| for PCORnet applications, the
    name of the field in the domain table with a vocabulary identifier
    to differentiate concepts from one another (ex: dx_type); can be set
    to NA for OMOP applications

  - `filter_logic` \| *character* \| logic to be applied to the
    domain_tbl in order to achieve the definition of interest; should be
    written as if you were applying it in a
    [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
    command in R

  To see an example of this file structure, see
  [`?sensitivityselectioncriteria::ssc_domain_file`](https://ssdqa.github.io/sensitivityselectioncriteria/reference/ssc_domain_file.md)

- domain_select:

  *string or vector* \|\| defaults to all internal `ssc_domain_file`
  domains

  A string or vector of domain names, matching those listed in
  `domain_tbl`, that should be used in the computation of facts per
  patient year

  This vector does NOT need to include domains used in the computation
  for outcomes, as those will be accessed from the `domain_tbl`
  separately

## Value

This function will return a dataframe summarizing fact and demographic
distributions for each of the provided cohort definitions. For a more
detailed description of output specific to each check type, see the
PEDSpace metadata repository

## Examples

``` r
#' Source setup file
source(system.file('setup.R', package = 'sensitivityselectioncriteria'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'ssc_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)
#> Connected to: :memory:@NA
#> To see environment settings, run `get_argos_default()`

#' Build mock base study cohort
base_cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000),
                #RSQLite does not store date objects,
                #hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Build mock alternate study cohort
alt_cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  head(100) %>%
  dplyr::mutate(start_date = as.Date(-5000),
                #RSQLite does not store date objects,
                #hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Prepare input tables
ssc_domain_tbl <- dplyr::tibble(domain = c('all conditions', 'outpatient visits'),
                                domain_tbl = c('condition_occurrence', 'visit_occurrence'),
                                concept_field = c('condition_concept_id', 'visit_concept_id'),
                                date_field = c('condition_start_date', 'visit_start_date'),
                                vocabulary_field = c(NA, NA),
                                filter_logic = c(NA, 'visit_concept_id == 9202'))

ssc_outcome_tbl <- read_codeset('dx_hypertension') %>%
  dplyr::mutate(variable = 'hypertension', domain = 'all conditions')

#' Execute `ssc_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
ssc_process_example <- ssc_process(base_cohort = base_cohort,
                                   alt_cohorts = list('Sample Alternate' = alt_cohort),
                                   omop_or_pcornet = 'omop',
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   domain_tbl = ssc_domain_tbl,
                                   domain_select = c('all conditions', 'outpatient visits'),
                                   outcome_concepts = ssc_outcome_tbl) %>%
  suppressMessages()
#> <SQL>
#> SELECT
#>   `person_id`,
#>   MAX(CASE WHEN (`domain` = 'all conditions') THEN `fact_ppy` END) AS `all conditions_ppy`
#> FROM (
#>   SELECT
#>     `person_id`,
#>     `domain`,
#>     CASE WHEN (`fu` != 0.0) THEN (ROUND(`total_fact_ct` / (`fu` * `k_mult`), 2)) WHEN NOT (`fu` != 0.0) THEN 0.0 END AS `fact_ppy`
#>   FROM (
#>     SELECT
#>       `q01`.*,
#>       CASE
#> WHEN (`fu` < 0.1) THEN 100.0
#> WHEN (`fu` >= 0.1 AND `fu` < 1.0) THEN 10.0
#> ELSE 1.0
#> END AS `k_mult`
#>     FROM (
#>       SELECT `q01`.*, 'all conditions' AS `domain`
#>       FROM (
#>         SELECT
#>           `site_summ`,
#>           `person_id`,
#>           `start_date`,
#>           `end_date`,
#>           `fu`,
#>           `cohort_id`,
#>           COUNT(*) AS `total_fact_ct`
#>         FROM (
#>           SELECT `q01`.*
#>           FROM (
#>             SELECT
#>               `condition_occurrence`.*,
#>               `start_date`,
#>               `end_date`,
#>               `site`,
#>               `cohort_id`,
#>               `site_summ`,
#>               `fu_diff`,
#>               `fu`
#>             FROM `condition_occurrence`
#>             INNER JOIN (
#>               SELECT `LHS`.*
#>               FROM (
#>                 SELECT `q01`.*, ROUND(`fu_diff` / 365.25, 3) AS `fu`
#>                 FROM (
#>                   SELECT
#>                     `person_id`,
#>                     `start_date`,
#>                     `end_date`,
#>                     `site`,
#>                     `cohort_id`,
#>                     `site_summ`,
#>                     CAST(`fu_diff` AS REAL) AS `fu_diff`
#>                   FROM (
#>                     SELECT
#>                       `cohort_tbl`.*,
#>                       julianday(end_date) - julianday(start_date) AS `fu_diff`
#>                     FROM `cohort_tbl`
#>                   ) AS `q01`
#>                 ) AS `q01`
#>               ) AS `LHS`
#>               LEFT JOIN (
#>                 SELECT `q01`.*, ROUND(`fu_diff` / 365.25, 3) AS `fu`
#>                 FROM (
#>                   SELECT
#>                     `person_id`,
#>                     `start_date`,
#>                     `end_date`,
#>                     `site`,
#>                     `cohort_id`,
#>                     `site_summ`,
#>                     CAST(`fu_diff` AS REAL) AS `fu_diff`
#>                   FROM (
#>                     SELECT
#>                       `cohort_tbl`.*,
#>                       julianday(end_date) - julianday(start_date) AS `fu_diff`
#>                     FROM `cohort_tbl`
#>                   ) AS `q01`
#>                 ) AS `q01`
#>               ) AS `RHS`
#>                 ON (
#>                   `LHS`.`person_id` = `RHS`.`person_id` AND
#>                   `LHS`.`start_date` = `RHS`.`start_date` AND
#>                   `LHS`.`end_date` = `RHS`.`end_date` AND
#>                   `LHS`.`site` = `RHS`.`site` AND
#>                   `LHS`.`cohort_id` = `RHS`.`cohort_id` AND
#>                   `LHS`.`site_summ` = `RHS`.`site_summ` AND
#>                   `LHS`.`fu_diff` = `RHS`.`fu_diff` AND
#>                   `LHS`.`fu` = `RHS`.`fu`
#>                 )
#>             ) AS `RHS`
#>               ON (`condition_occurrence`.`person_id` = `RHS`.`person_id`)
#>           ) AS `q01`
#>           WHERE
#>             (`condition_start_date` >= `start_date`) AND
#>             (`condition_start_date` <= `end_date`)
#>         ) AS `q01`
#>         GROUP BY
#>           `site_summ`,
#>           `person_id`,
#>           `start_date`,
#>           `end_date`,
#>           `fu`,
#>           `cohort_id`
#>       ) AS `q01`
#>     ) AS `q01`
#>   ) AS `q01`
#> ) AS `q01`
#> GROUP BY `person_id`
#> <SQL>
#> SELECT
#>   `person_id`,
#>   MAX(CASE WHEN (`domain` = 'all conditions') THEN `fact_ppy` END) AS `all conditions_ppy`
#> FROM (
#>   SELECT
#>     `person_id`,
#>     `domain`,
#>     CASE WHEN (`fu` != 0.0) THEN (ROUND(`total_fact_ct` / (`fu` * `k_mult`), 2)) WHEN NOT (`fu` != 0.0) THEN 0.0 END AS `fact_ppy`
#>   FROM (
#>     SELECT
#>       `q01`.*,
#>       CASE
#> WHEN (`fu` < 0.1) THEN 100.0
#> WHEN (`fu` >= 0.1 AND `fu` < 1.0) THEN 10.0
#> ELSE 1.0
#> END AS `k_mult`
#>     FROM (
#>       SELECT `q01`.*, 'all conditions' AS `domain`
#>       FROM (
#>         SELECT
#>           `site_summ`,
#>           `person_id`,
#>           `start_date`,
#>           `end_date`,
#>           `fu`,
#>           `cohort_id`,
#>           COUNT(*) AS `total_fact_ct`
#>         FROM (
#>           SELECT `q01`.*
#>           FROM (
#>             SELECT
#>               `condition_occurrence`.*,
#>               `start_date`,
#>               `end_date`,
#>               `site`,
#>               `cohort_id`,
#>               `site_summ`,
#>               `fu_diff`,
#>               `fu`
#>             FROM `condition_occurrence`
#>             INNER JOIN (
#>               SELECT `LHS`.*
#>               FROM (
#>                 SELECT `q01`.*, ROUND(`fu_diff` / 365.25, 3) AS `fu`
#>                 FROM (
#>                   SELECT
#>                     `person_id`,
#>                     `start_date`,
#>                     `end_date`,
#>                     `site`,
#>                     `cohort_id`,
#>                     `site_summ`,
#>                     CAST(`fu_diff` AS REAL) AS `fu_diff`
#>                   FROM (
#>                     SELECT
#>                       `cohort_tbl`.*,
#>                       julianday(end_date) - julianday(start_date) AS `fu_diff`
#>                     FROM `cohort_tbl`
#>                   ) AS `q01`
#>                 ) AS `q01`
#>               ) AS `LHS`
#>               LEFT JOIN (
#>                 SELECT `q01`.*, ROUND(`fu_diff` / 365.25, 3) AS `fu`
#>                 FROM (
#>                   SELECT
#>                     `person_id`,
#>                     `start_date`,
#>                     `end_date`,
#>                     `site`,
#>                     `cohort_id`,
#>                     `site_summ`,
#>                     CAST(`fu_diff` AS REAL) AS `fu_diff`
#>                   FROM (
#>                     SELECT
#>                       `cohort_tbl`.*,
#>                       julianday(end_date) - julianday(start_date) AS `fu_diff`
#>                     FROM `cohort_tbl`
#>                   ) AS `q01`
#>                 ) AS `q01`
#>               ) AS `RHS`
#>                 ON (
#>                   `LHS`.`person_id` = `RHS`.`person_id` AND
#>                   `LHS`.`start_date` = `RHS`.`start_date` AND
#>                   `LHS`.`end_date` = `RHS`.`end_date` AND
#>                   `LHS`.`site` = `RHS`.`site` AND
#>                   `LHS`.`cohort_id` = `RHS`.`cohort_id` AND
#>                   `LHS`.`site_summ` = `RHS`.`site_summ` AND
#>                   `LHS`.`fu_diff` = `RHS`.`fu_diff` AND
#>                   `LHS`.`fu` = `RHS`.`fu`
#>                 )
#>             ) AS `RHS`
#>               ON (`condition_occurrence`.`person_id` = `RHS`.`person_id`)
#>           ) AS `q01`
#>           WHERE
#>             (`condition_start_date` >= `start_date`) AND
#>             (`condition_start_date` <= `end_date`)
#>         ) AS `q01`
#>         GROUP BY
#>           `site_summ`,
#>           `person_id`,
#>           `start_date`,
#>           `end_date`,
#>           `fu`,
#>           `cohort_id`
#>       ) AS `q01`
#>     ) AS `q01`
#>   ) AS `q01`
#> ) AS `q01`
#> GROUP BY `person_id`
#> 
#> <PLAN>
#>    id parent notused
#> 1   2      0       0
#> 2  10      2      62
#> 3  14      2       0
#> 4  25      2      53
#> 5  42      2      62
#> 6 119      2       0
#> 7 194      0      82
#> 8 197      0       0
#>                                                                     detail
#> 1                                                           CO-ROUTINE q01
#> 2                                                          SCAN cohort_tbl
#> 3                       BLOOM FILTER ON condition_occurrence (person_id=?)
#> 4 SEARCH condition_occurrence USING AUTOMATIC COVERING INDEX (person_id=?)
#> 5                                                SCAN cohort_tbl LEFT-JOIN
#> 6                                             USE TEMP B-TREE FOR GROUP BY
#> 7                                                                 SCAN q01
#> 8                                             USE TEMP B-TREE FOR GROUP BY
#> <SQL>
#> SELECT
#>   `person_id`,
#>   MAX(CASE WHEN (`domain` = 'outpatient visits') THEN `fact_ppy` END) AS `outpatient visits_ppy`
#> FROM (
#>   SELECT
#>     `person_id`,
#>     `domain`,
#>     CASE WHEN (`fu` != 0.0) THEN (ROUND(`total_fact_ct` / (`fu` * `k_mult`), 2)) WHEN NOT (`fu` != 0.0) THEN 0.0 END AS `fact_ppy`
#>   FROM (
#>     SELECT
#>       `q01`.*,
#>       CASE
#> WHEN (`fu` < 0.1) THEN 100.0
#> WHEN (`fu` >= 0.1 AND `fu` < 1.0) THEN 10.0
#> ELSE 1.0
#> END AS `k_mult`
#>     FROM (
#>       SELECT `q01`.*, 'outpatient visits' AS `domain`
#>       FROM (
#>         SELECT
#>           `site_summ`,
#>           `person_id`,
#>           `start_date`,
#>           `end_date`,
#>           `fu`,
#>           `cohort_id`,
#>           COUNT(*) AS `total_fact_ct`
#>         FROM (
#>           SELECT `q01`.*
#>           FROM (
#>             SELECT
#>               `LHS`.*,
#>               `start_date`,
#>               `end_date`,
#>               `site`,
#>               `cohort_id`,
#>               `site_summ`,
#>               `fu_diff`,
#>               `fu`
#>             FROM (
#>               SELECT `visit_occurrence`.*
#>               FROM `visit_occurrence`
#>               WHERE (`visit_concept_id` = 9202.0)
#>             ) AS `LHS`
#>             INNER JOIN (
#>               SELECT `LHS`.*
#>               FROM (
#>                 SELECT `q01`.*, ROUND(`fu_diff` / 365.25, 3) AS `fu`
#>                 FROM (
#>                   SELECT
#>                     `person_id`,
#>                     `start_date`,
#>                     `end_date`,
#>                     `site`,
#>                     `cohort_id`,
#>                     `site_summ`,
#>                     CAST(`fu_diff` AS REAL) AS `fu_diff`
#>                   FROM (
#>                     SELECT
#>                       `cohort_tbl`.*,
#>                       julianday(end_date) - julianday(start_date) AS `fu_diff`
#>                     FROM `cohort_tbl`
#>                   ) AS `q01`
#>                 ) AS `q01`
#>               ) AS `LHS`
#>               LEFT JOIN (
#>                 SELECT `q01`.*, ROUND(`fu_diff` / 365.25, 3) AS `fu`
#>                 FROM (
#>                   SELECT
#>                     `person_id`,
#>                     `start_date`,
#>                     `end_date`,
#>                     `site`,
#>                     `cohort_id`,
#>                     `site_summ`,
#>                     CAST(`fu_diff` AS REAL) AS `fu_diff`
#>                   FROM (
#>                     SELECT
#>                       `cohort_tbl`.*,
#>                       julianday(end_date) - julianday(start_date) AS `fu_diff`
#>                     FROM `cohort_tbl`
#>                   ) AS `q01`
#>                 ) AS `q01`
#>               ) AS `RHS`
#>                 ON (
#>                   `LHS`.`person_id` = `RHS`.`person_id` AND
#>                   `LHS`.`start_date` = `RHS`.`start_date` AND
#>                   `LHS`.`end_date` = `RHS`.`end_date` AND
#>                   `LHS`.`site` = `RHS`.`site` AND
#>                   `LHS`.`cohort_id` = `RHS`.`cohort_id` AND
#>                   `LHS`.`site_summ` = `RHS`.`site_summ` AND
#>                   `LHS`.`fu_diff` = `RHS`.`fu_diff` AND
#>                   `LHS`.`fu` = `RHS`.`fu`
#>                 )
#>             ) AS `RHS`
#>               ON (`LHS`.`person_id` = `RHS`.`person_id`)
#>           ) AS `q01`
#>           WHERE
#>             (`visit_start_date` >= `start_date`) AND
#>             (`visit_start_date` <= `end_date`)
#>         ) AS `q01`
#>         GROUP BY
#>           `site_summ`,
#>           `person_id`,
#>           `start_date`,
#>           `end_date`,
#>           `fu`,
#>           `cohort_id`
#>       ) AS `q01`
#>     ) AS `q01`
#>   ) AS `q01`
#> ) AS `q01`
#> GROUP BY `person_id`
#> <SQL>
#> SELECT
#>   `person_id`,
#>   MAX(CASE WHEN (`domain` = 'outpatient visits') THEN `fact_ppy` END) AS `outpatient visits_ppy`
#> FROM (
#>   SELECT
#>     `person_id`,
#>     `domain`,
#>     CASE WHEN (`fu` != 0.0) THEN (ROUND(`total_fact_ct` / (`fu` * `k_mult`), 2)) WHEN NOT (`fu` != 0.0) THEN 0.0 END AS `fact_ppy`
#>   FROM (
#>     SELECT
#>       `q01`.*,
#>       CASE
#> WHEN (`fu` < 0.1) THEN 100.0
#> WHEN (`fu` >= 0.1 AND `fu` < 1.0) THEN 10.0
#> ELSE 1.0
#> END AS `k_mult`
#>     FROM (
#>       SELECT `q01`.*, 'outpatient visits' AS `domain`
#>       FROM (
#>         SELECT
#>           `site_summ`,
#>           `person_id`,
#>           `start_date`,
#>           `end_date`,
#>           `fu`,
#>           `cohort_id`,
#>           COUNT(*) AS `total_fact_ct`
#>         FROM (
#>           SELECT `q01`.*
#>           FROM (
#>             SELECT
#>               `LHS`.*,
#>               `start_date`,
#>               `end_date`,
#>               `site`,
#>               `cohort_id`,
#>               `site_summ`,
#>               `fu_diff`,
#>               `fu`
#>             FROM (
#>               SELECT `visit_occurrence`.*
#>               FROM `visit_occurrence`
#>               WHERE (`visit_concept_id` = 9202.0)
#>             ) AS `LHS`
#>             INNER JOIN (
#>               SELECT `LHS`.*
#>               FROM (
#>                 SELECT `q01`.*, ROUND(`fu_diff` / 365.25, 3) AS `fu`
#>                 FROM (
#>                   SELECT
#>                     `person_id`,
#>                     `start_date`,
#>                     `end_date`,
#>                     `site`,
#>                     `cohort_id`,
#>                     `site_summ`,
#>                     CAST(`fu_diff` AS REAL) AS `fu_diff`
#>                   FROM (
#>                     SELECT
#>                       `cohort_tbl`.*,
#>                       julianday(end_date) - julianday(start_date) AS `fu_diff`
#>                     FROM `cohort_tbl`
#>                   ) AS `q01`
#>                 ) AS `q01`
#>               ) AS `LHS`
#>               LEFT JOIN (
#>                 SELECT `q01`.*, ROUND(`fu_diff` / 365.25, 3) AS `fu`
#>                 FROM (
#>                   SELECT
#>                     `person_id`,
#>                     `start_date`,
#>                     `end_date`,
#>                     `site`,
#>                     `cohort_id`,
#>                     `site_summ`,
#>                     CAST(`fu_diff` AS REAL) AS `fu_diff`
#>                   FROM (
#>                     SELECT
#>                       `cohort_tbl`.*,
#>                       julianday(end_date) - julianday(start_date) AS `fu_diff`
#>                     FROM `cohort_tbl`
#>                   ) AS `q01`
#>                 ) AS `q01`
#>               ) AS `RHS`
#>                 ON (
#>                   `LHS`.`person_id` = `RHS`.`person_id` AND
#>                   `LHS`.`start_date` = `RHS`.`start_date` AND
#>                   `LHS`.`end_date` = `RHS`.`end_date` AND
#>                   `LHS`.`site` = `RHS`.`site` AND
#>                   `LHS`.`cohort_id` = `RHS`.`cohort_id` AND
#>                   `LHS`.`site_summ` = `RHS`.`site_summ` AND
#>                   `LHS`.`fu_diff` = `RHS`.`fu_diff` AND
#>                   `LHS`.`fu` = `RHS`.`fu`
#>                 )
#>             ) AS `RHS`
#>               ON (`LHS`.`person_id` = `RHS`.`person_id`)
#>           ) AS `q01`
#>           WHERE
#>             (`visit_start_date` >= `start_date`) AND
#>             (`visit_start_date` <= `end_date`)
#>         ) AS `q01`
#>         GROUP BY
#>           `site_summ`,
#>           `person_id`,
#>           `start_date`,
#>           `end_date`,
#>           `fu`,
#>           `cohort_id`
#>       ) AS `q01`
#>     ) AS `q01`
#>   ) AS `q01`
#> ) AS `q01`
#> GROUP BY `person_id`
#> 
#> <PLAN>
#>    id parent notused
#> 1   2      0       0
#> 2  10      2      62
#> 3  14      2       0
#> 4  28      2      53
#> 5  46      2      62
#> 6 123      2       0
#> 7 198      0      82
#> 8 201      0       0
#>                                                                                                detail
#> 1                                                                                      CO-ROUTINE q01
#> 2                                                                                     SCAN cohort_tbl
#> 3                               BLOOM FILTER ON visit_occurrence (visit_concept_id=? AND person_id=?)
#> 4 SEARCH visit_occurrence USING AUTOMATIC PARTIAL COVERING INDEX (visit_concept_id=? AND person_id=?)
#> 5                                                                           SCAN cohort_tbl LEFT-JOIN
#> 6                                                                        USE TEMP B-TREE FOR GROUP BY
#> 7                                                                                            SCAN q01
#> 8                                                                        USE TEMP B-TREE FOR GROUP BY
#> ┌ Output Function Details ──────────────────────────────────────┐
#> │ You can optionally use this dataframe in the accompanying     │
#> │ `ssc_output` function. Here are the parameters you will need: │
#> │                                                               │
#> │ Always Required: process_output                               │
#> │ Required for Check: alt_cohort_filter                         │
#> │                                                               │
#> │ See ?ssc_output for more details.                             │
#> └───────────────────────────────────────────────────────────────┘

ssc_process_example
#> $summary_values
#> # A tibble: 28 × 7
#>    site  cohort_id cohort_characteristic fact_group fact_summary cohort_total_pt
#>    <chr> <chr>     <chr>                 <chr>             <dbl>           <int>
#>  1 comb… Sample A… median_age_cohort_en… Cohort De…       -12.0               12
#>  2 comb… Sample A… median_age_first_vis… Cohort De…         1.46              12
#>  3 comb… Sample A… median_all condition… Clinical …         0                 12
#>  4 comb… Sample A… median_fu             Cohort De…         0                 12
#>  5 comb… Sample A… median_outpatient vi… Utilizati…         0                 12
#>  6 comb… base_coh… median_age_cohort_en… Cohort De…       -12.0               12
#>  7 comb… base_coh… median_age_first_vis… Cohort De…         1.46              12
#>  8 comb… base_coh… median_all condition… Clinical …         0                 12
#>  9 comb… base_coh… median_fu             Cohort De…         0                 12
#> 10 comb… base_coh… median_outpatient vi… Utilizati…         0                 12
#> # ℹ 18 more rows
#> # ℹ 1 more variable: output_function <chr>
#> 
#> $cohort_overlap
#> # A tibble: 1 × 3
#>   site     cohort_group                 group_ct
#>   <chr>    <chr>                           <int>
#> 1 combined base_cohort&Sample Alternate       12
#> 

#' Execute `ssc_output` function
ssc_output_example <- ssc_output(process_output = ssc_process_example,
                                 alt_cohort_filter = 'Sample Alternate') %>%
  suppressMessages()

ssc_output_example[[1]]

ssc_output_example[[2]]


#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(ssc_output_example[[2]])
#> Warning: geom_GeomLabel() has yet to be implemented in plotly.
#>   If you'd like to see this geom implemented,
#>   Please open an issue with your example code at
#>   https://github.com/ropensci/plotly/issues
#> Warning: geom_GeomLabel() has yet to be implemented in plotly.
#>   If you'd like to see this geom implemented,
#>   Please open an issue with your example code at
#>   https://github.com/ropensci/plotly/issues

{"x":{"data":[{"orientation":"v","width":[0,0,0,0,0],"base":[0.55000000000000004,1.55,2.5499999999999998,3.5499999999999998,4.5499999999999998],"x":[0,0,0,0,0],"y":[0.89999999999999991,0.90000000000000013,0.90000000000000036,0.90000000000000036,0.90000000000000036],"text":["diff_base: 0<br />cf: all conditions<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: age cohort entry<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: age first visit<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: follow-up<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: outpatient visits<br />cohort_id: Sample Alternate"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,77,111,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Sample Alternate","legendgroup":"Sample Alternate","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0,0,0,0,0,0,0,0,0],"base":[0.55000000000000004,1.55,2.5499999999999998,3.5499999999999998,4.5499999999999998,5.5499999999999998,6.5499999999999998,7.5499999999999998,8.5500000000000007],"x":[0,0,0,0,0,0,0,0,0],"y":[0.89999999999999991,0.90000000000000013,0.90000000000000036,0.90000000000000036,0.90000000000000036,0.90000000000000036,0.90000000000000036,0.89999999999999947,0.89999999999999858],"text":["diff_base: 0<br />cf: asian race<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: black race<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: female<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: hispanic<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: mixed race<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: other race<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: unknown race<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: white race<br />cohort_id: Sample Alternate","diff_base: 0<br />cf: hypertension<br />cohort_id: Sample Alternate"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,77,111,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Sample Alternate","legendgroup":"Sample Alternate","showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null},{"name":"Sample Alternate","legendgroup":"Sample Alternate","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"name":"Sample Alternate","legendgroup":"Sample Alternate","showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":52.529680365296812,"r":7.3059360730593621,"b":37.260273972602747,"l":124.9315068493151},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Difference between Alternate and Base Cohorts","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,0.33333333333333331],"automargin":true,"type":"linear","autorange":false,"range":[-0.050000000000000003,0.050000000000000003],"tickmode":"array","ticktext":["-0.050","-0.025","0.000","0.025","0.050"],"tickvals":[-0.050000000000000003,-0.025000000000000001,0,0.025000000000000008,0.050000000000000003],"categoryorder":"array","categoryarray":["-0.050","-0.025","0.000","0.025","0.050"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"y","title":"","hoverformat":".2f"},"annotations":[{"text":"Difference from Base Cohort","x":0.5,"y":0,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis","yshift":-21.917808219178088},{"text":"Cohort Characteristic","x":0,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis","xshift":-109.58904109589045},{"text":"Medians","x":0.16666666666666666,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Proportions","x":0.83333333333333337,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,5.5999999999999996],"tickmode":"array","ticktext":["all conditions","age cohort entry","age first visit","follow-up","outpatient visits"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["all conditions","age cohort entry","age first visit","follow-up","outpatient visits"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":0.33333333333333331,"y0":0,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":0.33333333333333331,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0.66666666666666674,"x1":1,"y0":0,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0.66666666666666674,"x1":1,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[-0.050000000000000003,0.050000000000000003],"tickmode":"array","ticktext":["-0.050","-0.025","0.000","0.025","0.050"],"tickvals":[-0.050000000000000003,-0.025000000000000001,0,0.025000000000000008,0.050000000000000003],"categoryorder":"array","categoryarray":["-0.050","-0.025","0.000","0.025","0.050"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.66666666666666674,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"yaxis2":{"type":"linear","autorange":false,"range":[0.40000000000000002,9.5999999999999996],"tickmode":"array","ticktext":["asian race","black race","female","hispanic","mixed race","other race","unknown race","white race","hypertension"],"tickvals":[1,2,3,4,5,6,6.9999999999999991,8,9],"categoryorder":"array","categoryarray":["asian race","black race","female","hispanic","mixed race","other race","unknown race","white race","hypertension"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"x2","title":"","hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":{"text":"Alternate Cohort","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1bc26a8b0774":{"x":{},"y":{},"fill":{},"type":"bar"},"1bc23db83067":{"x":{},"y":{},"fill":{},"label":{}}},"cur_data":"1bc26a8b0774","visdat":{"1bc26a8b0774":["function (y) ","x"],"1bc23db83067":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
