# Multi-Site Analysis for Independent Data Sources

The multi-site analyses included in this suite are intended to be
executed against data that are all stored in the same place. However,
there may be some instances where the data associated with each site is
stored in independent locations. This vignette outlines how the
multi-site analysis can be executed in these instances.

## Multi-Site Exploratory Analysis

First, execute the **Single Site, Exploratory** analyses, configured
appropriately for your study, against each data source.

``` r
library(sensitivityselectioncriteria)

my_table <- ssc_process(base_cohort = my_cohort,
                        alt_cohorts = list('My Alternate Cohort' = my_alt_cohort),
                        omop_or_pcornet = 'omop',
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        ...)
```

This function will produce 2 tables. Select the **first** table in the
list from each result set, then combine these results into a single
table with the different sites delineated in the `site` column.

``` r
my_final_results <- my_table1[[1]] %>% dplyr::union(my_table2[[1]]) ... %>%
  dplyr::union(my_table_n[[1]])
```

## Multi-Site Anomaly Detection Analysis

It is slightly more complex to reproduce the anomaly detection analysis
in this case, due to the level of summarization that is output by the
typical function. Instead of running the `ssc_process` as you normally
would, you will need to use an internal function to produce the correct
results.

The first step will be to run the appropriate internal function,
depending on which CDM you are using, against each of the data sources.
It intakes most of the same parameters as the primary `ssc_process`
function.

``` r
## For an OMOP CDM:
my_omop_table <- 
  sensitivityselectioncriteria:::compare_cohort_def_omop(
                                    base_cohort = my_cohort,
                                    alt_cohorts = list('My Alternate Cohort' = my_alt_cohort),
                                    multi_or_single_site = 'single',
                                    ...)

## For a PCORnet CDM:
my_pcornet_table <- 
  sensitivityselectioncriteria:::compare_cohort_def_pcnt(
                                    base_cohort = my_cohort,
                                    alt_cohorts = list('My Alternate Cohort' = my_alt_cohort),
                                    multi_or_single_site = 'single',
                                    ...)
```

Once this function has been executed against each data source, combine
these results into a single table with the different sites delineated in
the `site` column.

``` r
my_combo_results <- my_table1 %>% dplyr::union(my_table2) ... %>%
  dplyr::union(my_table_n)
```

Then, pass this combined table through the anomaly detection function
that will execute a standardized mean difference computation. If you
provided custom demographic mappings, extract the demographic labels
from this file. Otherwise, use the appropriate package-provided table
based on your data model (either `ssc_omop_demographics` or
`ssc_pcornet_demographics`).

``` r
my_final_results <- 
  sensitivityselectioncriteria:::compare_cohort_smd(cohort_def_output = my_combo_results,
                                                    demographic_vector = 
                                                      my_demographic_table %>%
                                                        select(demographic) %>% pull())
```
