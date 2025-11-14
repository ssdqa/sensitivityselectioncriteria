# *Multi-Site, Anomaly Detection, Cross-Sectional*

*Multi-Site, Anomaly Detection, Cross-Sectional*

## Usage

``` r
ssc_ms_anom_cs(process_output, large_n = FALSE, large_n_sites = NULL)
```

## Arguments

- process_output:

  the output of ssc_process

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally be compared against summary
  statistics

## Value

a table displaying the mean, median, and iqr standardized mean
difference values for each site & alternative cohort definition vs
baseline a dot plot showing the standardized mean difference value vs
baseline for each site & alternative cohort definition
