# SSC Sample Domain File

A sample version of the file structure expected for the `domain_tbl`
parameter in the `ssc_process` function. The user should recreate this
file and include their own domain definitions.

## Usage

``` r
ssc_domain_file
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 2
rows and 6 columns.

## Details

\#' @format \## ssc_domain_file

- domain:

  An arbitrary string to label the domain of interest

- domain_tbl:

  The name of the CDM table associated with the domain of interest

- concept_field:

  The name of the column in the domain table that contains the concepts
  of interest listed in the concept_set file.

- date_field:

  The name of the column in the domain table that contains dates to be
  used for time-based filtering.

- vocabulary_field:

  (PCORnet only) The name of the column in the domain table where the
  vocabulary type is stored

- filter_logic:

  (optional) a string to be parsed as logic to filter the domain_tbl as
  needed to best represent the domain
