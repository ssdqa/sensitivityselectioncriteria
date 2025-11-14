# SSC Demographic Mappings – PCORnet

A sample version of the file structure expected for the
`demographic_mappings` parameter in the `ssc_process` function. The user
should recreate this file and include their own clinical concepts.

## Usage

``` r
ssc_pcornet_demographics
```

## Format

### ssc_pcornet_demographics

A data frame with 3 columns

- demographic:

  a string used to identify the demographic category

- concept_field:

  the name of the field in the CDM demographic table containing the
  appropriate demographic information

- field_values:

  the values in concept_field that represent the demographic category

## Details

If the parameter is left NULL, this file or its OMOP compliment will be
used in place of custom mappings.
