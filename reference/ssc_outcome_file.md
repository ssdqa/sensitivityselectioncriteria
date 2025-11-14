# SSC Sample Outcomes Concept Set

A sample version of the file structure expected for the
`outcome_concepts` parameter in the `ssc_process` function. The user
should recreate this file and include their own clinical concepts.

## Usage

``` r
ssc_outcome_file
```

## Format

### ssc_outcome_file

A data frame with 6 columns

- concept_id:

  The OMOP concept_id; if the PCORnet CDM is being used, default this
  column to a random integer like the row number

- concept_code:

  The original code associated with the concept_id

- concept_name:

  (optional)The string name of the concept

- vocabulary_id:

  The vocabulary associated with the concept; if the PCORnet CDM is
  being used, ensure that the values of this field match the vocabulary
  abbreviations used in the CDM itself

- variable:

  A string label for the variable associated with the concept

- domain:

  The domain table where the concept should be identified. This should
  match the domain listed in the domain_tbl file
