
#' SSC Sample Domain File
#'
#' A sample version of the file structure expected for the `domain_tbl`
#' parameter in the `ssc_process` function. The user should recreate
#' this file and include their own domain definitions.
#'
#' #' @format ## ssc_domain_file
#' \describe{
#' A data frame with 2 rows and 5 columns
#'   \item{domain}{An arbitrary string to label the domain of interest}
#'   \item{domain_tbl}{The name of the CDM table associated with the domain of interest}
#'   \item{concept_field}{The name of the column in the domain table that contains the concepts of interest listed in the concept_set file.}
#'   \item{date_field}{The name of the column in the domain table that contains dates to be used for time-based filtering.}
#'   \item{vocabulary_field}{(PCORnet only) The name of the column in the domain table where the vocabulary type is stored}
#'   \item{filter_logic}{(optional) a string to be parsed as logic to filter the domain_tbl as needed to best represent the domain}
#' }
#'
"ssc_domain_file"

#' SSC Sample Outcomes Concept Set
#'
#' A sample version of the file structure expected for the `outcome_concepts`
#' parameter in the `ssc_process` function. The user should recreate this
#' file and include their own clinical concepts.
#'
#' @format ## ssc_outcome_file
#' A data frame with 6 rows and 6 columns
#' \describe{
#'   \item{concept_id}{The OMOP concept_id; if the PCORnet CDM is being used, default this column to a random integer like the row number}
#'   \item{concept_code}{The original code associated with the concept_id}
#'   \item{concept_name}{(optional)The string name of the concept}
#'   \item{vocabulary_id}{The vocabulary associated with the concept; if the PCORnet CDM is being used, ensure that the values of this field match the vocabulary abbreviations used in the CDM itself}
#'   \item{variable}{A string label for the variable associated with the concept}
#'   \item{domain}{The domain table where the concept should be identified. This should match the domain listed in the domain_tbl file}
#' }
#'
"ssc_outcome_file"

#' SSC Demographic Mappings -- OMOP
#'
#' A sample version of the file structure expected for the `demographic_mappings`
#' parameter in the `ssc_process` function. The user should recreate this
#' file and include their own clinical concepts.
#'
#' If the parameter is left NULL, this file or its PCORnet compliment will be used
#' in place of custom mappings.
#'
#' @format ## ssc_omop_demographics
#' A data frame with 6 rows and 6 columns
#' \describe{
#'   \item{demographic}{a string used to identify the demographic category}
#'   \item{concept_field}{the name of the field in the CDM person table containing the appropriate demographic information}
#'   \item{field_values}{the values in concept_field that represent the demographic category}
#' }
#'
"ssc_omop_demographics"

#' SSC Demographic Mappings -- PCORnet
#'
#' A sample version of the file structure expected for the `demographic_mappings`
#' parameter in the `ssc_process` function. The user should recreate this
#' file and include their own clinical concepts.
#'
#' If the parameter is left NULL, this file or its OMOP compliment will be used
#' in place of custom mappings.
#'
#' @format ## ssc_pcornet_demographics
#' A data frame with 6 rows and 6 columns
#' \describe{
#'   \item{demographic}{a string used to identify the demographic category}
#'   \item{concept_field}{the name of the field in the CDM demographic table containing the appropriate demographic information}
#'   \item{field_values}{the values in concept_field that represent the demographic category}
#' }
#'
"ssc_pcornet_demographics"
