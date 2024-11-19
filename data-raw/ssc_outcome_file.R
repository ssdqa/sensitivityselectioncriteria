## code to prepare `ssc_domain_file` dataset goes here

ssc_outcome_file <- tibble::tibble(concept_id = c(45542739, 1567956, 40164882, 40063353, 3007263, 40758583),
                                   concept_code = c('E11.1', 'E11', '861027', '406257', '17855-8', '55454-3'),
                                   concept_name = c('Type 2 diabetes mellitus, with ketoacidosis', 'Type 2 diabetes mellitus',
                                                   'metformin hydrochloride 100 MG/ML Oral Solution [Riomet]',
                                                   'metformin Oral Solution [Riomet]',
                                                   'Hemoglobin A1c/Hemoglobin.total in Blood by calculation', 'Hemoglobin A1c in Blood'),
                                   vocabulary_id = c('ICD10CM', 'ICD10CM', 'RxNorm', 'RxNorm', 'LOINC', 'LOINC'),
                                   variable = c('T2DM diagnosis', 'T2DM diagnosis', 'Metformin', 'Metformin',
                                               'Hba1c lab', 'Hba1c lab'),
                                   domain = c('condition_occurrence', 'condition_occurrence', 'drug_exposure',
                                             'drug_exposure', 'measurement', 'measurement'))

usethis::use_data(ssc_outcome_file, overwrite = TRUE)
