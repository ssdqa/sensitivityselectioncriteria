## code to prepare `ssc_omop_demographics` dataset goes here

ssc_omop_demographics <- dplyr::tibble('demographic' = c('black_race', 'white_race',
                                                         'asian_race', 'mixed_race',
                                                         'unknown_race', 'other_race',
                                                         'female', 'hispanic'),
                                       'concept_field' = c('race_concept_id', 'race_concept_id',
                                                           'race_concept_id', 'race_concept_id',
                                                           'race_concept_id', 'race_concept_id',
                                                           'gender_concept_id', 'ethnicity_concept_id'),
                                       'field_values' = c('8516', '8527', '8515', '44814659',
                                                          "44814660, 44814650, 44814653",
                                                          "44814649, 8657, 8557", '8532',
                                                          '38003563'))

usethis::use_data(ssc_omop_demographics, overwrite = TRUE)


## code to prepare `ssc_pcornet_demographics` dataset goes here

ssc_pcornet_demographics <- dplyr::tibble('demographic' = c('black_race', 'white_race',
                                                         'asian_race', 'mixed_race',
                                                         'unknown_race', 'other_race',
                                                         'female', 'hispanic'),
                                           'concept_field' = c('race', 'race',
                                                               'race', 'race',
                                                               'race', 'race',
                                                               'sex', 'hispanic'),
                                           'field_values' = c('03', '05', '02', '06',
                                                              "'NI', 'UN', '07'",
                                                              "'OT', '01', '04'", 'F',
                                                              'Y'))

usethis::use_data(ssc_pcornet_demographics, overwrite = TRUE)
