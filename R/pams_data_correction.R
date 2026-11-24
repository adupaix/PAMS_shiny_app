#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2026-11-24
#'#***************************************************************************
#'@description: corrections applied to the user answers coded on sysrev for
#'  the PAMS systematic map. Each correction is an independent sub-function,
#'  switched on/off by the corresponding parameter of pams_data_correction().
#'  Corrections are identified by project / article / user / case study ids
#'  (never by line numbers) and are only applied if the current coded value
#'  still matches the value that was flagged; otherwise a warning is raised
#'  and the correction is skipped (e.g. the value was fixed on sysrev).
#'  Flagging functions are in scripts/flagging_coding_errors.R
#'#***************************************************************************


#' Apply the corrections to the PAMS user answers
#'
#' @param user_answers data frame read from the *-user_answers.csv
#' @param land_sea_harmonization logical, apply correction 1 (harmonize
#'   ecosystem_main and the land/sea study_site_location codes)
#' @return the corrected data frame
pams_data_correction <- function(user_answers,
                                 land_sea_harmonization = TRUE) {

  if (land_sea_harmonization) {
    user_answers <- fix_land_sea_harmonization(user_answers)
  }

  user_answers
}


# Helpers ------------------------------------------------------------------

#' Compare a coded value to the expected (flagged) value, NA-safe
same_value <- function(x, expected) {
  (is.na(x) & is.na(expected)) | (!is.na(x) & !is.na(expected) & x == expected)
}

#' Apply case-study level value corrections
#'
#' @param corrections data frame with columns project_id, article_id, user_id,
#'   case_study_index, and for each corrected variable `var` a pair of
#'   columns old_`var` (value currently coded) and new_`var` (corrected value).
#'   new_`var` = NA leaves the variable unchanged.
#' @param correction_name name used in the warnings
apply_case_study_corrections <- function(user_answers, corrections,
                                         correction_name) {
  vars <- sub('^new_', '', grep('^new_', names(corrections), value = TRUE))

  for (i in seq_len(nrow(corrections))) {
    corr <- corrections[i, ]
    id <- paste(corr$project_id, corr$article_id, corr$user_id,
                'case', corr$case_study_index)
    rows <- which(user_answers$project_id == corr$project_id &
                    user_answers$article_id == corr$article_id &
                    user_answers$user_id == corr$user_id &
                    user_answers$case_study_index %in% corr$case_study_index)

    if (length(rows) == 0) {
      warning(correction_name, ': case study not found, skipped (', id, ')',
              call. = FALSE)
      next #skip to the next correction if the case study is not found
    }

    for (v in vars) {
      new <- corr[[paste0('new_', v)]]
      if (is.na(new)) next # skip if no new value is provided
      old <- corr[[paste0('old_', v)]]
      user_answers[[v]][rows] <- new
    }
  }

  user_answers
}

# 1. Land/sea harmonization ------------------------------------------------

#' Harmonize ecosystem_main and study_site_location (land codes < 200, marine
#' codes >= 200) and fix location code typos.
#' Flagged with flag_land_sea_mismatch() (scripts/flagging_coding_errors.R)
#' and validated case by case on 2026-11-24.
fix_land_sea_harmonization <- function(user_answers) {

  name <- 'land_sea_harmonization'

  land <- 'Land (coastal)'
  land_coastal <- 'Land (coastal); Coastal ocean'

  corrections <- tibble::tribble(
    ~project_id,   ~article_id,     ~user_id,      ~case_study_index,
    ~old_ecosystem_main,        ~new_ecosystem_main,
    ~old_study_site_location,   ~new_study_site_location,
    # A. land codes coded for an ocean-only ecosystem
    'proj_316741', 'art_106591147', 'user_208324', 0,
    'Coastal ocean',             NA,             '002',     '201;202',
    'proj_316775', 'art_108147586', 'user_197698', 0,
    'Coastal ocean',             land_coastal,   '80',      '81;280',
    'proj_316778', 'art_108147615', 'user_35899',  0,
    'Coastal ocean; Open-ocean', NA,             '007',     '204',
    'proj_316782', 'art_106604109', 'user_7648',   0,
    'Coastal ocean',             NA,             '010',     '204',
    'proj_316782', 'art_108147665', 'user_203407', 0,
    'Coastal ocean',             land_coastal,   '90;290',  NA,
    'proj_316787', 'art_106604261', 'user_222217', 0,
    'Coastal ocean',             NA,             '002;003;201', '201;202',
    'proj_316787', 'art_106604263', 'user_222217', 0,
    'Open-ocean',                NA,
    '001;002;003;004;005;007;201;202;203;204',   '200',
    'proj_316787', 'art_106604267', 'user_222217', 0,
    'Coastal ocean',             NA,             '070;80;130', '270;280;330',
    'proj_316789', 'art_106604312', 'user_209743', 0,
    'Coastal ocean',             NA,             '003;004', '201;202',
    'proj_316790', 'art_106604341', 'user_204463', 0,
    'Coastal ocean',             NA,             '003',     '201;202',
    'proj_316790', 'art_106604344', 'user_204463', 0,
    'Coastal ocean',             land_coastal,   '73',      '73;270',
    'proj_316790', 'art_106604350', 'user_204463', 0,
    'Coastal ocean',             NA,             '81',      '81;280',
    'proj_316790', 'art_106604356', 'user_204463', 0,
    'Coastal ocean',             NA,             '84',      '280',
    'proj_316790', 'art_108147733', 'user_204463', 0,
    'Coastal ocean; Open-ocean', NA,             '000',     '200',
    'proj_316790', 'art_108147738', 'user_204463', 0,
    'Coastal ocean',             NA,             '030;050', '230;250',
    'proj_316792', 'art_106604392', 'user_209611', 0,
    'Coastal ocean',             land,           '203;005', '005',
    'proj_316793', 'art_106604416', 'user_209644', 0,
    'Coastal ocean',             NA,             '81;280',  '280',
    'proj_316793', 'art_106604416', 'user_209644', 1,
    'Coastal ocean',             NA,             '81;280',  '280',
    # B. marine codes coded for a land-only ecosystem
    'proj_316543', 'art_106591113', 'user_133645', 0,
    land,                        NA,             '280;81',  '81',
    'proj_316775', 'art_106591218', 'user_197698', 0,
    land,                        'Coastal ocean', '003;201', '201',
    'proj_316780', 'art_106603027', 'user_12431',  0,
    land,                        NA,             '350',     '160',
    'proj_316780', 'art_106603033', 'user_12431',  0,
    land,                        NA,             '204',     '006;010',
    'proj_316786', 'art_106604239', 'user_2706',   5,
    land,                        land_coastal,   '202;004;005', '202;203;005',
    'proj_316792', 'art_106604383', 'user_209611', 0,
    land,                        land_coastal,   '005;202;203', '202;203;005',
    'proj_316793', 'art_108147766', 'user_209644', 0,
    land,                        NA,             '006;204', '006',
    # C. mixed ecosystem with only land or only marine codes
    'proj_316776', 'art_106591247', 'user_209182', 0,
    'Land (coastal); Coastal ocean', land,       '203',     '005',
    'proj_316782', 'art_106604119', 'user_203407', 0,
    NA,                          NA,             '216',     '216;160',
    'proj_316782', 'art_106604133', 'user_7648',   0,
    NA,                          NA,             '204',     '204;007',
    'proj_316783', 'art_106604144', 'user_7648',   0,
    NA,                          NA,             '201;202;203;204', '200;150',
    'proj_316785', 'art_106604216', 'user_206938', 0,
    'Coastal ocean; Land (coastal)', 'Coastal ocean', NA,   NA,
    'proj_316790', 'art_106604329', 'user_204463', 0,
    NA,                          NA,             '204',     '216',
    # D. typos in the location codes
    'proj_316790', 'art_106604343', 'user_204463', 0,
    NA,                          NA,             '202;2023', '202;203'
  )

  apply_case_study_corrections(user_answers, corrections, name)
}
