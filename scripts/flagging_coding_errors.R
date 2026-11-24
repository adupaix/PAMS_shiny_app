#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2026-11-24
#'#***************************************************************************
#'@description: functions used to flag coding errors in the user answers
#'  exported from sysrev (PAMS systematic map). Each flagging function takes the
#'  user answers data frame (as read from the *-user_answers.csv) and returns
#'  one row per flagged case study, with the sysrev link to the article.
#'  These functions only flag: the corrections themselves are applied in
#'  R/pams_data_correction.R
#'#***************************************************************************


#' Build the sysrev link to an article
#'
#' @param project_id project ids, with or without the "proj_" prefix
#' @param article_id article ids, with or without the "art_" prefix
sysrev_article_link <- function(project_id, article_id) {
  paste0('https://www.sysrev.com/u/196345/p/',
         gsub('[^0-9]', '', project_id),
         '/article/',
         gsub('[^0-9]', '', article_id))
}


#' Keep one row per coded case study (included articles only)
case_study_level <- function(user_answers) {
  user_answers |>
    dplyr::filter(as.logical(include_code) %in% TRUE) |>
    dplyr::distinct(project_id, article_id, user_id, case_study_index,
                    .keep_all = TRUE) |>
    dplyr::select(project_id, article_id, user_id, case_study_index,
                  ecosystem_main, study_site_location, title)
}


#' 1. Flag land/sea mismatches between ecosystem_main and study_site_location
#'
#' Codes < 200 are terrestrial, codes >= 200 are marine
#' (Annexe 1 - Codes pour study_site_location).
#' Flag types:
#'  - "typo": a code that is not in the reference table (location_code.csv),
#'    has more than 3 digits, or is not numeric (e.g. "2023")
#'  - "land_code_in_ocean_only": ecosystem_main contains only Coastal ocean
#'    and/or Open-ocean but terrestrial codes (< 200) are coded
#'  - "marine_code_in_land_only": ecosystem_main is Land (coastal) only but
#'    marine codes (>= 200) are coded
#'  - "mixed_missing_marine" / "mixed_missing_land": ecosystem_main contains
#'    both Land (coastal) and an ocean category, but only terrestrial / only
#'    marine codes are coded
#'
#' @param user_answers data frame read from the *-user_answers.csv
#' @param location_codes vector of valid location codes (numeric)
#' @param exceptions case studies validated as correct despite the mismatch
#'   (not flagged as long as ecosystem_main and study_site_location are
#'   unchanged), see land_sea_validated_exceptions()
#' @return one row per flagged case study x flag type, with the sysrev link
flag_land_sea_mismatch <- function(user_answers, location_codes,
                                   exceptions = land_sea_validated_exceptions()) {

  cs <- case_study_level(user_answers)

  codes_long <- cs |>
    dplyr::filter(!is.na(study_site_location)) |>
    tidyr::separate_longer_delim(study_site_location, delim = ';') |>
    dplyr::mutate(
      code_raw = stringr::str_trim(study_site_location),
      code = dplyr::if_else(grepl('^[0-9]{1,3}$', code_raw),
                            suppressWarnings(as.numeric(code_raw)),
                            NA_real_),
      is_typo = is.na(code) | !code %in% location_codes
    )

  per_cs <- codes_long |>
    dplyr::group_by(project_id, article_id, user_id, case_study_index) |>
    dplyr::summarise(
      typo_codes = paste(code_raw[is_typo], collapse = ';'),
      n_land = sum(!is_typo & code < 200),
      n_marine = sum(!is_typo & code >= 200),
      .groups = 'drop'
    )

  cs |>
    dplyr::left_join(per_cs,
                     by = c('project_id', 'article_id', 'user_id',
                            'case_study_index')) |>
    dplyr::mutate(
      eco_land = grepl('Land', ecosystem_main),
      eco_ocean = grepl('Coastal ocean|Open-ocean', ecosystem_main),
      flag_typo = !is.na(typo_codes) & typo_codes != '',
      flag_land_code_in_ocean_only = eco_ocean & !eco_land & n_land > 0,
      flag_marine_code_in_land_only = eco_land & !eco_ocean & n_marine > 0,
      flag_mixed_missing_marine = eco_land & eco_ocean &
        n_land > 0 & n_marine == 0,
      flag_mixed_missing_land = eco_land & eco_ocean &
        n_marine > 0 & n_land == 0
    ) |>
    tidyr::pivot_longer(dplyr::starts_with('flag_'),
                        names_to = 'flag', names_prefix = 'flag_',
                        values_to = 'flagged') |>
    dplyr::filter(flagged %in% TRUE) |>
    dplyr::anti_join(exceptions,
                     by = c('project_id', 'article_id', 'user_id',
                            'case_study_index', 'ecosystem_main',
                            'study_site_location')) |>
    dplyr::mutate(link = sysrev_article_link(project_id, article_id)) |>
    dplyr::select(flag, project_id, article_id, user_id, case_study_index,
                  ecosystem_main, study_site_location, typo_codes, link,
                  title) |>
    dplyr::arrange(flag, project_id, article_id, user_id, case_study_index)
}


#' Case studies validated (2026-11-24) as correct despite a land/sea mismatch,
#' with the values as they are after pams_data_correction(). Mostly marine
#' socio-ecosystems for which on-land activities/institutions are also studied.
land_sea_validated_exceptions <- function() {
  tibble::tribble(
    ~project_id,   ~article_id,     ~user_id,      ~case_study_index,
    ~ecosystem_main,                  ~study_site_location,
    'proj_316779', 'art_106602200', 'user_196345', 0,
    'Coastal ocean',                  '003;202',
    'proj_316779', 'art_108147626', 'user_196345', 0,
    'Coastal ocean',                  '71;270',
    'proj_316786', 'art_106604242', 'user_2706',   0,
    'Coastal ocean',                  '201;002',
    'proj_316786', 'art_106604242', 'user_2706',   1,
    'Coastal ocean',                  '201;003',
    'proj_316790', 'art_106604350', 'user_204463', 0,
    'Coastal ocean',                  '81;280',
    'proj_316786', 'art_106604239', 'user_2706',   0,
    'Land (coastal)',                 '212',
    'proj_316786', 'art_106604239', 'user_2706',   3,
    'Land (coastal)',                 '216',
    'proj_316786', 'art_106604239', 'user_2706',   4,
    'Land (coastal)',                 '216',
    'proj_316790', 'art_106604329', 'user_204463', 0,
    'Land (coastal); Coastal ocean',  '216'
  ) |>
    dplyr::mutate(case_study_index = as.integer(case_study_index))
}
