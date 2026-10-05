#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2026-08-03
#'@email : amael.dupaix@ird.fr
#'#***************************************************************************
#'@description: script to load data from sysrev through API
#'
#'#***************************************************************************

if(reload_data ||
   !file.exists(file.path('data',
                          paste0(Sys.Date(), '-user_answers.csv')))){
  
  file.remove(list.files('data', pattern = '-user_answers.csv',
                         full.names = TRUE))
  
  project_ids <- as.numeric(
    readLines(file.path(data_dir, 'sysrev_projects.txt'))
  )
  articles_data <- SLRtools::zotero_format_data(
    readRDS(file.path(data_dir, 'articles_data.rds'))
  )
  
  for (i in 1:length(project_ids)){
    project <- project_ids[i] ; project_id <- paste0("proj_", project)
    
    parsed <- SLRtools::sysrev_generate_export(project_id = project_id,
                                               sysrev_key = sysrev_key)
    SLRtools::sysrev_download_export(parsed, project_id, data_dir)
  }
  
  user_answers <- list()
  unresolved_papers <- list()
  for (i in 1:length(project_ids)){
    project <- project_ids[i] ; project_id <- paste0("proj_", project)
    
    formatted_data <- SLRtools::sysrev_format_data(
      project_id = project_id,
      data_dir = data_dir,
      articles_data = articles_data,
      group_label_fun = sysrev_develop_group_labels_pams,
    )
    user_answers[[i]] <- formatted_data$user_answers
  }
  
  user_answers <- dplyr::bind_rows(user_answers)
  
  utils::write.csv(user_answers,
                   file = file.path(data_dir,
                                    paste0(Sys.Date(), '-user_answers.csv')),
                   row.names = F)
    
}

# read formatted data
user_answers <- utils::read.csv(file.path(data_dir,
                                          paste0(Sys.Date(), '-user_answers.csv'))) |>
  dplyr::mutate(include_code = as.logical(include_code))

# get simplified df with per article information
# keep one row per paper
answers_per_paper <- user_answers |>
  dplyr::select(project_id:challenges, title:Book.Author) |>
  dplyr::distinct()
# calculate basic screening information
excluded_papers <- answers_per_paper |>
  dplyr::filter(!include_code)
included_papers <- answers_per_paper |>
  dplyr::filter(include_code)

# get variables to print
n_coding <- nrow(answers_per_paper)
n_included <- nrow(included_papers)
n_excluded <- nrow(excluded_papers)

# get exclusion reasons
exclusion_reasons <- excluded_papers |>
  dplyr::count(exclude_note) |>
  dplyr::arrange(n)


# Automatic correction: impact/lever/challenges
included_papers$impact_evaluation[
  which(is.na(included_papers$impact_evaluation))] <- 'No'
included_papers$levers[
  which(is.na(included_papers$levers))] <- 'No'
included_papers$challenges[
  which(is.na(included_papers$challenges))] <- 'No'
# Boolean flags at article level: TRUE if a definition is provided, i.e. the
# coded value is not NA / "none" / "no" (case- and whitespace-insensitive).
included_papers <- included_papers |>
  dplyr::mutate(
    dplyr::across(
      c(participant_definition, approach_definition),
      ~ !(is.na(.x) | tolower(trimws(.x)) %in% c("none", "no", "")),
      .names = "has_{.col}"
    )
  )


# keep a data frame with only the included papers
included_answers <- user_answers |>
  dplyr::filter(include_code) |>
  dplyr::mutate(participant_type = stringr::str_replace_all(
    participant_type,
    stringr::fixed("Public authorities, decision makers and managers"),
    "Decision makers"
  ))

# Automatic correction: habitats
included_answers <- included_answers |>
  dplyr::mutate(vulnerable_habitat = dplyr::case_when(
    grepl('Land', ecosystem_main) &
      !grepl('Other littoral', vulnerable_habitat) &
      grepl('None', vulnerable_habitat) ~
      'Other littoral ecosystems',
    grepl('Land', ecosystem_main) &
      is.na(vulnerable_habitat) ~ 'Other littoral ecosystems',
    
    
    grepl('Coastal', ecosystem_main) &
      !grepl('Other continental', vulnerable_habitat) &
      grepl('None', vulnerable_habitat) ~
      'Other continental shelf ecosystems',
    grepl('Coastal', ecosystem_main) &
      is.na(vulnerable_habitat) ~ 'Other continental shelf ecosystems',
    
    T ~ vulnerable_habitat
  )) |>
  # Automatic correction : when inclusivity is empty, put None
  dplyr::mutate(participant_inclusivity = dplyr::case_when(
    is.na(participant_inclusivity) ~ 'None',
    T ~ participant_inclusivity
  )) |>
  # automatic correction when involvement_iteration is NA put FALSE
  dplyr::mutate(
    involvement_iteration = ifelse(is.na(involvement_iteration),
                                   FALSE,
                                   involvement_iteration)
  )

# one line per case study (do not consider participant information in this df)
case_study_data <- included_answers |>
  dplyr::distinct(article_id, user_id, case_study_index, .keep_all = T)

# participant level data
participant_data <- included_answers

# Data for the maps
# Counts are computed on case_study_data (one row per case study) so that
# case studies with several participants are not counted several times.
# One row per case study x coded location.
case_study_locations <- case_study_data |>
  dplyr::select(article_id, user_id, case_study_index, study_site_location) |>
  tidyr::separate_longer_delim(study_site_location, delim = ';') |>
  dplyr::mutate(
    study_site_location = stringr::str_trim(study_site_location),
    # codes are entered with inconsistent zero-padding ("090" vs "90"), so
    # they are compared numerically; codes longer than 3 digits (e.g. "0055")
    # are ambiguous and set to NA so that they are flagged as wrong
    location = dplyr::if_else(
      grepl('^[0-9]{1,3}$', study_site_location),
      suppressWarnings(as.numeric(study_site_location)),
      NA_real_
    )
  )

# Only codes listed in the reference table are kept. Rejected lines (including
# missing locations) are saved for checking/correction in SysRev.
valid_location_codes <- utils::read.csv(file.path(data_dir, 'location_code.csv'))$code
wrong_location_codes <- case_study_locations |>
  dplyr::filter(!location %in% valid_location_codes)
saveRDS(wrong_location_codes, file.path(output_dir, 'wrong_location_codes.rds'))

# Exact matching on the numeric code (not grepl), so that e.g. 90 does not
# also count 290 or 190. Each case study is counted once per location.
count_data <- data.frame(location = 0:400) |>
  dplyr::left_join(
    case_study_locations |>
      dplyr::filter(location %in% valid_location_codes) |>
      dplyr::distinct(article_id, user_id, case_study_index, location) |>
      dplyr::count(location),
    by = 'location'
  ) |>
  dplyr::mutate(location = as.character(location))

# categories for the picker_part_type input
participant_list <- lapply(
  strsplit(participant_data$participant_type, ';'),
  stringr::str_trim
)
participant_choices <- sort(
  unique(unlist(participant_list))
)


