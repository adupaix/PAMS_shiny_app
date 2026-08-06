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
      group_label_fun = sysrev_develop_group_labels_pams
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
  dplyr::select(project_id:challenges) |>
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


# keep a data frame with only the included papers
included_answers <- user_answers |>
  dplyr::filter(include_code)

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
  ))

# one line per case study (do not consider participant information in this df)
case_study_data <- included_answers |>
  dplyr::distinct(article_id, user_id, case_study_index, .keep_all = T)

# participant level data
participant_data <- included_answers

# Data for the maps
location_list <- lapply(
  strsplit(included_answers$study_site_location, ';'),
  stringr::str_trim
)
location_categories <- sort(
  unique(unlist(location_list))
)

count_data <- data.frame(location = 0:400) |>
  dplyr::left_join(data.frame(location = as.numeric(location_categories),
                              n = mapply(function(x) sum(
                                grepl(x, unlist(location_list))),
                                location_categories)),
                   by = 'location') |>
  dplyr::mutate(location = as.character(location))
