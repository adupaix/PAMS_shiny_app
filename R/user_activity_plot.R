#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2026-11-02
#'@email : amael.dupaix@ird.fr
#'#***************************************************************************
#'@description: function building a bar plot of the number of articles coded
#' per user (Include / Exclude), similar to the SysRev "Member Activity" panel.
#'
#'#***************************************************************************

#' Bar plot of the number of articles coded per user
#'
#' Input: the raw SysRev parquet exports downloaded by scripts/load_sysrev_data.R
#' (data_dir/proj_*/user-answers.parquet and labels.parquet). These are used
#' instead of `user_answers` because sysrev_format_data() keeps only one coder
#' per article (resolver or fallback), which would under-credit double-coding.
#' Here EVERY review counts, as in SysRev.
#'
#' User names: SysRev exports only contain `user_id`. Names are read from
#' `user_names_path` (columns user_id, user_name). If the file does not exist,
#' a template is written with the user_ids and empty names; unmatched ids fall
#' back to the raw user_id in the plot.
#'
#' @param data_dir directory containing sysrev_projects.txt and the proj_*
#'   export folders
#' @param user_names_path csv file mapping user_id to user_name
#' @return a ggplot object
user_activity_plot <- function(data_dir = 'data',
                               user_names_path = file.path(data_dir,
                                                           'user_names.csv')) {

  project_ids <- paste0(
    'proj_',
    as.numeric(readLines(file.path(data_dir, 'sysrev_projects.txt')))
  )

  # ---- Read raw answers ---------------------------------------------------
  # One row per answered label, with the label short name attached so that
  # the inclusion question can be identified across projects (label_ids
  # differ between projects, short_label 'include_code' does not).
  raw_answers <- project_ids |>
    purrr::map(function(project_id) {
      l <- SLRtools::sysrev_read_parquet(project_id, data_dir)
      # projects with no review yet have no user-answers.parquet
      if (is.null(l$user_answers)) return(NULL)
      l$user_answers |>
        dplyr::left_join(
          dplyr::select(l$label, label_id, short_label),
          by = 'label_id'
        ) |>
        dplyr::mutate(project_id = project_id)
    }) |>
    dplyr::bind_rows()

  # ---- One row per (project, article, user) -------------------------------
  # An article counts as coded by a user as soon as that user answered at
  # least one label on it. The decision comes from the include_code boolean
  # answer; articles where it is missing are kept as "No decision" rather
  # than dropped.
  include_answers <- raw_answers |>
    dplyr::filter(short_label == 'include_code') |>
    dplyr::distinct(project_id, article_id, user_id, answer)

  articles_per_user <- raw_answers |>
    dplyr::distinct(project_id, article_id, user_id) |>
    dplyr::left_join(include_answers,
                     by = c('project_id', 'article_id', 'user_id'),
                     relationship = 'one-to-one') |>
    dplyr::mutate(decision = dplyr::case_when(
      answer == 'true' ~ 'Include',
      answer == 'false' ~ 'Exclude',
      TRUE ~ 'No decision'
    ))

  # ---- User names ---------------------------------------------------------
  # Write a template once (never overwrite a filled file)
  if (!file.exists(user_names_path)) {
    data.frame(user_id = sort(unique(articles_per_user$user_id)),
               user_name = '') |>
      utils::write.csv(user_names_path, row.names = FALSE)
    message('Template written to ', user_names_path,
            ': fill the user_name column and re-run.')
  }

  user_names <- utils::read.csv(user_names_path, colClasses = 'character') |>
    dplyr::mutate(user_name = dplyr::na_if(trimws(user_name), ''))

  missing_names <- setdiff(unique(articles_per_user$user_id),
                           user_names$user_id[!is.na(user_names$user_name)])
  if (length(missing_names) > 0) {
    message(length(missing_names), ' user_id(s) without name in ',
            user_names_path, ': ', paste(missing_names, collapse = ', '))
  }

  # ---- Count per user -----------------------------------------------------
  decision_levels <- c('Include', 'Exclude', 'No decision')

  count_per_user <- articles_per_user |>
    dplyr::left_join(user_names, by = 'user_id') |>
    dplyr::mutate(
      user_label = dplyr::coalesce(user_name, user_id),
      decision = factor(decision, levels = decision_levels)
    ) |>
    dplyr::count(user_label, decision) |>
    # order users by total number of coded articles (largest on top)
    dplyr::mutate(total = sum(n), .by = user_label) |>
    dplyr::mutate(user_label = forcats::fct_reorder(user_label, total))

  # ---- Plot ---------------------------------------------------------------
  decision_colours <- c('Include' = '#8BDBA1',
                        'Exclude' = '#F9B58B',
                        'No decision' = 'grey70')

  count_per_user |>
    ggplot2::ggplot(ggplot2::aes(
      x = n, y = user_label, fill = decision,
      # tooltip text, used when converted with plotly::ggplotly()
      text = paste0(user_label, '<br>', decision, ': ', n,
                    '<br>Total: ', total)
    )) +
    # reverse stacking so that Include sits on the left, as in SysRev
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE),
                      width = 0.7) +
    ggplot2::scale_fill_manual(values = decision_colours, drop = TRUE) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(big.mark = ' '),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = 'top',
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
}
