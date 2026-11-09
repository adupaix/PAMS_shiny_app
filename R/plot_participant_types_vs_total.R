#' @title Scatter plot of the number of participant types vs participants
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study. For each case study, number of distinct
#' participant types coded in its participant groups (x) against its total
#' number of participants (y, log scale). The total is `participant_nb_total`
#' when coded, otherwise the sum of `participant_nb` over the participant
#' groups. Case studies without a total or without a participant type are
#' dropped.
#'
#' @param participant_data data frame with one row per participant group and
#'   the columns `project_id`, `article_id`, `user_id`, `case_study_index`,
#'   `participant_type` (";"-separated values), `participant_nb_total` and
#'   `participant_nb`
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_participant_types_vs_total <- function(participant_data) {
  stopifnot(is.data.frame(participant_data),
            all(c("project_id", "article_id", "user_id", "case_study_index",
                  "participant_type", "participant_nb_total",
                  "participant_nb") %in% names(participant_data)))

  participant_case_data <- participant_data |>
    dplyr::mutate(
      case_unit_id = interaction(.data$project_id, .data$article_id,
                                 .data$user_id, .data$case_study_index,
                                 drop = TRUE),
      participant_nb_total = suppressWarnings(
        as.numeric(.data$participant_nb_total)
      ),
      participant_nb = suppressWarnings(as.numeric(.data$participant_nb))
    )

  # Number of distinct participant types per case study
  case_participant_types <- participant_case_data |>
    dplyr::select(dplyr::all_of(c("case_unit_id", "participant_type"))) |>
    tidyr::separate_rows("participant_type", sep = ";") |>
    dplyr::mutate(participant_type = stringr::str_trim(.data$participant_type)) |>
    dplyr::filter(!is.na(.data$participant_type),
                  .data$participant_type != "",
                  .data$participant_type != "NA") |>
    dplyr::distinct() |>
    dplyr::count(.data$case_unit_id, name = "n_participant_types")

  # Total number of participants per case study: coded total, otherwise sum
  # over the participant groups
  case_participant_totals <- participant_case_data |>
    dplyr::group_by(.data$case_unit_id) |>
    dplyr::summarise(
      total_from_field = dplyr::first(
        stats::na.omit(.data$participant_nb_total), default = NA_real_
      ),
      total_from_categories = if (all(is.na(.data$participant_nb))) {
        NA_real_
      } else {
        sum(.data$participant_nb, na.rm = TRUE)
      },
      total_participants = dplyr::if_else(!is.na(.data$total_from_field),
                                          .data$total_from_field,
                                          .data$total_from_categories),
      .groups = "drop"
    )

  participant_scatter_data <- case_participant_totals |>
    dplyr::inner_join(case_participant_types, by = "case_unit_id") |>
    dplyr::filter(is.finite(.data$total_participants),
                  !is.na(.data$n_participant_types))

  ggplot2::ggplot(participant_scatter_data,
                  ggplot2::aes(x = .data$n_participant_types,
                               y = .data$total_participants)) +
    ggplot2::geom_point(alpha = 0.7, color = "#287C8E", size = 2) +
    ggplot2::scale_y_continuous(
      transform = "log10",
      labels = scales::label_number(big.mark = ",")
    ) +
    ggplot2::labs(x = "Number of participant-type categories",
                  y = "Total number of participants") +
    ggplot2::theme_minimal()
}
