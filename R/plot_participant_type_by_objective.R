#' @title Heatmap of the case studies per participant type and objective
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study x participant type x engagement objective.
#' Objectives are coded at article level: each case study inherits the
#' objectives of its article. Several participant groups of the same type
#' in a case study count once.
#'
#' NOTE: unlike the engagement objectives bar plots, the objectives are not
#' corrected with [format_and_correct_objectives()] ("Unclear" is kept when
#' other objectives are coded).
#'
#' @param participant_data data frame with one row per participant group and
#'   the columns `article_id`, `user_id`, `case_study_index`,
#'   `participant_type` and `engagement_objectives` (";"-separated values for
#'   the last two)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_participant_type_by_objective <- function(participant_data) {
  case_study_cols <- c("article_id", "user_id", "case_study_index")
  stopifnot(is.data.frame(participant_data),
            all(c(case_study_cols, "participant_type",
                  "engagement_objectives") %in% names(participant_data)))

  # Case studies per participant type x objective
  participant_objective_counts <- participant_data |>
    dplyr::select(dplyr::all_of(c(case_study_cols, "participant_type",
                                  "engagement_objectives"))) |>
    dplyr::rename(engagement_objective = "engagement_objectives") |>
    tidyr::separate_rows("participant_type", sep = ";") |>
    tidyr::separate_rows("engagement_objective", sep = ";") |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("participant_type", "engagement_objective")),
      stringr::str_trim
    )) |>
    dplyr::filter(!is.na(.data$participant_type),
                  .data$participant_type != "",
                  .data$participant_type != "NA",
                  !is.na(.data$engagement_objective),
                  .data$engagement_objective != "",
                  .data$engagement_objective != "NA") |>
    dplyr::distinct() |>
    dplyr::count(.data$participant_type, .data$engagement_objective,
                 name = "n_case_studies")

  ggplot2::ggplot(participant_objective_counts,
                  ggplot2::aes(x = .data$participant_type,
                               y = .data$engagement_objective,
                               fill = .data$n_case_studies)) +
    ggplot2::geom_tile(color = "white") +
    # viridis (colour-blind friendly)
    ggplot2::scale_fill_viridis_c(name = "Case studies", limits = c(0, NA)) +
    ggplot2::scale_x_discrete() +
    ggplot2::labs(x = "Participant type", y = "Engagement objectives") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1))
}
