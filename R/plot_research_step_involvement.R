#' @title Bar plot of the research steps by involvement level
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study x research step x involvement level: several
#' participant types or groups with the same step and level in a case study
#' count once. Steps and levels are not linked in the coding book, so a
#' participant group with several steps and several levels contributes every
#' step x level pair. Participant groups with a "No implication" or missing
#' involvement level are excluded.
#'
#' @param participant_data data frame with one row per participant group and
#'   the columns `article_id`, `user_id`, `case_study_index`,
#'   `participant_type`, `research_step` and `involvement_level`
#'   (";"-separated values for the last three)
#' @param part_types participant types to keep (a case study counts if any of
#'   them is involved at that step and level); groups with a missing
#'   `participant_type` are always excluded
#' @param level_order involvement levels, in legend order
#' @param step_order research steps, in axis order
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_research_step_involvement <- function(participant_data,
                                           part_types = participant_type_levels,
                                           level_order = involvement_levels,
                                           step_order = research_steps) {
  case_study_cols <- c("article_id", "user_id", "case_study_index")
  stopifnot(is.data.frame(participant_data),
            all(c(case_study_cols, "participant_type", "research_step",
                  "involvement_level") %in% names(participant_data)))

  # NOTE: filter() also drops the groups with a missing involvement_level
  step_level_data <- participant_data |>
    dplyr::filter(.data$involvement_level != "No implication") |>
    dplyr::select(dplyr::all_of(c(case_study_cols, "participant_type",
                                  "research_step", "involvement_level"))) |>
    split_duplicate_rows(split_col = "participant_type", sep = ";") |>
    # filter on participant type BEFORE deduplicating
    dplyr::filter(.data$participant_type %in% part_types) |>
    # cross step x level within each participant group, then deduplicate
    split_duplicate_rows(split_col = "research_step", sep = ";") |>
    split_duplicate_rows(split_col = "involvement_level", sep = ";") |>
    dplyr::select(dplyr::all_of(c(case_study_cols, "research_step",
                                  "involvement_level"))) |>
    dplyr::distinct() |>
    dplyr::mutate(
      involvement_level = factor(.data$involvement_level,
                                 levels = level_order),
      research_step = factor(.data$research_step, levels = step_order)
    )

  ggplot2::ggplot(step_level_data,
                  ggplot2::aes(x = .data$research_step,
                               fill = .data$involvement_level)) +
    ggplot2::geom_bar() +
    # viridis palette (colour-blind friendly), reversed: dark = high level
    ggplot2::scale_fill_viridis_d("Involvement level", direction = -1) +
    ggplot2::xlab("Research step") +
    ggplot2::ylab("Number of case study \u00d7 involvement level") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey")
    )
}
