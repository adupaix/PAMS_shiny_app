#' @title Heatmap of the involvement methods per research step
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = participant group x involvement method x research step.
#' Methods and steps are not linked in the coding book, so a participant
#' group with several methods and several steps contributes every pair.
#'
#' @param participant_data data frame with one row per participant group and
#'   the columns `involvement_method` and `research_step` (";"-separated
#'   values)
#' @param step_order research steps kept, in x-axis order
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_method_by_research_step <- function(participant_data,
                                         step_order = research_steps) {
  stopifnot(is.data.frame(participant_data),
            all(c("involvement_method", "research_step") %in%
                  names(participant_data)))

  # Participant groups per method x step (pairs crossed within each group)
  method_step_counts <- tibble::tibble(
    participant_group_id = seq_len(nrow(participant_data)),
    involvement_method = participant_data$involvement_method,
    research_step = participant_data$research_step
  ) |>
    tidyr::separate_rows("involvement_method", sep = ";") |>
    tidyr::separate_rows("research_step", sep = ";") |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("involvement_method", "research_step")),
      stringr::str_trim
    )) |>
    dplyr::filter(!is.na(.data$involvement_method),
                  .data$involvement_method != "",
                  .data$involvement_method != "NA",
                  .data$research_step %in% step_order) |>
    dplyr::distinct() |>
    dplyr::count(.data$involvement_method, .data$research_step,
                 name = "n_participant_groups") |>
    dplyr::mutate(research_step = factor(.data$research_step,
                                         levels = step_order))

  ggplot2::ggplot(method_step_counts,
                  ggplot2::aes(x = .data$research_step,
                               y = .data$involvement_method,
                               fill = .data$n_participant_groups)) +
    ggplot2::geom_tile(color = "white") +
    # viridis (colour-blind friendly)
    ggplot2::scale_fill_viridis_c(name = "Participant groups",
                                  limits = c(0, NA)) +
    ggplot2::labs(x = "Research step", y = "Involvement method") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1))
}
