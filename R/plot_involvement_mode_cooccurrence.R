#' @title Heatmap of the co-occurrence of the involvement modes
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = participant group (one row of `participant_data`). Diagonal
#' = number of participant groups coded with the mode; off-diagonal = number
#' of participant groups coded with both modes.
#'
#' @param participant_data data frame with one row per participant group and
#'   a character column `involvement_mode` (";"-separated values)
#' @param mode_order involvement modes, in axis order (modes absent from it
#'   are shown as NA)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_involvement_mode_cooccurrence <- function(
    participant_data, mode_order = involvement_mode_levels) {
  stopifnot(is.data.frame(participant_data),
            "involvement_mode" %in% names(participant_data))

  # One row per participant group x mode
  involvement_mode_long <- tibble::tibble(
    participant_group_id = seq_len(nrow(participant_data)),
    involvement_mode = participant_data$involvement_mode
  ) |>
    tidyr::separate_rows("involvement_mode", sep = ";") |>
    dplyr::mutate(involvement_mode = stringr::str_trim(
      .data$involvement_mode
    )) |>
    dplyr::filter(!is.na(.data$involvement_mode),
                  .data$involvement_mode != "",
                  .data$involvement_mode != "NA") |>
    dplyr::distinct()

  # Pairs of modes within each participant group (self-join)
  involvement_mode_pairs <- involvement_mode_long |>
    dplyr::inner_join(involvement_mode_long, by = "participant_group_id",
                      suffix = c("_1", "_2"),
                      relationship = "many-to-many") |>
    dplyr::count(.data$involvement_mode_1, .data$involvement_mode_2,
                 name = "n_participant_groups") |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("involvement_mode_1", "involvement_mode_2")),
      function(x) factor(x, levels = mode_order)
    ))

  ggplot2::ggplot(involvement_mode_pairs,
                  ggplot2::aes(x = .data$involvement_mode_1,
                               y = .data$involvement_mode_2,
                               fill = .data$n_participant_groups)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data$n_participant_groups),
                       size = 3, color = "white") +
    # viridis (colour-blind friendly)
    ggplot2::scale_fill_viridis_c(name = "Participant groups",
                                  limits = c(0, NA)) +
    ggplot2::labs(x = "Involvement mode", y = "Involvement mode") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1))
}
