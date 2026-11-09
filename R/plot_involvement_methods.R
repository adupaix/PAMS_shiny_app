#' @title Bar plot of the involvement methods
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = participant group (one row of `participant_data`). For each
#' involvement method, percentage of the participant groups with at least
#' one method coded that use it. A group can be coded with several methods,
#' so percentages may sum to more than 100 %. Bars are sorted by number of
#' participant groups (most numerous on top).
#'
#' @param participant_data data frame with one row per participant group and
#'   a character column `involvement_method` (";"-separated values)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_involvement_methods <- function(participant_data) {
  stopifnot(is.data.frame(participant_data),
            "involvement_method" %in% names(participant_data))

  # One row per participant group x method
  involvement_method_long <- tibble::tibble(
    participant_group_id = seq_len(nrow(participant_data)),
    involvement_method = participant_data$involvement_method
  ) |>
    tidyr::separate_rows("involvement_method", sep = ";") |>
    dplyr::mutate(involvement_method = stringr::str_trim(
      .data$involvement_method
    )) |>
    dplyr::filter(!is.na(.data$involvement_method),
                  .data$involvement_method != "",
                  .data$involvement_method != "NA") |>
    dplyr::distinct()

  # Denominator = participant groups with at least one method coded
  n_groups_with_method <- dplyr::n_distinct(
    involvement_method_long$participant_group_id
  )
  involvement_method_counts <- involvement_method_long |>
    dplyr::count(.data$involvement_method, name = "n_participant_groups") |>
    dplyr::mutate(percent = 100 * .data$n_participant_groups /
                    n_groups_with_method)

  ggplot2::ggplot(involvement_method_counts,
                  ggplot2::aes(x = stats::reorder(.data$involvement_method,
                                                  .data$n_participant_groups),
                               y = .data$percent)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1), limits = c(0, NA)
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Involvement method", y = "Participant groups (%)") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey")
    )
}
