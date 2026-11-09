#' @title Bar plot of the participant selection methods
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = article x selection method: each article counts once per
#' selection method coded in any of its participant groups. Percentages are
#' relative to the number of article x method pairs (they sum to 100 %);
#' an article coded with several methods counts once under each.
#'
#' @param participant_data data frame with one row per participant group and
#'   the columns `project_id`, `article_id` and `selection_method`
#'   (";"-separated values)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_selection_method <- function(participant_data) {
  stopifnot(is.data.frame(participant_data),
            all(c("project_id", "article_id", "selection_method") %in%
                  names(participant_data)))

  # Articles per selection method
  selection_method_counts <- participant_data |>
    dplyr::select(dplyr::all_of(c("project_id", "article_id",
                                  "selection_method"))) |>
    tidyr::separate_rows("selection_method", sep = ";") |>
    dplyr::mutate(selection_method = stringr::str_trim(
      .data$selection_method
    )) |>
    dplyr::filter(!is.na(.data$selection_method),
                  .data$selection_method != "",
                  .data$selection_method != "NA") |>
    dplyr::distinct() |>
    dplyr::count(.data$selection_method) |>
    dplyr::mutate(var = "selection_method",
                  percent = 100 * .data$n / sum(.data$n, na.rm = TRUE))

  ggplot2::ggplot(selection_method_counts,
                  ggplot2::aes(x = .data$var, y = .data$percent,
                               fill = .data$selection_method)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(.data$n, "\n(", round(.data$percent),
                                  "%)")),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3
    ) +
    # RColorBrewer "Blues" (sequential)
    ggplot2::scale_fill_brewer(name = "Selection\nmethod", palette = "Blues") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1), limits = c(0, NA)
    ) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Papers (%)") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey"),
      axis.title.y = ggplot2::element_blank()
    )
}
