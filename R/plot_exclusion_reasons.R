#' @title Bar plot of the exclusion reasons
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = excluded article (one row of `excluded_papers`). For each
#' exclusion reason, percentage of the excluded articles coded with it. One
#' article can have several ";"-separated reasons, so percentages are
#' relative to the number of excluded articles and may sum to more than
#' 100 %. Missing reasons (NA) are kept on purpose, as an "NA" bar, so that
#' missing `exclude_note` values stay visible.
#'
#' @param excluded_papers data frame with one row per excluded article and
#'   the columns `project_id`, `article_id` and `exclude_note`
#'   (";"-separated values)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_exclusion_reasons <- function(excluded_papers) {
  stopifnot(is.data.frame(excluded_papers),
            all(c("project_id", "article_id", "exclude_note") %in%
                  names(excluded_papers)))

  # One row per article x reason, then % of excluded articles per reason
  exclusion_reasons <- excluded_papers |>
    dplyr::select(dplyr::all_of(c("project_id", "article_id",
                                  "exclude_note"))) |>
    tidyr::separate_rows("exclude_note", sep = ";") |>
    dplyr::mutate(exclude_note = stringr::str_trim(.data$exclude_note)) |>
    dplyr::filter(is.na(.data$exclude_note) | .data$exclude_note != "") |>
    dplyr::distinct() |>
    dplyr::count(.data$exclude_note, name = "n_articles") |>
    dplyr::mutate(percent = 100 * .data$n_articles / nrow(excluded_papers))

  ggplot2::ggplot(exclusion_reasons,
                  ggplot2::aes(x = .data$exclude_note, y = .data$percent,
                               fill = .data$exclude_note)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%",
                                                    .data$percent)),
                       position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1), limits = c(0, NA)
    ) +
    ggplot2::labs(x = "Exclusion reason", y = "Excluded articles (%)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}
