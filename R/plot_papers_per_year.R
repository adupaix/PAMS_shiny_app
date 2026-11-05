#' @title Bar plot of the number of papers per publication year
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = included article (one row of `included_papers`). Number of
#' included articles per publication year (articles without a year are
#' dropped). If `fill_var` is given, bars are stacked by the values of that
#' article-level variable: the depth variables (levels = `depth_levels`) are
#' merged with [merge_depth_levels()] and coloured with the "Blues" palette,
#' the other variables use the viridis palette. Articles with a missing
#' `fill_var` are dropped.
#'
#' @param included_papers data frame with one row per included article, a
#'   column `Publication.Year` and, if used, the column `fill_var`
#' @param fill_var `NULL` (single colour) or the name of a variable of
#'   `choices` used to stack the bars
#' @param choices named character vector of the variables that can be used
#'   as `fill_var` (names = legend titles)
#' @param var_levels named list of the level order of each variable of
#'   `choices`; values absent from it are appended at the end
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_papers_per_year <- function(included_papers,
                                 fill_var = NULL,
                                 choices = area_choices,
                                 var_levels = area_levels) {
  stopifnot(is.data.frame(included_papers),
            "Publication.Year" %in% names(included_papers),
            is.null(fill_var) || fill_var %in% choices)

  axis_layers <- list(
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()),
    ggplot2::xlab("Publication year"),
    ggplot2::ylab("Number of papers"),
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey")
    )
  )

  # Single colour: number of articles per year
  if (is.null(fill_var)) {
    year_counts <- tibble::tibble(
      publication_year = suppressWarnings(
        as.integer(included_papers$Publication.Year)
      )
    ) |>
      dplyr::filter(!is.na(.data$publication_year)) |>
      dplyr::count(.data$publication_year, name = "n_articles")

    return(
      ggplot2::ggplot(year_counts,
                      ggplot2::aes(x = .data$publication_year,
                                   y = .data$n_articles)) +
        ggplot2::geom_col(fill = "#287C8E") +
        axis_layers
    )
  }

  # Stacked by fill_var: level order and palette depend on the variable type
  fill_raw <- as.character(included_papers[[fill_var]])
  fill_title <- names(choices)[match(fill_var, choices)]
  if (identical(var_levels[[fill_var]], depth_levels)) {
    fill_value <- merge_depth_levels(fill_raw)
    fill_levels <- unique(c("No", "Detailed/Evaluated", "Mentioned/Discussed",
                            sort(unique(stats::na.omit(fill_value)))))
    # RColorBrewer "Blues" (sequential)
    fill_scale <- ggplot2::scale_fill_brewer(fill_title, palette = "Blues",
                                             drop = FALSE)
  } else {
    fill_value <- fill_raw
    # NOTE: unexpected values are appended to the expected levels instead of
    # being silently coerced to NA
    fill_levels <- unique(c(var_levels[[fill_var]],
                            sort(unique(stats::na.omit(fill_raw)))))
    # viridis (colour-blind friendly), reversed
    fill_scale <- ggplot2::scale_fill_viridis_d(fill_title, direction = -1)
  }

  year_counts <- tibble::tibble(
    Publication.Year = suppressWarnings(
      as.numeric(included_papers$Publication.Year)
    ),
    fill_value = factor(fill_value, levels = fill_levels)
  ) |>
    dplyr::filter(!is.na(.data$Publication.Year), !is.na(.data$fill_value)) |>
    dplyr::count(.data$Publication.Year, .data$fill_value)

  ggplot2::ggplot(year_counts,
                  ggplot2::aes(x = .data$Publication.Year, y = .data$n,
                               fill = .data$fill_value)) +
    ggplot2::geom_col(colour = "white", linewidth = 0.2) +
    fill_scale +
    axis_layers
}
