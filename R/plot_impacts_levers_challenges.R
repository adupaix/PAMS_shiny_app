#' @title Bar plot of the impact evaluation, levers and challenges
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = included article (one row of `included_papers`). For each
#' variable (`impact_evaluation`, `levers`, `challenges`), stacked bar of the
#' coded depth levels, merged two by two with [merge_depth_levels()].
#' Missing values are set to "No" in scripts/load_sysrev_data.R, so every
#' included article is counted once per variable.
#'
#' @param included_papers data frame with one row per included article and
#'   the columns `impact_evaluation`, `levers` and `challenges`
#' @param as_percent if `TRUE`, plot the percentage of included articles
#'   (the number of articles is written in each bar segment); if `FALSE`,
#'   plot the number of articles
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_impacts_levers_challenges <- function(included_papers,
                                           as_percent = TRUE) {
  depth_vars <- c("impact_evaluation", "levers", "challenges")
  stopifnot(is.data.frame(included_papers),
            all(depth_vars %in% names(included_papers)))

  # Number of articles per variable x merged depth level
  impact_lever_chal <- dplyr::bind_rows(lapply(depth_vars, function(var) {
    tibble::tibble(var = var,
                   value_plot = merge_depth_levels(included_papers[[var]])) |>
      dplyr::count(.data$var, .data$value_plot)
  })) |>
    dplyr::group_by(.data$var) |>
    dplyr::mutate(percent = 100 * .data$n / sum(.data$n)) |>
    dplyr::ungroup()

  if (as_percent) {
    value_layers <- list(
      ggplot2::aes(y = .data$percent),
      ggplot2::geom_col(),
      ggplot2::geom_text(ggplot2::aes(label = .data$n),
                         position = ggplot2::position_stack(vjust = 0.5),
                         size = 3),
      ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1),
                                  limits = c(0, NA)),
      ggplot2::ylab("Included articles (%)")
    )
  } else {
    value_layers <- list(
      ggplot2::aes(y = .data$n),
      ggplot2::geom_col(),
      ggplot2::ylab("Number of articles")
    )
  }

  ggplot2::ggplot(impact_lever_chal,
                  ggplot2::aes(x = .data$var, fill = .data$value_plot)) +
    value_layers +
    # RColorBrewer "Blues" (sequential)
    ggplot2::scale_fill_brewer(name = "Coded\nvalue", palette = "Blues") +
    ggplot2::coord_flip() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey"),
      axis.title.y = ggplot2::element_blank()
    )
}
