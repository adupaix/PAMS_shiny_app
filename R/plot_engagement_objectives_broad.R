#' @title Bar plot of the engagement objectives (broad categories)
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = included article (one row of `included_papers`). The coded
#' objectives of each article are corrected with
#' [format_and_correct_objectives()] and reduced to their broad category
#' (text before "-", e.g. "Substantive - Data collection" -> "Substantive").
#' For each broad category, percentage of the articles with at least one
#' objective coded that include it (exact match). One article can have
#' several objectives, so percentages may sum to more than 100 %. Bars are
#' sorted by number of articles (most numerous on top).
#'
#' @param included_papers data frame with one row per included article and a
#'   character column `engagement_objectives` (";"-separated values)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_engagement_objectives_broad <- function(included_papers) {
  stopifnot(is.data.frame(included_papers),
            "engagement_objectives" %in% names(included_papers))

  # Corrected objectives per article, then their broad category
  objective_list <- lapply(
    strsplit(included_papers$engagement_objectives, ";"),
    format_and_correct_objectives
  )
  main_objective_list <- lapply(objective_list, function(objectives) {
    trimws(vapply(strsplit(objectives, "-"), function(x) x[1], character(1)))
  })
  main_objective_categories <- sort(unique(unlist(main_objective_list)))

  # Denominator = included articles with at least one objective coded
  n_coded_objectives <- sum(vapply(
    objective_list, function(x) any(!is.na(x) & x != ""), logical(1)
  ))
  main_objectives_df <- tibble::tibble(
    objective = main_objective_categories,
    nb_articles = unname(count_category_occurrences(
      main_objective_list, main_objective_categories
    ))
  ) |>
    dplyr::mutate(percent = 100 * .data$nb_articles / n_coded_objectives)

  # objectives sorted by number of articles (most numerous on top);
  # fill uses the unsorted column so that colours stay the same
  ggplot2::ggplot(main_objectives_df,
                  ggplot2::aes(x = stats::reorder(.data$objective,
                                                  .data$nb_articles),
                               y = .data$percent,
                               fill = .data$objective)) +
    ggplot2::geom_col() +
    # RColorBrewer "Set1" (qualitative)
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1), limits = c(0, NA)
    ) +
    ggplot2::coord_flip() +
    ggplot2::xlab("Engagement objective") +
    ggplot2::ylab("Coded articles (%)") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      legend.position = "none",
      panel.grid.major = ggplot2::element_line(color = "grey")
    )
}
