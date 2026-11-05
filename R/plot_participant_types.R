#' @title Bar plot of the participant types
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#' 
#' @description
#' Coding unit = participant group (one row of `participant_data`). For each
#' participant type, counts the participant groups whose `participant_type`
#' contains it (exact match, see [count_category_occurrences()]). A group
#' coded with several types counts once under each, so bars can sum to more
#' than the number of groups (or to more than 100 %).
#'
#' @param participant_data data frame with one row per participant group and
#'   a character column `participant_type` (";"-separated values)
#' @param categories participant types to plot, in display order (top to
#'   bottom)
#' @param other_categories categories drawn in grey instead of blue
#' @param as_percent if `TRUE`, plot the percentage of participant groups
#'   with at least one participant type coded (groups with a missing
#'   `participant_type` are excluded from the denominator); if `FALSE`, plot
#'   the number of participant groups
#' @param label_width maximum number of characters per line of the category
#'   labels (longer labels are wrapped)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_participant_types <- function(participant_data,
                                   categories = participant_type_levels,
                                   other_categories = participant_type_other,
                                   as_percent = FALSE,
                                   label_width = 20) {
  stopifnot(is.data.frame(participant_data),
            "participant_type" %in% names(participant_data))

  # Count participant groups per type
  n_participant_groups <- count_category_occurrences(
    participant_data$participant_type, categories
  )
  part_type_df <- tibble::tibble(
    part_type = categories,
    color = ifelse(categories %in% other_categories, "Other", "Known"),
    nb_part_type = unname(n_participant_groups)
  )

  # Value plotted: count, or % of participant groups with a type coded
  if (as_percent) {
    n_groups_with_type <- sum(!is.na(participant_data$participant_type) &
                                trimws(participant_data$participant_type) != "")
    part_type_df$value <- 100 * part_type_df$nb_part_type / n_groups_with_type
    y_scale <- ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1), limits = c(0, NA)
    )
    y_label <- "Participant groups (%)"
  } else {
    part_type_df$value <- part_type_df$nb_part_type
    y_scale <- NULL
    y_label <- "Number of participant groups"
  }

  # coord_flip() draws the first level at the bottom: reverse the limits so
  # that `categories` reads top to bottom
  ggplot2::ggplot(part_type_df,
                  ggplot2::aes(x = .data$part_type, y = .data$value,
                               fill = .data$color)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(categories),
                              labels = scales::label_wrap(label_width)) +
    y_scale +
    ggplot2::scale_fill_manual("", values = c(Known = "#08519C",
                                              Other = "grey")) +
    ggplot2::xlab("Participant type") +
    ggplot2::ylab(y_label) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey"),
      legend.position = "none"
    )
}
