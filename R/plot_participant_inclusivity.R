#' @title Radar plot of the participant inclusivity
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = participant group (one row of `participant_data`). For each
#' inclusivity category, percentage of all participant groups whose
#' `participant_inclusivity` contains it (exact match, see
#' [count_category_occurrences()]). A group can be coded with several
#' categories, so percentages can sum to more than 100 %.
#'
#' NOTE: ggradar::ggradar() expects values between `grid.min` (0) and
#' `grid.max` (1). The percentages (0-100) are above `grid.max`, so ggradar
#' divides them by their maximum (with a warning): the outer grid circle
#' corresponds to the largest percentage, not to 100 %. The exact
#' percentages are written in the axis labels.
#'
#' @param participant_data data frame with one row per participant group and
#'   a character column `participant_inclusivity` (";"-separated values)
#' @param categories inclusivity categories to plot, in clockwise order
#' @return a ggplot object
#' @export
plot_participant_inclusivity <- function(participant_data,
                                         categories = inclusivity_levels) {
  stopifnot(is.data.frame(participant_data),
            "participant_inclusivity" %in% names(participant_data))

  # % of all participant groups (NA inclusivity is set to "None" in
  # scripts/load_sysrev_data.R, so every group is coded)
  pct_groups <- 100 * count_category_occurrences(
    participant_data$participant_inclusivity, categories
  ) / nrow(participant_data)

  # ggradar format: one row per group, first column = group name
  radar_data <- data.frame(group = 1, as.list(pct_groups),
                           check.names = FALSE)
  names(radar_data) <- c("group",
                         paste0(categories, "\n", round(pct_groups), "%"))

  ggradar::ggradar(radar_data,
                   group.point.size = 1,
                   group.line.width = 1,
                   axis.label.size = 3,
                   background.circle.colour = "white",
                   gridline.mid.colour = "grey")
}
