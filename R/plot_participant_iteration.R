#' @title Bar plot of the iterative involvement per participant type
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = participant group (one row of `participant_data`). For each
#' participant type, percentage of the participant groups including that type
#' (exact match, see [count_category_occurrences()]) whose involvement is
#' iterative (`involvement_iteration` is `TRUE`). Types coded in no group are
#' shown at 0 %.
#'
#' @param participant_data data frame with one row per participant group,
#'   a character column `participant_type` (";"-separated values) and a
#'   logical column `involvement_iteration`
#' @param categories participant types to plot, in display order (top to
#'   bottom)
#' @param label_width maximum number of characters per line of the category
#'   labels (longer labels are wrapped)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_participant_iteration <- function(participant_data,
                                       categories = participant_type_levels,
                                       label_width = 20) {
  stopifnot(is.data.frame(participant_data),
            all(c("participant_type", "involvement_iteration") %in%
                  names(participant_data)))

  # NOTE: a missing involvement_iteration counts as not iterative
  # (scripts/load_sysrev_data.R already sets NA to FALSE)
  is_iterative <- participant_data$involvement_iteration %in% TRUE

  # Participant groups per type: all, and iterative only
  nb_part_type <- count_category_occurrences(
    participant_data$participant_type, categories
  )
  nb_iterative <- count_category_occurrences(
    participant_data$participant_type[is_iterative], categories
  )
  part_type_iter_df <- tibble::tibble(
    part_type = categories,
    nb_part_type = unname(nb_part_type),
    nb_iterative = unname(nb_iterative),
    pct_iterative = ifelse(nb_part_type == 0, 0,
                           100 * nb_iterative / nb_part_type)
  )

  # coord_flip() draws the first level at the bottom: reverse the limits so
  # that `categories` reads top to bottom
  ggplot2::ggplot(part_type_iter_df,
                  ggplot2::aes(x = .data$part_type,
                               y = .data$pct_iterative)) +
    ggplot2::geom_col(fill = "grey") +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(categories),
                              labels = scales::label_wrap(label_width)) +
    ggplot2::xlab("Participant type") +
    ggplot2::ylab("% of iterative involvement") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey"),
      legend.position = "none"
    )
}
