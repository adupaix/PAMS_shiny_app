#' @title Tile and pie plot of participant type x involvement level x step
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study x participant type x involvement level (tiles)
#' and x research step (pie slices): several participant groups of the same
#' type with the same level and step in a case study count once. Steps and
#' levels are not linked in the coding book, so a participant group with
#' several steps and several levels contributes every pair. Groups with a
#' missing or "No implication" involvement level are excluded.
#'
#' Each tile (participant type x involvement level) is shaded by its number
#' of case studies and holds a pie of the research steps.
#'
#' @param participant_data data frame with one row per participant group and
#'   the columns `article_id`, `user_id`, `case_study_index`,
#'   `participant_type`, `involvement_level` and `research_step`
#'   (";"-separated values for the last three)
#' @param level_order involvement levels kept, in x-axis order
#' @param step_order research steps kept, in pie order
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_participant_involvement_pies <- function(participant_data,
                                              level_order = involvement_levels,
                                              step_order = research_steps) {
  case_study_cols <- c("article_id", "user_id", "case_study_index")
  stopifnot(is.data.frame(participant_data),
            all(c(case_study_cols, "participant_type", "involvement_level",
                  "research_step") %in% names(participant_data)))

  # One row per case study x participant type x level x step (cross step x
  # level within each participant group, then deduplicate)
  participant_units <- participant_data |>
    dplyr::filter(!is.na(.data$involvement_level),
                  .data$involvement_level != "No implication") |>
    dplyr::select(dplyr::all_of(c(case_study_cols, "participant_type",
                                  "involvement_level", "research_step"))) |>
    tidyr::separate_rows("participant_type", sep = ";") |>
    tidyr::separate_rows("involvement_level", sep = ";") |>
    tidyr::separate_rows("research_step", sep = ";") |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("participant_type", "involvement_level",
                      "research_step")),
      stringr::str_trim
    )) |>
    dplyr::filter(!is.na(.data$participant_type),
                  .data$participant_type != "",
                  .data$participant_type != "NA",
                  .data$involvement_level %in% level_order,
                  .data$research_step %in% step_order) |>
    dplyr::distinct()

  # Tile positions: involvement levels on x, participant types (alphabetical,
  # types present in the data only) on y
  type_order <- sort(unique(participant_units$participant_type))
  type_positions <- stats::setNames(seq_along(type_order), type_order)
  level_positions <- stats::setNames(seq_along(level_order), level_order)
  add_positions <- function(data) {
    dplyr::mutate(
      data,
      x_pos = unname(level_positions[.data$involvement_level]),
      y_pos = unname(type_positions[.data$participant_type])
    )
  }

  # Case studies per tile, and per tile x research step (pie slices)
  participant_cell_counts <- participant_units |>
    dplyr::select(dplyr::all_of(c(case_study_cols, "participant_type",
                                  "involvement_level"))) |>
    dplyr::distinct() |>
    dplyr::count(.data$participant_type, .data$involvement_level,
                 name = "n_units") |>
    add_positions()
  participant_pie_counts <- participant_units |>
    dplyr::count(.data$participant_type, .data$involvement_level,
                 .data$research_step, name = "n_units") |>
    add_positions()
  participant_pie_polygons <- make_pie_polygons(participant_pie_counts)

  ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = participant_cell_counts,
      ggplot2::aes(x = .data$x_pos, y = .data$y_pos, fill = .data$n_units),
      color = "white", width = 1, height = 1
    ) +
    ggplot2::scale_fill_gradient(name = "Case studies", low = "white",
                                 high = "grey35", limits = c(0, NA)) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_polygon(
      data = participant_pie_polygons,
      ggplot2::aes(x = .data$x, y = .data$y,
                   group = interaction(.data$x_pos, .data$y_pos,
                                       .data$slice),
                   fill = .data$research_step),
      color = "white", linewidth = 0.2
    ) +
    ggplot2::scale_x_continuous(breaks = seq_along(level_order),
                                labels = level_order,
                                expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_y_continuous(breaks = seq_along(type_order),
                                labels = type_order,
                                expand = ggplot2::expansion(add = 0.5)) +
    # viridis (colour-blind friendly)
    ggplot2::scale_fill_viridis_d(name = "Research step") +
    ggplot2::labs(x = "Involvement level", y = "Participant type") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1))
}

#' @title Build the pie polygons of plot_participant_involvement_pies()
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' One polygon per pie slice: for each tile (participant type x involvement
#' level, centred on `x_pos`, `y_pos`), the research steps are drawn as
#' slices proportional to `n_units`, in alphabetical order of the steps.
#'
#' @param data data frame with the columns `participant_type`,
#'   `involvement_level`, `research_step`, `n_units`, `x_pos` and `y_pos`
#' @param radius pie radius (in tile units)
#' @param points_per_pie number of points used to draw a full circle
#' @return tibble with the polygon coordinates (`x`, `y`), `research_step`,
#'   `slice` and the tile columns
#' @noRd
make_pie_polygons <- function(data, radius = 0.34, points_per_pie = 80) {
  data |>
    dplyr::group_by(.data$participant_type, .data$involvement_level,
                    .data$x_pos, .data$y_pos) |>
    dplyr::group_modify(function(cell, key) {
      cell <- cell |> dplyr::arrange(.data$research_step)
      end_angles <- 2 * pi * cumsum(cell$n_units) / sum(cell$n_units)
      start_angles <- c(0, utils::head(end_angles, -1))
      x_center <- key$x_pos[[1]]
      y_center <- key$y_pos[[1]]
      dplyr::bind_rows(lapply(seq_len(nrow(cell)), function(i) {
        n_points <- max(2, ceiling(points_per_pie *
                                     cell$n_units[i] / sum(cell$n_units)))
        angles <- seq(start_angles[i], end_angles[i], length.out = n_points)
        tibble::tibble(
          x = c(x_center, x_center + radius * cos(angles), x_center),
          y = c(y_center, y_center + radius * sin(angles), y_center),
          research_step = cell$research_step[i],
          slice = i
        )
      }))
    }) |>
    dplyr::ungroup()
}
