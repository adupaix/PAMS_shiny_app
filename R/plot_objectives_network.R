#' @title Co-occurrence network of the engagement objectives
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = included article (one row of `included_papers`). The coded
#' objectives of each article are corrected with
#' [format_and_correct_objectives()]; objectives listed in `exclude` are
#' then dropped.
#'
#' - Nodes = detailed objectives, labelled without their broad category
#'   ("Substantive - Data collection" -> "Data collection"; objectives
#'   without a sub-category, e.g. "Normative", keep their label). Node size =
#'   number of articles coding the objective.
#' - Node colour = broad category (text before "-", e.g. "Substantive").
#'   Colours are RColorBrewer "Set1" over all the broad categories coded
#'   (before `exclude`), as in [plot_engagement_objectives_detailed()], so
#'   that a broad category has the same colour in both plots.
#' - Links = co-occurrence: number of articles coding both objectives (link
#'   width). Pairs coded together in fewer than `min_cooccurrence` articles
#'   are not drawn.
#'
#' Two layouts (`layout`):
#' - `"circle"`: nodes on a circle, grouped by broad category
#'   (deterministic).
#' - `"fr"`: Fruchterman-Reingold force-directed layout, links weighted by
#'   the number of co-occurring articles (objectives often coded together
#'   are drawn closer). The layout is stochastic: it is computed with a fixed
#'   `seed` (via [withr::with_seed()], which restores the global random
#'   state afterwards), so the plot does not change between two calls.
#'
#' @param included_papers data frame with one row per included article and a
#'   character column `engagement_objectives` (";"-separated values)
#' @param exclude objectives not drawn (exact match on the full label)
#' @param min_cooccurrence minimum number of articles for a link to be drawn
#' @param layout node layout, `"circle"` or `"fr"` (force-directed)
#' @param seed random seed of the `"fr"` layout (ignored for `"circle"`)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_objectives_network <- function(included_papers,
                                    exclude = c("Unclear", "None"),
                                    min_cooccurrence = 1,
                                    layout = c("circle", "fr"),
                                    seed = 42) {
  layout <- match.arg(layout)
  stopifnot(is.data.frame(included_papers),
            "engagement_objectives" %in% names(included_papers),
            is.null(exclude) || is.character(exclude),
            is.numeric(min_cooccurrence), length(min_cooccurrence) == 1,
            is.numeric(seed), length(seed) == 1)

  # One row per article x objective (an objective coded twice counts once)
  objective_list <- lapply(
    strsplit(included_papers$engagement_objectives, ";"),
    format_and_correct_objectives
  )
  objectives_long <- tibble::tibble(
    article_index = rep(seq_along(objective_list), lengths(objective_list)),
    objective = unlist(objective_list)
  ) |>
    dplyr::filter(!is.na(.data$objective), .data$objective != "",
                  .data$objective != "NA") |>
    dplyr::distinct()

  # broad category = text before "-"; colour levels are taken before
  # `exclude` so that colours match plot_engagement_objectives_detailed()
  broad_category <- function(x) {
    trimws(vapply(strsplit(x, "-"), function(y) y[1], character(1)))
  }
  broad_levels <- sort(unique(broad_category(objectives_long$objective)))

  objectives_long <- objectives_long |>
    dplyr::filter(!.data$objective %in% exclude)
  if (nrow(objectives_long) == 0) {
    stop("No engagement objective left to plot.")
  }

  # Nodes, ordered by broad category then label (circle order)
  nodes <- objectives_long |>
    dplyr::count(.data$objective, name = "n_articles") |>
    dplyr::mutate(
      broad = broad_category(.data$objective),
      # detailed label = text after the first "-" (whole label if none)
      label = ifelse(grepl("-", .data$objective),
                     trimws(sub("^[^-]*-", "", .data$objective)),
                     .data$objective)
    ) |>
    dplyr::arrange(.data$broad, .data$label)

  # Links: pairs of objectives coded in the same article (self-join), each
  # unordered pair kept once
  links <- objectives_long |>
    dplyr::inner_join(objectives_long, by = "article_index",
                      suffix = c("_1", "_2"),
                      relationship = "many-to-many") |>
    dplyr::filter(.data$objective_1 < .data$objective_2) |>
    dplyr::count(.data$objective_1, .data$objective_2,
                 name = "n_articles") |>
    dplyr::filter(.data$n_articles >= min_cooccurrence)

  # Layout (igraph), nodes in the order of `nodes`
  graph <- igraph::graph_from_data_frame(
    links, directed = FALSE, vertices = nodes
  )
  coords <- if (layout == "circle") {
    igraph::layout_in_circle(graph)
  } else {
    # NOTE: FR is stochastic (random initial positions); the seed is set
    # locally so that the app's global random state is left untouched.
    # sqrt() weights: raw counts make the dense core collapse into a clump
    # (weak outliers push the scale), sqrt keeps "more co-occurrence =
    # closer" while spreading the nodes more evenly
    withr::with_seed(seed, igraph::layout_with_fr(
      graph, weights = sqrt(igraph::E(graph)$n_articles)
    ))
  }
  # Centre on the bounding-box midpoint and scale into [-1, 1] so that the
  # plot limits below fit both layouts: the circle is scaled uniformly
  # (no-op), the FR layout per axis (distances are not quantitative in a
  # force-directed layout, so stretching it to fill the panel is harmless)
  bbox_centre <- colMeans(apply(coords, 2, range))
  coords <- sweep(coords, 2, bbox_centre)
  coords <- if (layout == "circle") {
    coords / max(sqrt(rowSums(coords^2)), 1e-9)
  } else {
    sweep(coords, 2, pmax(apply(abs(coords), 2, max), 1e-9), "/")
  }
  nodes$x <- coords[, 1]
  nodes$y <- coords[, 2]

  links <- links |>
    dplyr::left_join(dplyr::select(nodes, "objective", x = "x", y = "y"),
                     by = c("objective_1" = "objective")) |>
    dplyr::left_join(dplyr::select(nodes, "objective", xend = "x",
                                   yend = "y"),
                     by = c("objective_2" = "objective")) |>
    # thinnest links drawn first (below)
    dplyr::arrange(.data$n_articles)

  # Labels offset from their node, away from the centre of the network, and
  # justified on that side (text on the right of right-hand nodes, above top
  # nodes, etc.). A node at the exact centre gets its label above it.
  label_offset <- 0.12
  nodes <- nodes |>
    dplyr::mutate(
      radius = sqrt(.data$x^2 + .data$y^2),
      dir_x = ifelse(.data$radius > 1e-9, .data$x / .data$radius, 0),
      dir_y = ifelse(.data$radius > 1e-9, .data$y / .data$radius, 1),
      label_x = .data$x + label_offset * .data$dir_x,
      label_y = .data$y + label_offset * .data$dir_y,
      hjust = (1 - .data$dir_x) / 2,
      vjust = (1 - .data$dir_y) / 2
    )

  # Circle: labels placed radially (above). FR: nodes can be inside the
  # network, so labels are repelled from each other and from the nodes
  # (point.size = drawn node size, same formula as scale_size_area() below)
  max_node_size <- 14
  label_layer <- if (layout == "circle") {
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = .data$label_x, y = .data$label_y,
                   label = .data$label, hjust = .data$hjust,
                   vjust = .data$vjust),
      size = 3.5, color = "grey15"
    )
  } else {
    ggrepel::geom_text_repel(
      data = dplyr::mutate(
        nodes,
        point_size = max_node_size * sqrt(.data$n_articles /
                                            max(.data$n_articles))
      ),
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                   point.size = .data$point_size),
      size = 3.5, color = "grey15", seed = seed,
      box.padding = 0.4, min.segment.length = Inf
    )
  }

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = links,
      ggplot2::aes(x = .data$x, y = .data$y,
                   xend = .data$xend, yend = .data$yend,
                   linewidth = .data$n_articles),
      color = "grey40", alpha = 0.5, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$n_articles,
                   fill = .data$broad),
      shape = 21, color = "white", stroke = 1
    ) +
    label_layer +
    # RColorBrewer "Set1" (qualitative), same mapping as the bar plots
    ggplot2::scale_fill_brewer(palette = "Set1", limits = broad_levels,
                               breaks = unique(nodes$broad),
                               name = "Broad category") +
    ggplot2::scale_size_area(max_size = max_node_size, name = "Articles") +
    ggplot2::scale_linewidth_continuous(range = c(0.3, 4),
                                        name = "Articles\n(co-occurrence)") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(size = 5),
                                   order = 1),
      size = ggplot2::guide_legend(override.aes = list(fill = "grey50"),
                                   order = 2)
    ) +
    # wide limits so that the labels outside the circle are not cut
    ggplot2::coord_equal(xlim = c(-1.8, 1.8), ylim = c(-1.25, 1.25),
                         clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "right",
                   plot.background = ggplot2::element_rect(fill = "white",
                                                           color = NA))
}
