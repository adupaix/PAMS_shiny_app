#' @title Bar plot of the vulnerable habitats of the case studies
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study (one row of `case_study_data`). For each habitat,
#' number of case studies whose `vulnerable_habitat` includes it (exact
#' match, see [count_category_occurrences()]). A case study can be coded
#' with several habitats, so bars can sum to more than the number of case
#' studies. Bars are coloured by ecosystem (`groups`).
#'
#' @param case_study_data data frame with one row per case study and a
#'   character column `vulnerable_habitat` (";"-separated values)
#' @param categories habitats to plot, in display order (top to bottom)
#' @param groups ecosystem of each habitat (named by habitat), used as
#'   colour: "Land", "Coastal", "Open-ocean" or "None"
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_habitats <- function(case_study_data,
                          categories = habitat_levels,
                          groups = habitat_groups) {
  stopifnot(is.data.frame(case_study_data),
            "vulnerable_habitat" %in% names(case_study_data),
            all(categories %in% names(groups)))

  habitat_df <- tibble::tibble(
    habitat = categories,
    color = unname(groups[categories]),
    nb_case_studies = unname(count_category_occurrences(
      case_study_data$vulnerable_habitat, categories
    ))
  )

  # coord_flip() draws the first level at the bottom: reverse the limits so
  # that `categories` reads top to bottom
  ggplot2::ggplot(habitat_df,
                  ggplot2::aes(x = .data$habitat, y = .data$nb_case_studies,
                               fill = .data$color)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(categories)) +
    # RColorBrewer "Blues" shades, grey for "None"
    ggplot2::scale_fill_manual("", values = c(Coastal = "#3182BD",
                                              Land = "#6BAED6",
                                              `Open-ocean` = "#08519C",
                                              None = "grey")) +
    ggplot2::xlab("Habitats") +
    ggplot2::ylab("Number of case studies") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey")
    )
}
