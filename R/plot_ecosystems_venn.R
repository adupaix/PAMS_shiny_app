#' @title Venn diagram of the main ecosystems of the case studies
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study (one row of `case_study_data`). Each set contains
#' the case studies whose `ecosystem_main` includes the ecosystem (exact
#' match, see [list_rows_by_category()]). Case studies coded only "Unclear"
#' or with a missing ecosystem belong to no set.
#'
#' @param case_study_data data frame with one row per case study and a
#'   character column `ecosystem_main` (";"-separated values)
#' @param categories ecosystems, in set order; names are used as set labels
#' @return a ggplot object
#' @export
plot_ecosystems_venn <- function(case_study_data,
                                 categories = ecosystem_levels) {
  stopifnot(is.data.frame(case_study_data),
            "ecosystem_main" %in% names(case_study_data))

  ecosystem_list <- list_rows_by_category(case_study_data$ecosystem_main,
                                          categories)

  ggVennDiagram::ggVennDiagram(ecosystem_list, lwd = 0) +
    ggplot2::scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
    ggplot2::theme(legend.position = "none")
}
