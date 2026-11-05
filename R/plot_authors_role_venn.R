#' @title Venn diagram of the role of the authors
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = included article (one row of `included_papers`). Each set
#' contains the articles whose `leader_authors` includes the role (exact
#' match, see [list_rows_by_category()]).
#'
#' @param included_papers data frame with one row per included article and a
#'   character column `leader_authors` (";"-separated values)
#' @param categories author roles, in set order
#' @return a ggplot object
#' @export
plot_authors_role_venn <- function(included_papers,
                                   categories = author_role_levels) {
  stopifnot(is.data.frame(included_papers),
            "leader_authors" %in% names(included_papers))

  role_list <- list_rows_by_category(included_papers$leader_authors,
                                     categories)

  ggVennDiagram::ggVennDiagram(role_list, lwd = 0) +
    ggplot2::scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
    ggplot2::theme(legend.position = "none")
}
