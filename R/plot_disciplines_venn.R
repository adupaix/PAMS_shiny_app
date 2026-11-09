#' @title Venn diagram of the article disciplines
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = included article (one row of `included_papers`). Each set
#' contains the articles whose `article_discipline` includes the discipline
#' (exact match, see [list_rows_by_category()]). Sets are labelled A, B, C...
#' and the correspondence is given in the title.
#'
#' @param included_papers data frame with one row per included article and a
#'   character column `article_discipline` (";"-separated values)
#' @param categories disciplines, in set order (A, B, C...)
#' @return a ggplot object
#' @export
plot_disciplines_venn <- function(included_papers,
                                  categories = discipline_levels) {
  stopifnot(is.data.frame(included_papers),
            "article_discipline" %in% names(included_papers))

  set_letters <- LETTERS[seq_along(categories)]
  disciplines_list <- list_rows_by_category(
    included_papers$article_discipline,
    stats::setNames(categories, set_letters)
  )

  ggVennDiagram::ggVennDiagram(disciplines_list, lwd = 0) +
    ggplot2::ggtitle(paste0(set_letters, ": ", categories, "\n",
                            collapse = "")) +
    ggplot2::scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
    ggplot2::theme(legend.position = "none")
}
