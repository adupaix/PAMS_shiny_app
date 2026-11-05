#' Coded categories of the coding book
#'
#' Single definition of the categories coded in SysRev, shared by the Shiny
#' app and qmd/plot_workflow.qmd so that both always plot the same
#' categories. Each vector is written in display order.
#'
#' - `participant_type_levels`: values of `participant_type` (one participant
#'   group can be coded with several, ";"-separated). "Public authorities,
#'   decision makers and managers" is renamed "Decision makers and managers"
#'   in scripts/load_sysrev_data.R.
#' - `participant_type_other`: participant types drawn in grey (no known
#'   sector).
#'
#' @name categories
#' @export
participant_type_levels <- c(
  "Aquaculture sector", "Art & culture sector",
  "Decision makers and managers", "Education sector", "Energy sector",
  "Fisheries sector", "General public", "NGOs", "Other industries",
  "Science & research", "Tourism sector", "Others", "Unclear"
)

#' @rdname categories
#' @export
participant_type_other <- c("Others", "Unclear")
