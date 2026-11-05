#' @title Coded categories of the coding book
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
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
#' - `involvement_levels`: values of `involvement_level`, from the highest to
#'   the lowest level ("No implication" is excluded from the plots).
#' - `research_steps`: values of `research_step`, in research-process order.
#' - `inclusivity_levels`: values of `participant_inclusivity`, in radar
#'   (clockwise) order.
#' - `depth_levels`: ordinal values of `impact_evaluation`, `levers` and
#'   `challenges` (raw values, before [merge_depth_levels()]).
#' - `area_choices`: article-level variables that can colour the papers per
#'   year plot (names = labels shown in the app picker and in the legend).
#' - `area_levels`: level order of each variable of `area_choices`.
#' - `discipline_levels`: values of `article_discipline` (Venn sets A, B, C).
#' - `author_role_levels`: values of `leader_authors`.
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

#' @rdname categories
#' @export
involvement_levels <- c(
  "Fully-lead", "Involve/Collaborate", "Consult", "Inform", "Unclear"
)

#' @rdname categories
#' @export
research_steps <- c(
  "Design", "Collect", "Analyze", "Evaluate", "Disseminate", "Unclear"
)

#' @rdname categories
#' @export
inclusivity_levels <- c("Gender", "Ethnic", "Age", "Nationality", "Other",
                        "None")

#' @rdname categories
#' @export
depth_levels <- c("No", "Mentioned", "Discussed", "Evaluated", "Detailed")

#' @rdname categories
#' @export
area_choices <- c(
  "Impact evaluation"          = "impact_evaluation",
  "Levers"                     = "levers",
  "Challenges"                 = "challenges",
  "Participants definition"    = "has_participant_definition",
  "Approach definition"        = "has_approach_definition"
)

#' @rdname categories
#' @export
area_levels <- list(
  impact_evaluation          = depth_levels,
  levers                     = depth_levels,
  challenges                 = depth_levels,
  has_participant_definition = c("FALSE", "TRUE"),
  has_approach_definition    = c("FALSE", "TRUE")
)

#' @rdname categories
#' @export
discipline_levels <- c(
  "Life science & Environment", "Sciences & Technology",
  "Social Sciences & Humanities"
)

#' @rdname categories
#' @export
author_role_levels <- c("Leader", "Participant", "Observer")
