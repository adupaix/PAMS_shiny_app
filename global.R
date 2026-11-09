#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2026-08-03
#'@email : amael.dupaix@ird.fr
#'#***************************************************************************
#'@description: Shiny app to read exported data from
#'              Sysrev (https://www.sysrev.com/).
#'
#'#***************************************************************************

# Libraries and functions -------------------------------------------------
library(shiny)
library(bs4Dash) # update of shinydashboard
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggVennDiagram)
library(devtools)
library(shinyWidgets)
library(plotly)

if (!'SLRtools' %in% installed.packages()){
  devtools::install_github("yutannihilation/ggsflabel")
  devtools::install_github('FRBCesab/rbibtools')
  devtools::install_github('adupaix/SLRtools')
}
library(SLRtools)
if (!'ggradar' %in% installed.packages()){
  devtools::install_github('ricardo-bion/ggradar')
}
library(ggradar)

# NOTE: functions and constants in R/ are not sourced here: Shiny sources
# every R/*.R file automatically after global.R (shiny::loadSupport), so R/
# must only contain definitions (package-ready)

# Data dir
data_dir <- 'data'
reload_data <- F
sysrev_key <- Sys.getenv('SYSREV_KEY')
eez_path <- file.path(data_dir, 'EEZ_land_union_v4_202410')
lands_path <- file.path(data_dir, 'OSM_lands')
regions_path <- file.path(data_dir, 'French_regions')

# Output dir (generated artefacts, ignored by git): created at launch if missing
output_dir <- 'outputs'
if (!dir.exists(output_dir)) dir.create(output_dir, showWarnings = FALSE)

# variables definition
# update 28/09/2026
n_to_code <- 804

# area_choices, depth_levels and area_levels are defined in R/categories.R

# source file that loads and formats sysrev data
source('scripts/load_sysrev_data.R')
# save the csv file with the date in the name, use API only if the file was
# not downloaded today or if input on the first tab
