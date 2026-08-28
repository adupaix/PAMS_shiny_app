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

# source small functions
source('R/utils.R')

# Data dir
data_dir <- 'data'
reload_data <- F
sysrev_key <- Sys.getenv('SYSREV_KEY')
eez_path <- file.path(data_dir, 'EEZ_land_union_v4_202410')
lands_path <- file.path(data_dir, 'OSM_lands')
regions_path <- file.path(data_dir, 'French_regions')

# variables definition
n_to_code <- 769

# source file that loads and formats sysrev data
source('R/load_sysrev_data.R')
# save the csv file with the date in the name, use API only if the file was
# not downloaded today or if input on the first tab
