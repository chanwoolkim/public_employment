# main.R
# This is the main driver

# Remove everything before starting
rm(list=ls())

# Set working directory (Mac)
setwd("~/Documents/GitHub/public_employment/")
code_path <- "~/Documents/GitHub/public_employment/"
cps_path <- "~/Dropbox/UChicago - Graduate/Research/Public Sector Employment/Data/"

# Load packages
require(ggplot2)
require(ipumsr)
require(MASS)
require(radiant)
require(RColorBrewer)
require(tidyverse)
require(xtable)

# Set seed
set.seed(2019)

# Execute
source(paste0(code_path, "prelim.R"))
source(paste0(code_path, "cps_clean.R"))
source(paste0(code_path, "cps_analysis.R"))
source(paste0(code_path, "cps_analysis_earnings.R"))
source(paste0(code_path, "cps_analysis_predict.R"))
source(paste0(code_path, "cps_analysis_education.R"))
source(paste0(code_path, "cps_analysis_occupation.R"))
