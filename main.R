# main.R
# This is the main driver

# Remove everything before starting
rm(list=ls())

# Set working directory (PC)
setwd("C:/Users/chanw/Dropbox/UChicago - Graduate/Research/Public Sector Employment/")
code_path <- "~/GitHub/public_employment/"
cps_path <- "C:/Users/chanw/Dropbox/UChicago - Graduate/Research/Public Sector Employment/Data/"

# Set working directory (Mac)
setwd("~/Dropbox/UChicago - Graduate/Research/Public Sector Employment/")
code_path <- "~/Documents/GitHub/public_employment/"
cps_path <- "~/Dropbox/UChicago - Graduate/Research/Public Sector Employment/Data/"

# Load packages
require(ggplot2)
require(ipumsr)
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
