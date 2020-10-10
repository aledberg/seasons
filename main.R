## Top-level script for generating the figures corrssponding to
## Ledberg 2020
## A large decrease in the magnitude of seasonal fluctuations in mortality among elderly
## explains part of the increase in longevity in Sweden during 20th century
##
## A. Ledberg, 10-10-2020


## This code in this file load the data and does some preprocessing.
## Next, a number of scripts are called, first to estimate hazard rates
## for the cohorts, and then to make the figures

## load required packages
library(data.table)
library(survival)
library(flexsurv)
require(splines)
require(ggplot2)
require(gridExtra)
require(lubridate)
require(latex2exp)
require(scales)
require(cowplot)
require(MASS)
########################################################################
## define the parameters to use (these were the ones used in the paper)

## Birth years
byears <- 1800:1901

## Starting age for hazard estimation 
startAge <- 60
## Number of years to include in the estimation
nyears <- 30

######################################################################
## load the data
fn="dataForPaper.rds"
dead <- readRDS(fn)

######################################################################
## call estimateRates.R to estimate the hazards and fit the model
## using poisson regression (Eq.~2 in the paper). This might take a little
## while but not more than a couple of minutes. 

source("estimateRates.R")

#####################################################################
## Next we can make the figures. 

## figure 1
source("figure1.R")

## figure 2
source("figure2.R")

## figure 3
source("figure3.R")

## figure 4, this takes a while (many minutes) since the intergrals in Eq. 5 + 6 are solved numerically.
## (this code could very likely be made considerably faster by using a more clever numerical integration routine)

source("figure4.R")

