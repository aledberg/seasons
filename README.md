# seasons

Data and R-code related to the manuscript _A large decrease in the magnitude of seasonal fluctuations in mortality among elderly explains part of the increase in longevity in Sweden during 20th century_ [available here](https://www.medrxiv.org/content/10.1101/2020.04.10.20060780v2)

---

## Data source
The data used in this project originates from [Sveriges dödbok](https://www.rotter.se/produkter/cd-dvd-usb/svdb), a genealogical database with information about people who have died in Sweden since 1860. Sveriges dödbok is released by Sveriges Släktforskarförbund (The Federation of Swedish Genealogical Societies) and I am grateful to them for allowing me to use the data. 

---

## The data included here

The data included in this project consists of the variables needed to reproduce the figures in the manuscript. More specifically, the following variables are available: sex, year of birth, date of death, lifespan (in days). 

---

## How-to process the data


The code is organized in a set of R-scripts as follows:

* main.R:  Contains the code to read data into R and calls a number of other scripts where estimation and plotting are done

* estimateRates.R: Code to estimate hazard rates for all the cohorts

* figure1.R: Generate figure 1 in the manuscript

* figure2.R: Generate figure 2 in the manuscript

* figure3.R: Generate figure 3 in the manuscript

* figure4.R: Generate figure 4 in the manuscript


**Example:**

To generate the figures do the following: 

* clone the repository (git clone https://github.com/aledberg/seasons)

* start R in the cloned directory

* at the R prompt execute source("main.R")


