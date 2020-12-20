library(knitr)
library(rvest)
library(gsubfn)

library(tmap)
library(shiny)
library(RColorBrewer)

options(gsubfn.engine="R")

library("readxl")
library("openxlsx")

library(readr)

library(tidyverse)#library(dplyr),library(tidyr)
library(pdftools)



# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
