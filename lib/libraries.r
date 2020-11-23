library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)


options(gsubfn.engine="R")

library("readxl")
library("openxlsx")
library(dplyr)
library(readr)
library(tidyverse) 


# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
