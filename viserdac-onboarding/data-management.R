# setup working directory
setwd(here::here("viserdac-onboarding"))

#libraries
library(tidyverse)
library(readxl)
library(janitor)

## import data
read_excel("viserdac-data.xlsx") |> 
  clean_names()
