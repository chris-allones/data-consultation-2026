## set working directory
setwd(here::here("rentaro-ai-effects-academic"))

## libraries
library(tidyverse)
library(readxl)
library(janitor)


## data management
read_excel("data/ai-data.xlsx") |>
  clean_names() |>
  glimpse()
