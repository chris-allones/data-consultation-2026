## libraries
library(tidyverse)
library(janitor)
library(readxl)

# data management

## import data

likert_res_dta <- 
  read_excel("data/rage-bait-data.xlsx") |> 
  clean_names()

socio_demo_dta <- 
  read_excel("data/rage-bait-data.xlsx", sheet = 2) |> 
  clean_names()


## by social media platform
soc_med_dta <- 
  socio_demo_dta |> 
  select(sex, starts_with("social")) |> 
  separate_longer_delim(cols = social_media, delim = ",") |> 
  mutate(social_media = str_trim(social_media, "both")) 



