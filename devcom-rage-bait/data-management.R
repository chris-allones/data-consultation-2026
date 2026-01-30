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


## category for soc med
soc_med_category_sub_dta <- 
  soc_med_dta |> 
  mutate(social_media_content = str_remove_all(social_media_content, "\\s*\\([^)]*\\)")) |> 
  separate_longer_delim(cols = social_media_content, delim = ",") |> 
  mutate(social_media_content = str_trim(social_media_content, "both")) |> 
  count(social_media_content, sort = T) |> 
  filter(n > 10)

## soc med category
soc_med_category <-
  soc_med_dta |> 
  mutate(social_media_content = str_remove_all(social_media_content, "\\s*\\([^)]*\\)")) |> 
  separate_longer_delim(cols = social_media_content, delim = ",") |> 
  mutate(social_media_content = str_trim(social_media_content, "both"))

## combining soc med categories
soc_med_content_dta <- 
  soc_med_category_sub_dta |> 
  left_join(soc_med_category) |> 
  select(-n)
  


