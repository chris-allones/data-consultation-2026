## working directory
setwd(here::here("didi-festival"))

## libraries
library(tidyverse)
library(readxl)
library(janitor)


## importing data
read_excel("data/fest-data.xlsx") |> 
  clean_names() |> 
  glimpse()



read_excel("data/fest-data.xlsx", sheet = 2) |> 
  clean_names() |> 
  glimpse()
  
