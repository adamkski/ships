library(furrr)
library(fs)
library(urltools)
library(janitor)
library(rvest)
library(tidyverse)
library(here)
source(here("functions.R"))

scrapes <- dir_ls("~/projects/get_ships/", regexp = "html$")
plan(multiprocess)

# parse
parsed_ships <- future_map_dfr(scrapes, ~parse_html_ship(.x), .progress = T)
parsed_ships <- parsed_ships %>% 
  select(-value:-value3)
parsed_ships %>% write_rds(here("data/parsed_ships.rds"))

parsed_owners <- future_map(scrapes, ~parse_html_owners(.x), .id = "html", .progress = T)
parsed_owners %>% write_rds(here("data/parsed_owners.rds"))

scrapes_parsed %>% 
  write_rds(here("data/scrapes_parsed.rds"))
