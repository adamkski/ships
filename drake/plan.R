Rprof("your_samples.rds")
library(tidyverse)
library(drake)
library(here)

load_ships <- drake_plan(
  owners = read_rds( file_in(here("data/parsed_owners.rds")) ) %>% 
    bind_rows %>% 
    distinct(name, .keep_all = T),
  ships = read_rds( file_in(here("data/parsed_ships.rds")) ) %>% 
    distinct( official_number, name ) %>% 
    write_rds( file_out(here("data/ship_names.rds")) ),
  ship_list = read_rds( file_in(here("data/ship_names.rds")) ),
  ship_chunks = split( ship_list, cut_number(as.numeric(ship_list$official_number), max(floor(nrow(ship_list)/500), 4)) ),
  ship_owners = map_df( ship_chunks, ~.x %>% left_join(owners, by = "name") )
)

plan <- bind_rows(load_ships)

config <- drake_config(plan)
vis_drake_graph(config)
make(plan)
Rprof(NULL)