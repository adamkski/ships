check_for_next_page_href <- function(page){
  page %>% 
    html_node("div[class='pagination-container']") %>% 
    html_nodes(xpath = "//a[text()='Next >']") %>% 
    html_attr("href")
}

extract_ships <- function(page){
  page %>% 
    html_node("table[class='span-6 zebra']") %>% 
    html_table() %>% 
    as_tibble() %>% 
    clean_names()
}

search_vessels <- function(port){
  vessel %>% 
    html_node("form[action='/Saf-Sec-Sur/4/vrqs-srib/eng/vessel-registrations/advanced-search']") %>% 
    html_form() %>% 
    set_values(
      DataSource="AllRegisteredVessels",
      ExcludeClosedVessels=FALSE,
      ExcludeSuspendedVessels=FALSE,
      PortOfRegistry=port
    )
}

.extract_ships_by_port <- function(port, pb) {
  pb$tick()$print()
  
  search_result <- vessel %>% 
    submit_form(search_vessels(port))
  
  # first page result
  result <- extract_ships(search_result)
  
  # page through results (if there are any)
  next_page <- check_for_next_page_href(search_result)
  while(length(next_page) > 0){
    search_result <- search_result %>% 
      jump_to(next_page) 
    result <- bind_rows(result, extract_ships(search_result))
    next_page <- check_for_next_page_href(search_result)
    Sys.sleep(0.5)
  }
  
  result
  
}

.extract_ships_by_port_err <- possibly(.extract_ships_by_port, otherwise = NULL)
extract_ships_by_port <- function(ports){
  pb <- progress_estimated(length(ports))
  map_df(ports, ~.extract_ships_by_port_err(.x, pb))
}


parse_div <- function(div_el){

  tibble(name = div_el %>% 
           html_nodes(".span-2") %>% 
           html_text(),
         value = div_el %>% 
           html_nodes(".span-3") %>% 
           html_text()) 
}

.scrape_ship <- function(ship_id, pb){
  
  Sys.sleep(.5)
  pb$tick()$print()
  url <- str_c("https://wwwapps.tc.gc.ca/Saf-Sec-Sur/4/vrqs-srib/eng/vessel-registrations/details/", ship_id)
  read_html(url) %>% 
    html_nodes("div[class='span-6 border-span-6']") %>% 
    write_rds(here(str_c("data/", ship_id, ".rds")))
  
}

scrape_ship <- function(list){
  
  pb <- progress_estimated(length(list))
  map(list, ~.scrape_ship(.x, pb))
  
}

parse_ship <- function(ship_raw){
  
  results <- bind_cols(
    
    # vessel
    parse_div(ship_raw[1]) %>% 
      pivot_wider(names_from = "name", values_from = "value") %>% 
      clean_names,
    
    # general statistics
    parse_div(ship_raw[2]) %>% 
      pivot_wider(names_from = "name", values_from = "value") %>% 
      clean_names,
    
    # engine
    parse_div(ship_raw[3]) %>% 
      pivot_wider(names_from = "name", values_from = "value") %>% 
      clean_names,
    
    # builder
    parse_div(ship_raw[4]) %>% 
      pivot_wider(names_from = "name", values_from = "value") %>% 
      clean_names
    
  )
  results
}



.parse_owners <- function(ship_raw){
  # owners
  result <- map_df(ship_raw[5:length(ship_raw)], ~parse_div(.x) %>% 
                     pivot_wider(names_from = "name", values_from = "value", values_fn = list(value = list)) %>% 
                     clean_names %>% 
                     unnest(cols = c(name, address, city, country, postal_code))) 
  
  result
}

parse_owners <- possibly(.parse_owners, otherwise = NULL)

parse_html_ship <- function(doc){
  
  doc %>% 
    read_html() %>% 
    html_nodes("div[class='span-6 border-span-6']") %>% 
    parse_ship()
} 


parse_html_owners <- function(doc){
  
  result <- doc %>% 
    read_html() %>% 
    html_nodes("div[class='span-6 border-span-6']") %>% 
      parse_owners()
  
} 

