# scrape some vessels from the site

# individual boats have designated web link
# e.g. https://wwwapps.tc.gc.ca/Saf-Sec-Sur/4/vrqs-srib/eng/vessel-registrations/details/314691

library(fs)
library(urltools)
library(janitor)
library(rvest)
library(tidyverse)
library(here)

source(here("functions.R"))

vessel <- html_session("https://wwwapps.tc.gc.ca/Saf-Sec-Sur/4/vrqs-srib/eng/vessel-registrations/advanced-search")

# get list of ports
ports <- vessel %>% 
  html_node("select[id='PortOfRegistry']") %>% 
  html_nodes("option") %>% 
  html_attr("value") 
# drop zero length
ports <- ports[str_length(ports) > 0]

# search through ports
vessel_scraped <- suppressMessages(extract_ships_by_port((ports)))

vessel_scraped %>% 
  write_rds(here("vessel_scraped.rds"))
vessel_scraped <- read_rds(here("vessel_scraped.rds"))


vessel_scraped %>% 
  mutate(port_of_registry = fct_lump(port_of_registry, 14)) %>% 
  count(port_of_registry, sort = T) %>% 
  mutate(p = n/sum(n),
         port_of_registry = fct_reorder(port_of_registry, p)) %>% 
  ggplot(aes(port_of_registry, p)) +
  geom_col() +
  coord_flip()

# scrape details of ships
active_vessels <- vessel_scraped %>% 
  filter(status != "CLOSED")
scrape_ship(active_vessels$official_number)

# try in pythong
active_vessels %>% 
  pull(official_number) %>% 
  str_c("https://wwwapps.tc.gc.ca/Saf-Sec-Sur/4/vrqs-srib/eng/vessel-registrations/details/", .) %>% 
  str_c(., collapse = "','") %>% 
  clipr::write_clip()


# try function on saved html
read_html(here("data/ship-138143.html")) %>% 
  html_nodes("div[class='span-6 border-span-6']") %>% 
  parse_ship()
