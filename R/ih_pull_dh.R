library(httr2)
library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(tidygeocoder)

api <- "https://malls.fandom.com/api.php"

resp <- request(api) %>%
  req_url_query(
    action = "query",
    prop = "revisions",
    titles = "IHOP/Locations",
    rvprop = "content",
    rvslots = "main",
    format = "json",
    formatversion = 2
  ) %>%
  req_user_agent("Mozilla/5.0") %>%
  req_perform()

dat <- resp_body_json(resp, simplifyVector = FALSE)

wikitext <- dat$query$pages[[1]]$revisions[[1]]$slots$main$content

lines <- str_split(wikitext, "\n")[[1]] %>%
  str_squish()

status <- NA_character_
state <- NA_character_
rows <- list()

for (line in lines) {
  
  if (str_detect(line, "^==\\s*Current\\s*==")) status <- "Current"
  if (str_detect(line, "^==\\s*Former\\s*==")) status <- "Former"
  
  if (str_detect(line, "^===.+===$")) {
    state <- line %>%
      str_remove_all("=") %>%
      str_squish()
    next
  }
  
  if (!is.na(status) && !is.na(state) && str_detect(line, "^\\*")) {
    
    loc_raw <- line %>%
      str_remove("^\\*\\s*") %>%
      str_squish()
    
    parts <- str_split(loc_raw, "\\s+-\\s+", n = 3)[[1]]
    
    rows[[length(rows) + 1]] <- tibble(
      status = status,
      state = state,
      city = parts[1],
      address = parts[2],
      note = ifelse(length(parts) >= 3, parts[3], NA_character_),
      loc_raw = loc_raw
    )
  }
}

ihop_locations <- bind_rows(rows)

ihop_geo <- ihop_locations %>%
  filter(status == "Current") %>%
  mutate(full_address = paste(address, city, state, "USA")) %>%
  geocode(full_address, method = "osm", lat = latitude, long = longitude)

nrow(ihop_locations)
head(ihop_locations)