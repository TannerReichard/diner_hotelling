#libraries and helpers 
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(leaflet)
library(readxl)
library(tidyr)
library(tigris)
library(sf)

# if left hand side is null then return right side, need this for weird pulls from archive
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# because why isn't this a default function anywhere
sorted_table <- function(x){
  table(x) %>% 
    as.data.frame() %>% 
    arrange(desc(Freq))
}

# main pull #

#loads in snapshots by day and gets archive_url to feed next section

get_wayback_snapshots_daily <- function(
    url = "https://locations.wafflehouse.com",
    from = "20230801"
) {
  res <- httr::GET(
    "https://web.archive.org/cdx",
    query = list(
      url = url,
      output = "json",
      fl = "timestamp,original,statuscode,mimetype,digest",
      filter = "statuscode:200",
      from = from,
      collapse = "timestamp:8"   # <- one capture per YYYYMMDD
    ),
    httr::timeout(150)
  )
  
  httr::stop_for_status(res)
  
  x <- jsonlite::fromJSON(
    httr::content(res, as = "text", encoding = "UTF-8")
  )
  
  tibble::as_tibble(x[-1, , drop = FALSE], .name_repair = "minimal") |>
    rlang::set_names(x[1, ]) |>
    dplyr::mutate(
      timestamp_raw = timestamp,
      timestamp = lubridate::ymd_hms(timestamp),
      snapshot_date = as.Date(timestamp),
      archive_url = paste0(
        "https://web.archive.org/web/",
        timestamp_raw,
        "id_/",
        original
      )
    )
}

#before this date things start to get funky with the archives

targets_daily <- get_wayback_snapshots_daily(
  from = "20230801"
)


#parsing function ####

parse_waffle_snapshot_url <- function(archive_url) {
  res <- httr::GET(
    archive_url,
    httr::user_agent("Mozilla/5.0"),
    httr::timeout(30)
  )
  
  httr::stop_for_status(res)
  
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  
  next_json <- stringr::str_match(
    txt,
    '<script id="__NEXT_DATA__" type="application/json">(.*?)</script>'
  )[, 2]
  
  if (is.na(next_json)) return(NULL)
  
  dat <- jsonlite::fromJSON(next_json, simplifyVector = FALSE)
  locs <- dat$props$pageProps$locations
  
  if (is.null(locs)) return(NULL)
  
  tibble::tibble(raw = locs) |>
    dplyr::transmute(
      store_code = purrr::map_chr(raw, ~ .x$storeCode %||% NA_character_),
      business_name = purrr::map_chr(raw, ~ .x$businessName %||% NA_character_),
      city = purrr::map_chr(raw, ~ .x$city %||% NA_character_),
      state = purrr::map_chr(raw, ~ .x$state %||% NA_character_),
      latitude = purrr::map_dbl(raw, ~ as.numeric(.x$latitude %||% NA_real_)),
      longitude = purrr::map_dbl(raw, ~ as.numeric(.x$longitude %||% NA_real_)),
      
      status_code = purrr::map_chr(raw, ~ .x$`_status` %||% NA_character_),
      
      #shotgun out and see what we find
      
      business_hours_json = purrr::map_chr(
        raw,
        ~ jsonlite::toJSON(.x$businessHours %||% list(), auto_unbox = TRUE, null = "null")
      ),
      
      formatted_business_hours = purrr::map_chr(
        raw,
        ~ paste(.x$formattedBusinessHours %||% NA_character_, collapse = "; ")
      ),
      
      special_hours_json = purrr::map_chr(
        raw,
        ~ jsonlite::toJSON(.x$specialHours %||% list(), auto_unbox = TRUE, null = "null")
      ),
      
      custom_json = purrr::map_chr(
        raw,
        ~ jsonlite::toJSON(.x$custom %||% list(), auto_unbox = TRUE, null = "null")
      ),
      
      closed_reason = purrr::map_chr(
        raw,
        ~ .x$closedReason %||% .x$closureReason %||% .x$temporarilyClosedReason %||% NA_character_
      )
    )
}

# end function start parsing ####

safe_parse <- purrr::possibly(parse_waffle_snapshot_url, otherwise = NULL)

out <- vector("list", nrow(targets_daily))

for (i in seq_len(nrow(targets_daily))) {
  message("[", i, "/", nrow(targets_daily), "] ", targets_daily$snapshot_date[i])
  
  out[[i]] <- safe_parse(targets_daily$archive_url[i])
  
  Sys.sleep(2)
  
  print(i)
}


hist_locs <- purrr::map2_dfr(out,
                             targets_daily$snapshot_date,
                             ~if(is.null(.x) NULL else dplyr::mutate(.x, snapshot_date = .y))
)




city_county_ref_one <- places_sf %>%
  st_join(
    counties_sf,
    join = st_intersects,
    left = FALSE
  ) %>%
  mutate(overlap_area = as.numeric(st_area(geometry))) %>% 
  st_drop_geometry() %>%
  transmute(
    city,
    state_fips = state_fips.x,
    county,
    cz_fips,
    overlap_area
  ) %>%
  group_by(city, state_fips) %>%
  slice_max(overlap_area, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(city, state_fips, county, cz_fips)


waho_city_keys <- waho2 %>%
  distinct(city, state_fips)

places_needed <- places_sf %>%
  semi_join(waho_city_keys, by = c("city", "state_fips"))

city_county_ref_one <- places_needed %>%
  st_point_on_surface() %>%
  st_join(
    counties_sf,
    join = st_intersects,
    left = FALSE
  ) %>%
  st_drop_geometry() %>%
  transmute(
    city,
    state_fips = state_fips.x,
    county,
    cz_fips
  ) %>%
  distinct(city, state_fips, .keep_all = TRUE)

waho_final <- hist_locs %>%
  left_join(
    city_county_ref_one,
    by = c("city", "state_fips")
  )


write.csv(waho_final, "raw_wh_data.csv", row.names = F)
