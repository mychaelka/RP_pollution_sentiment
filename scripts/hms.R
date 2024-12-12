library(tidyverse)
library(sf)
library(viridis)
library(lutz)

sf_use_s2(FALSE)

usa_counties <- st_read("../data/USA_shapefiles/counties") |> 
  dplyr::select(NAME, STATE_NAME) |>
  filter(STATE_NAME != "Alaska") |>
  filter(STATE_NAME != "Hawaii") |> 
  mutate(CNTY_UNIQUE=paste(NAME, STATE_NAME))

# find local timezone for each county based on centroid
usa_counties <- usa_counties %>%
  mutate(
    county_centroid = st_centroid(geometry),  # Get the centroid of each county
    timezone = tz_lookup_coords(
      st_coordinates(county_centroid)[, 2],  # Latitude
      st_coordinates(county_centroid)[, 1],  # Longitude
      method = "accurate"
    )
  )

# load all HMS files
hms_dir <- "../data/HMS_smoke/"
hms_folders <- list.dirs(hms_dir, recursive = FALSE)
all_smoke_data <- data.frame()

Sys.setenv("OGR_GEOMETRY_ACCEPT_UNCLOSED_RING" = "NO")

# iterate over shapefiles and join all into one dataframe 
for (hms_folder in hms_folders) {
  shp_file <- list.files(hms_folder, pattern = "\\.shp$", full.names = TRUE)
  
  folder_name <- basename(hms_folder)
  date_str <- gsub(".*(\\d{8}).*", "\\1", folder_name)
  date_utc <- as.Date(date_str, format = "%Y%m%d")
  
  smoke_data <- st_read(shp_file)
  
  valid_check <- st_is_valid(smoke_data)
  if (any(is.na(valid_check)) || any(!valid_check, na.rm = TRUE)) {
    smoke_data <- st_make_valid(smoke_data)
  }
  
  smoke_data <- smoke_data %>%
    mutate(
      density_score = case_when(
        Density == "Light" ~ 1,
        Density == "Medium" ~ 2,
        Density == "Heavy" ~ 3,
        TRUE ~ 0
      )
    )
  
  smoke_data <- st_transform(smoke_data, st_crs(usa_counties))
  smoke_counties <- st_join(usa_counties, smoke_data, join = st_intersects)
  smoke_counties <- smoke_counties %>% 
    mutate(
      local_date = as.Date(with_tz(date_utc, tzone = timezone))  # convert timestamp to local date
  )
  
  smoke_by_county <- smoke_counties %>%
    st_drop_geometry() %>% 
    group_by(CNTY_UNIQUE) %>%
    summarize(total_exposure = median(density_score, na.rm = TRUE),
              date = last(local_date))

  smoke_by_county_df <- as.data.frame(smoke_by_county) %>%
    select(CNTY_UNIQUE, total_exposure, date)
  
  all_smoke_data <- bind_rows(all_smoke_data, smoke_by_county_df)
}

all_smoke_data <- usa_counties %>% 
  left_join(all_smoke_data, by="CNTY_UNIQUE")

# map
all_smoke_data %>% 
  st_transform(crs=5070) %>% 
  ggplot(aes(fill=total_exposure)) + 
  geom_sf(lwd=0.1) + 
  theme_void() +
  scale_fill_viridis(option="B",
                     na.value = "white") +
  labs(title = "Smoke Exposure by County", 
       fill = "Total Exposure", 
       caption = "Data from HMS") +
  facet_wrap(~ date, ncol = 6) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        panel.grid = element_blank())


# write csv
all_smoke_data %>% st_drop_geometry() %>% write_csv("../data/smoke_new.csv")
