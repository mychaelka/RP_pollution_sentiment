library(tidyverse)
library(fixest)
library(lutz)
library(sf)
library(readr)
library(lubridate)
library(data.table)

load("../data/full_data_counties.RData")

# create local timestamps (timestamps in original data are in UTC)
full_data$timezone <- tz_lookup(full_data, method = "accurate")
full_data$local_time <- with_tz(full_data$timestamp, tzone = full_data$timezone)
full_data$local_timebin <- with_tz(full_data$time_bin, tzone = full_data$timezone)
full_data$day <- as.POSIXlt(full_data$local_time)$wday
full_data$date <- as.Date(full_data$local_time)

# extract longitute/latitude from geometry
full_data <- full_data %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

# full_data_text is only necessary for examples of tweet classification and bot detection
# and for robustness check excluding pollution and wildfire related terms.
# otherwise drop the text column to have smaller size of data.
full_data_text <- full_data %>% st_drop_geometry() %>% drop_na()
full_data <- full_data |> dplyr::select(!(text)) |> st_drop_geometry() |> drop_na()

# calculate wind strength
full_data <- full_data %>% mutate(wind_strength = sqrt(wind_x^2 + wind_y^2),
                        wind_direction = (270 - (atan2(wind_y, wind_x) * (180 / pi))) %% 360)

# cut pm25 into bins
full_data <- full_data %>% 
  mutate(
    pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5),Inf), include.lowest = TRUE)
  )

# assign each tweet only one main topic
full_data$main_label <- lapply(full_data$label, function(x) {
  aux <- gsub('\\[', '', x) %>% 
    gsub('\\]', '', .) %>% 
    strsplit(., ",")
  ifelse(is.na(aux[[1]][1]), "'no_topic'", aux[[1]][1])
}) %>% unlist()

# hour of day indicator
full_data$day_hour <- lubridate::hour(full_data$local_time)

# county unique indicator (some states have the same county names)
full_data <- full_data %>% mutate(CNTY_UNIQUE=paste(full_data$NAME, full_data$STATE_NAME))
full_data_text <- full_data_text %>% mutate(CNTY_UNIQUE=paste(full_data$NAME, full_data$STATE_NAME))

# remove bots from data
bot_usernames <- read_csv("../data/bot_usernames.csv")
day_bot_usernames <- read_csv("../data/day_bot_usernames.csv")
bots <- full_data %>% filter(username %in% bot_usernames$username)
day_bots <- full_data %>% filter(username %in% day_bot_usernames$username)

# join bots data and standardize -- will be used later for placebo check
all_bots <- rbindlist(list(bots, day_bots)) %>% 
  dplyr::select(date, CNTY_UNIQUE, day, day_hour, username, main_label, lon, lat,
                pm25_cat, pm25, aod550, wind_x, wind_y, temperature, dewpoint, clouds, 
                precipitation, visibility,
                vader_compound, roberta_positive,
                roberta_negative, bertweet_positive,
                bertweet_negative) %>%
  drop_na() %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))


data <- full_data %>% filter(!(username %in% all_bots$username))
#data_text <- full_data_text %>% filter(!(username %in% all_bots$username))
rm(bot_usernames, day_bot_usernames, bots, day_bots)

# standardize main data
scaled <- data %>% dplyr::select(date, NAME, STATE_NAME, 
                                 CNTY_UNIQUE, day, day_hour, username, main_label, 
                                 lon, lat, pm25_cat, pm25, aod550, wind_x, wind_y, 
                                 temperature, dewpoint, clouds, precipitation, visibility,
                                 vader_compound, roberta_positive,
                                 roberta_negative, bertweet_positive,
                                 bertweet_negative, wind_strength) %>% 
  drop_na() %>% 
  mutate(across(c(pm25:wind_strength), ~ (.-mean(.)) / sd(.)))


# group data by county and date
grouped <- data %>%  
  group_by(CNTY_UNIQUE, date) %>% 
  dplyr::summarize(num_tweets=n(), pm25=median(pm25), temp_max=max(temperature), 
                   temp_min=min(temperature), temp_med=median(temperature), temp_mean=mean(temperature),
            wind_x=median(wind_x), wind_y=median(wind_y), dewpoint=median(dewpoint), 
            aod550=median(aod550), precip=median(precipitation), clouds=median(clouds),
            visibility=median(visibility),
            precip_mean=mean(precipitation), vis_mean=mean(visibility),
            vader_compound=median(vader_compound),
            roberta_positive=median(roberta_positive),
            roberta_negative=median(roberta_negative),
            bertweet_positive=median(bertweet_positive),
            bertweet_negative=median(bertweet_negative)) %>%
  mutate(wind_strength = sqrt(wind_x^2 + wind_y^2),
          wind_direction = (270 - (atan2(wind_y, wind_x) * (180 / pi))) %% 360) %>% 
  mutate(pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5), Inf), include.lowest = TRUE)) %>% 
  mutate(across(c(pm25:wind_strength), ~ (. -mean(.)) / sd(.))) # counties with only one tweet will result in NA's due to zero variance


# save all data so it does not have to be cleaned again
# save(full_data, data, all_bots, scaled, grouped, file="../data/models.RData")


### MODELS
load("../data/models.RData")

# baseline regression, weighted by number of tweets in each county
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp_med + I(temp_med^2) + precip + visibility + wind_strength | CNTY_UNIQUE + date,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
) |>
  etable()


# nonlinear regression -- pm25 bins
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat + temp_med + I(temp_med^2) + precip + visibility + wind_strength | CNTY_UNIQUE + date,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
) |>
  etable()


#### Individual level regressions
# county + date + username FE
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature + I(temperature^2) + precipitation + visibility + wind_strength | CNTY_UNIQUE + date + username,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# county + date + username + hour + topic FE
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature + I(temperature^2) + precipitation + visibility + wind_strength | CNTY_UNIQUE + date + username + main_label + day_hour,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()


#### Robustness and sensitivity checks
# baseline, mean instead of median
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp_mean + I(temp_mean^2) + precip_mean + vis_mean + wind_strength | CNTY_UNIQUE + date,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
) |>
  etable()


# bots -- placebo test
grouped_bots <- all_bots %>% group_by(CNTY_UNIQUE, date) %>% 
  summarize(num_tweets=n(), pm25=median(pm25), temp_med=median(temperature), 
            wind_x=median(wind_x), wind_y=median(wind_y), dewpoint=median(dewpoint), 
            aod550=median(aod550), precip=median(precipitation), clouds=median(clouds),
            visibility=median(visibility),
            vader_compound=median(vader_compound),
            roberta_positive=median(roberta_positive),
            roberta_negative=median(roberta_negative),
            bertweet_positive=median(bertweet_positive),
            bertweet_negative=median(bertweet_negative)) %>% 
  mutate(wind_strength = sqrt(wind_x^2 + wind_y^2),
         wind_direction = (270 - (atan2(wind_y, wind_x) * (180 / pi))) %% 360) %>% 
  mutate(pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5),Inf), include.lowest = TRUE)) %>% 
  mutate(across(c(pm25:wind_strength), ~ (.-mean(.)) / sd(.)))


feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp_med + I(temp_med^2) + precip + visibility + wind_strength | CNTY_UNIQUE + date,
  data = grouped_bots,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped_bots$num_tweets
) |>
  etable()


# remove wildfire counties -- California and Washington
data_without_california <- data %>% filter(STATE_NAME != "California") %>% filter(STATE_NAME != "Washington") %>% 
  group_by(CNTY_UNIQUE, date) %>% 
  dplyr::summarize(num_tweets=n(), pm25=median(pm25), temp_med=median(temperature), 
            wind_x=median(wind_x), wind_y=median(wind_y), dewpoint=median(dewpoint), 
            aod550=median(aod550), precip=median(precipitation), clouds=median(clouds),
            visibility=median(visibility),
            vader_compound=median(vader_compound),
            roberta_positive=median(roberta_positive),
            roberta_negative=median(roberta_negative),
            bertweet_positive=median(bertweet_positive),
            bertweet_negative=median(bertweet_negative)) %>% 
  mutate(wind_strength = sqrt(wind_x^2 + wind_y^2),
         wind_direction = (270 - (atan2(wind_y, wind_x) * (180 / pi))) %% 360) %>% 
  mutate(pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5),Inf), include.lowest = TRUE)) %>% 
  mutate(across(c(pm25:wind_strength), ~ (.-mean(.)) / sd(.)))


feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp_med + I(temp_med^2) + precip + visibility + wind_strength | CNTY_UNIQUE + date,
  data = data_without_california,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = data_without_california$num_tweets
) |>
  etable()


# remove wildfire and pollution-related tweets
terms_to_exclude <- c(
  # pollution related terms
  "pollution", "air quality", "smog", "particulate matter", "PM2.5", "PM10", "ozone", 
  "carbon monoxide", "CO2", "sulfur dioxide", "nitrogen dioxide", "emissions", "exhaust", 
  "greenhouse gases", "toxic air", "hazardous air", "fumes", "environmental impact", 
  "contamination", "pollutants", "soot", "dust", "ash", "industrial emissions", 
  "traffic pollution", "atmospheric pollution", "clean air", "respiratory issues", 
  "asthma triggers", "air purifier", "air monitoring", "pollution index",
  
  # wildfire related terms
  "wildfire", "wild fire", "forest fire", "bushfire", "bush fire", "wildland fire", 
  "firestorm", "fire hazard", "firefighting", "firefighter", "firefighters", 
  "controlled burn", "prescribed burn", "fire season", "fire suppression", 
  "smoke plume", "wildfire smoke", "flames", "blaze", "burning", "scorched", 
  "wildfire risk", "fire evacuation", "fire danger", "fire warning", "fire watch", 
  "smoke", "ash", "ember", "dry lightning", "flame retardant", "fire retardant", "fire containment", 
  "evacuation order", "red flag warning", "fire line", "firebreak", 
  "brush fire", "wildland", "fire lookout",
  "burn ban", "burn zone", "smoldering", "charred", "fire risk", "air quality alert"
)

pattern <- paste(terms_to_exclude, collapse = "|")
data_text <- data_text %>% 
  filter(!grepl(pattern, data_text$text, ignore.case = TRUE))


grouped_without_terms <- data_text %>%  
  group_by(CNTY_UNIQUE, date) %>% 
  dplyr::summarize(num_tweets=n(), pm25=median(pm25), temp_max=max(temperature), 
                   temp_min=min(temperature), temp_med=median(temperature), temp_mean=mean(temperature),
                   wind_x=median(wind_x), wind_y=median(wind_y), dewpoint=median(dewpoint), 
                   aod550=median(aod550), precip=median(precipitation), clouds=median(clouds),
                   visibility=median(visibility),
                   precip_mean=mean(precipitation), vis_mean=mean(visibility),
                   vader_compound=median(vader_compound),
                   roberta_positive=median(roberta_positive),
                   roberta_negative=median(roberta_negative),
                   bertweet_positive=median(bertweet_positive),
                   bertweet_negative=median(bertweet_negative)) %>% 
  mutate(wind_strength = sqrt(wind_x^2 + wind_y^2),
         wind_direction = (270 - (atan2(wind_y, wind_x) * (180 / pi))) %% 360) %>% 
  mutate(pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5), Inf), include.lowest = TRUE)) %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (. -mean(.)) / sd(.)))

# baseline without pollution and wildfire terms
feols(
  c(bertweet_positive, bertweet_negative) ~ pm25 + temp_med + I(temp_med^2) + precip + visibility + wind_strength | CNTY_UNIQUE + date,
  data = grouped_without_terms,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped_without_terms$num_tweets
) |>
  etable()


## instrumental variables with wildfire smoke
smoke <- read_csv("../data/HMS_smoke/smoke_new.csv") %>% 
  mutate(total_exposure=replace_na(total_exposure, 0))

grouped <- left_join(grouped, smoke %>% dplyr::select(CNTY_UNIQUE, date, total_exposure), by=c("CNTY_UNIQUE", "date"))
grouped <- grouped %>% mutate(across(c(total_exposure), ~ (. -mean(.)) / sd(.)))

# Reduced-form
feols(
  c(bertweet_positive, bertweet_negative) ~ total_exposure + temp_med + I(temp_med^2) + precip + visibility + wind_strength | CNTY_UNIQUE + date,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = ~num_tweets
) %>% 
  etable()

# IV
iv <- feols(
  c(bertweet_positive, bertweet_negative) ~ temp_med + I(temp_med^2) + precip + visibility + wind_strength | CNTY_UNIQUE + date | pm25 ~ total_exposure,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = ~num_tweets
)

iv %>% etable()
iv$`lhs: bertweet_positive`$iv_first_stage