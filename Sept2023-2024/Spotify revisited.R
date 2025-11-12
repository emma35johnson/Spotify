
library(jsonlite)
library(tidyverse)
library(lubridate)

# Importing data
history <- lapply(
  c("C:/Users/emma3/OneDrive/Documents/Masters Coursework/Fall 2024/talk presentation_files/my_spotify_data/Spotify Account Data/StreamingHistory_music_0.json",
    "C:/Users/emma3/OneDrive/Documents/Masters Coursework/Fall 2024/talk presentation_files/my_spotify_data/Spotify Account Data/StreamingHistory_music_1.json",
    "C:/Users/emma3/OneDrive/Documents/Masters Coursework/Fall 2024/talk presentation_files/my_spotify_data/Spotify Account Data/StreamingHistory_music_2.json"),
  fromJSON) %>%
  bind_rows()

# Data structure
glimpse(history)

# Cleaning off the jump
history <- history %>%
  mutate(endTime = as.POSIXct(endTime, format="%Y-%m-%d %H:%M")
         ) %>% # Convert endTime to POSIXct (Date type)
  filter(trackName != "Unknown Track" | artistName != "Unknown Artist",
         format(endTime, "%Y-%m") != "2023-04"
         ) # Remove entries with "Unknown Track" or "Unknown Artist" and random 04/2023 entry



# Summary statistics

# Top 10 songs by total minutes listened
history %>%
  group_by(trackName, artistName) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n()) %>%
  arrange(desc(total_mins)) %>%
  head(10)

# Top 10 songs by total plays
history %>%
  group_by(trackName, artistName) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n()) %>%
  arrange(desc(total_plays)) %>%
  head(10)

# Top 10 artists by total minutes listened
history %>%
  group_by(artistName) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n()) %>%
  arrange(desc(total_mins)) %>%
  head(10)

# Top 10 artists by total plays
history %>%
  group_by(artistName) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n()) %>%
  arrange(desc(total_plays)) %>%
  head(10)

# Total time spent listening
history %>%
  summarise(total_mins = sum(msPlayed)/60000, 
            total_hrs = total_mins/60,
            total_days = total_hrs/24)

# Total number of unique songs and artists
history %>%
  summarise(unique_songs = n_distinct(trackName),
            unique_artists = n_distinct(artistName))

# Top artist each month by minutes listened
history %>%
  mutate(month = format(endTime, "%Y-%m")) %>%
  group_by(month, artistName) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n()) %>%
  arrange(month, desc(total_mins)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Top artist each month by total plays
history %>%
  mutate(month = format(endTime, "%Y-%m")) %>%
  group_by(month, artistName) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n()) %>%
  arrange(month, desc(total_plays)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Top song each month by minutes listened
history %>%
  mutate(month = format(endTime, "%Y-%m")) %>%
  group_by(month, trackName) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n()) %>%
  arrange(month, desc(total_mins)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Top song each month by total plays
history %>%
  mutate(month = format(endTime, "%Y-%m")) %>%
  group_by(month, trackName) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n()) %>%
  arrange(month, desc(total_plays)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Monthly summary statistics
history %>%
  mutate(month = format(endTime, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_mins = sum(msPlayed)/60000,
            total_plays = n(),
            unique_songs = n_distinct(trackName),
            unique_artists = n_distinct(artistName)) %>%
  arrange(month)

# Plot of minutes listened per month
history %>%
  mutate(month = format(endTime, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_mins = sum(msPlayed)/60000) %>%
  ggplot(aes(x = month, y = total_mins, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Minutes Listened Per Month",
       x = "Month",
       y = "Total Minutes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot of top 10 artists by total minutes listened
history %>%
  group_by(artistName) %>%
  summarise(total_mins = sum(msPlayed)/60000) %>%
  arrange(desc(total_mins)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(artistName, total_mins), y = total_mins)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Artists by Total Minutes Listened",
       x = "Artist",
       y = "Total Minutes") +
  theme_minimal()




# Extracting time components
history_times <- history %>%
  mutate(
    time = format(endTime, "%H:%M"), # does not have smaller time infor than minutes
    day = day(endTime),
    month = month(endTime),
    year = year(endTime)) %>%
  select(-endTime)
  
# Top 10 listening times (hours of the day) by total minutes listened
history_times %>%
  group_by(hour = hour(hm(time))) %>%
  summarise(total_mins = sum(msPlayed)/60000) %>%
  arrange(desc(total_mins)) %>%
  head(10)


## write as functions where I input the cols??


