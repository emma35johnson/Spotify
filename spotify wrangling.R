if (!require(jsonlite)) install.packages("jsonlite")
library(jsonlite)
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

######################################

browseURL("https://support.spotify.com/us/article/understanding-my-data/")

######################################

lib <- fromJSON("C:/Users/emma3/Downloads/my_spotify_data/Spotify Account Data/YourLibrary.json")
glimpse(lib)
str(lib)

str(lib$tracks)

head(sort(table(lib$tracks$artist), decreasing = TRUE)) # most saved songs by artist

frequency_df <- lib$tracks %>%
  count(artist) %>% 
  arrange(desc(n))

frequency_df[1:20,] # artists with the most songs saved


############ HIST 1 (from 9/12/2023 - 2/24/2024)
hist1 <- fromJSON("C:/Users/emma3/Downloads/my_spotify_data/Spotify Account Data/StreamingHistory_music_0.json")
str(hist1)

result <- hist1 %>%
  group_by(trackName) %>%
  summarise(total_score1 = sum(msPlayed), .groups = 'drop')
str(result)
head(sort(result$total_score1, decreasing = TRUE))

result %>%
  arrange(desc(total_score1))



############ HIST 2 (from 2/24/2024 - 7/4/2024)

hist2 <- fromJSON("C:/Users/emma3/Downloads/my_spotify_data/Spotify Account Data/StreamingHistory_music_1.json")
str(hist2)
head(sort(table(hist2$msPlayed), decreasing = TRUE))
hist2$trackName


result2 <- hist2 %>%
  group_by(trackName) %>%
  summarise(total_score2 = sum(msPlayed), .groups = 'drop')
str(result2)
head(sort(result2$total_score2, decreasing = TRUE))

result2 %>%
  arrange(desc(total_score2))


############ HIST 3 (from 7/4/2024 - 9/11/2024)

hist3 <- fromJSON("C:/Users/emma3/Downloads/my_spotify_data/Spotify Account Data/StreamingHistory_music_2.json")
str(hist3)
head(sort(table(hist3$msPlayed), decreasing = TRUE))
hist3$trackName


result3 <- hist3 %>%
  group_by(trackName) %>%
  summarise(total_score3 = sum(msPlayed), .groups = 'drop')
str(result3)
head(sort(result3$total_score3, decreasing = TRUE))

result3 %>%
  arrange(desc(total_score3))



################################################ ALL HISTORY

## Total Time Spent Listening

## Milliseconds

hist.all <- bind_rows(hist1, hist2, hist3)
str(hist.all)

hist.sort.ms <- hist.all %>%
  group_by(trackName, artistName) %>%
  summarise(ms = sum(msPlayed), .groups = 'drop')%>%
  arrange(desc(ms))

hist.sort.ms %>%
  print(n = 50)

## Minutes:

hist.sort.min <- hist.all %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))

hist.sort.min %>%
  print(n = 50)

## Hours:

hist.sort.hr <- hist.all %>%
  group_by(trackName, artistName) %>%
  summarise(hr = sum(msPlayed)/3600000, .groups = 'drop') %>%
  arrange((desc(hr)))

hist.sort.hr %>%
  print(n = 50)

## Total time spent listening to music
sum(hist.sort.min$min) # output = 47525.81
# easy way to convert to hours after the work already done:
sum(hist.sort.hr$hr) # output = 792.0968
# total number of (consecutive) days spent listening
(sum(hist.sort.hr$hr))/24 # output = 33.00403


##### By artist (i.e. interaction between artist and minutes listed)

hist.sort.art <- hist.sort.min %>%
  group_by(artistName) %>%
  summarise(minutes = sum(min), .groups = 'drop') %>%
  arrange((desc(minutes)))

hist.sort.art %>%
  print(n = 50)

# plotting top 10 artists minutes spent listening
ggplot(data = hist.sort.art[1:10,], aes(artistName, minutes)) +
  geom_point(size = 2.5) +
  labs(x = "Artist", y = "Minutes", title = "Time Spent Listening to Top 10 Artists of 2024") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################### By song
hist.sort.min <- hist.all %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop')
hist.sort.min %>%
  group_by(trackName) %>%
  summarise(minutes = sum(min), .groups = 'drop')%>%
  arrange((desc(minutes)))

hist.sort.art <- hist.sort.art %>%
  arrange((desc(minutes)))

hist.sort.art






## Monthly Breakdown

sep23.hist <- hist.all %>%
  filter(grepl("2023-09", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min))) # starts midway through the month (9/12/23)
oct23.hist <- hist.all %>%
  filter(grepl("2023-10", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
nov23.hist <- hist.all %>%
  filter(grepl("2023-11", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
dec23.hist <- hist.all %>%
  filter(grepl("2023-12", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
jan24.hist <- hist.all %>%
  filter(grepl("2024-01", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
feb24.hist <- hist.all %>%
  filter(grepl("2024-02", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
mar24.hist <- hist.all %>%
  filter(grepl("2024-03", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
apr24.hist <- hist.all %>%
  filter(grepl("2024-04", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
may24.hist <- hist.all %>%
  filter(grepl("2024-05", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
jun24.hist <- hist.all %>%
  filter(grepl("2024-06", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
jul24.hist <- hist.all %>%
  filter(grepl("2024-07", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
aug24.hist <- hist.all %>%
  filter(grepl("2024-08", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min)))
sep24.hist <- hist.all %>%
  filter(grepl("2024-09", endTime)) %>%
  group_by(trackName, artistName) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop') %>%
  arrange((desc(min))) # ends midway through the month (9/11/24)

matrix(cbind(
sum(sep23.hist$min),
sum(oct23.hist$min),
sum(nov23.hist$min),
sum(dec23.hist$min),
sum(jan24.hist$min),
sum(feb24.hist$min),
sum(mar24.hist$min),
sum(apr24.hist$min),
sum(may24.hist$min),
sum(jun24.hist$min),
sum(jul24.hist$min),
sum(aug24.hist$min),
sum(sep24.hist$min)))#work on adding another id column
# order from highest to lowest: Jan, Feb, Dec, Apr, Aug, Mar, Jun, May, Jul, Oct, Nov, Sept24*, Sept23*
# if combining Septembers:
sum(c(sep23.hist$min, sep24.hist$min))
# order then:  Jan, Feb, Dec, Apr, Aug, Mar, Jun, Sept*, May, Jul, Oct, Nov

# Creating a linear model for monthly listening
min.hist <- hist.all %>%
  group_by(trackName, artistName, endTime) %>%
  summarise(min = sum(msPlayed)/60000, .groups = 'drop')

#sep24.hist <- 
k <- min.hist %>%
  filter(grepl("2023-09", endTime))
#   # ends midway through the month (9/11/24)
#sum(k$min)

month.min <- c(
  sep23.hist$min,
  oct23.hist$min,
  nov23.hist$min,
  dec23.hist$min,
  jan24.hist$min,
  feb24.hist$min,
  mar24.hist$min,
  apr24.hist$min,
  may24.hist$min,
  jun24.hist$min,
  jul24.hist$min,
  aug24.hist$min,
  sep24.hist$min)
month <- as.factor(c(rep("sep23", length(sep23.hist$min)), 
                      rep("oct23", length(oct23.hist$min)), 
                      rep("nov23", length(nov23.hist$min)),
                      rep("dec23", length(dec23.hist$min)), 
                      rep("jan24", length(jan24.hist$min)), 
                      rep("feb24", length(feb24.hist$min)),
                      rep("mar24", length(mar24.hist$min)), 
                      rep("apr24", length(apr24.hist$min)), 
                      rep("may24", length(may24.hist$min)),
                      rep("jun24", length(jun24.hist$min)), 
                      rep("jul24", length(jul24.hist$min)), 
                      rep("aug24", length(aug24.hist$min)),
                      rep("sep24", length(sep24.hist$min))
                    ))
str(month)
levels(month)

all <- data.frame(month.min, month)
anova(lm(month.min ~ month, data = all))
# H0: The mean minutes spent listening per month will be equal for all months.
# Ha: The mean response will be different for at least 1 of the month pairs.

tukey <- TukeyHSD(aov(month.min ~ month, data = all))
# nov23-apr24 
# oct23-apr24
# sep24-apr24
# dec23-aug24
# and so on


# combining septembers 
sept2324.mean <- sum(sum(sep23.hist$min), sum(sep24.hist$min))

monthly.total <- year %>%
  group_by(month) %>%
  summarise(total.min = sum(month.min))

ggplot(data = year, aes(x = month, y = month.min)) +
  geom_point(size = 1) +
  labs(x = "Month", y = "Minutes", title = "Monthly Streaming Activity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = monthly.total, aes(x = month, y = total.min)) +
  geom_line(color = "blue") +
  geom_point() +  # Optional: adds points at each time
  labs(title = "Total Minutes Listened Per Month",
       x = "Month",
       y = "Minutes")

str(hist.sort.min)

#hist.all$min <- (hist.all$msPlayed)/60000
#ggplot(data = hist.all, aes(x = endTime, y = min)) +
#  geom_line(color = "blue") +
#  labs(title = "Time Series",
#       x = "Month",
#       y = "Minutes")

group_means <- c(sum(oct23.hist$min),
                 sum(nov23.hist$min),
                 sum(dec23.hist$min),
                 sum(jan24.hist$min),
                 sum(feb24.hist$min),
                 sum(mar24.hist$min),
                 sum(apr24.hist$min),
                 sum(may24.hist$min),
                 sum(jun24.hist$min),
                 sum(jul24.hist$min),
                 sum(aug24.hist$min),
                 sept2324.mean)

overall_mean <- mean(group_means)

# Initialize a results data frame
results <- data.frame(Group_Mean = group_means)

# Calculate the difference from the overall mean
results$Difference = results$Group_Mean - overall_mean

# Assuming you have the standard deviation of the group means
# You can also calculate the standard error (SE)
std_dev <- sd(group_means)
n <- length(group_means)  # Number of groups
se <- std_dev / sqrt(n)

# Calculate z-scores for comparison (if applicable)
results$Z_score <- abs(results$Difference / se)

# Determine significance (e.g., using a z-score threshold for normal distribution)
# change alpha to see different results
alpha <- 0.05/12 # familywise
critical_z <- qnorm(1 - alpha / 2)  # Two-tailed test

results$Significant <- ifelse(results$Z_score > critical_z, "Yes", "No")

results$Month <- as.factor(c("oct23","nov23","dec23","jan24","feb24",
                             "mar24","apr24","may24","jun24","jul24","aug24","sep2324"))

results

# next, seasons?