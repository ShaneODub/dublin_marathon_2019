library(dplyr)
library(rvest)
library(tidyverse)
library(hms)

url <- "https://www.tdleventservices.co.uk/event-results/events?event=3492&page="

list.of.dataframes <- vector("list",180)

page <- 1

while (page <=180){
  list.of.dataframes[[page]] <-
    paste(url,page,sep="") %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/div/div/div[2]/div[2]/table') %>% 
    html_table() %>%
    data.frame()
  page <- page + 1
  Sys.sleep(60)
}

results <- bind_rows(list.of.dataframes)
# Didn't work: "Error: Column `Gender.Position` can't be converted from integer to character"

# First, lapply mutate_if over the list and then bind_rows
results <- 
  list.of.dataframes %>% 
  lapply(., mutate_if, is.integer, as.character) %>% 
  bind_rows() %>% 
  as_tibble()

# Visual inspection
View(results)

# Keeping just the columns I plan to use
results <- select(results,1,4,6,10,12,14,16,18,20,22,21)

# Race.Number, Gender, Category apparently contain no missing or weird values.
#
# X10K,X20K,Halfway,X30K,X40K,Chip.Time etc. contain times at various checkpoints, but 
# also some NA and blank cells, and 'DNF' for some of the non-finishers.
#
# Overall.Position contains the order of finishing, but also 'DNF' for the non-finishers.
#
# Gun.Time contains only times, and 'DNF'. No empty or NA cells.
#
# All columns are currently character type.

glimpse(results)
# Observations: 17,931
# Variables: 11
# $ Race.Number      <chr> "14", "25", "3", "1", "8", "24", "5", "28", "65", "20", "21", "26"...
# $ Gender           <chr> "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "M...
# $ Category         <chr> "MS", "MS", "MS", "MS", "MS", "MS", "MS", "MS", "MS", "MS", "MS", ...
# $ X10K             <chr> "00:31:54", "00:31:53", "00:31:53", "00:31:54", "00:31:54", "00:31...
# $ X20K             <chr> "01:01:53", "01:03:30", "01:01:53", "01:01:52", "01:01:53", "01:03...
# $ Halfway          <chr> "01:05:06", "01:06:44", "01:05:07", "01:05:06", "01:05:05", "01:06...
# $ X30K             <chr> "01:32:31", "01:35:00", "01:32:31", "01:33:17", "01:33:55", "01:34...
# $ X40K             <chr> "02:01:42", "02:05:18", "02:04:49", "02:05:41", "02:05:52", "02:06...
# $ Gun.Time         <chr> "02:08:06", "02:12:01", "02:12:05", "02:13:02", "02:13:11", "02:13...
# $ Chip.Time        <chr> "02:08:06", "02:12:01", "02:12:04", "02:13:01", "02:13:10", "02:13...
# $ Overall.Position <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13...

# Let's turn the data into long/tidy format before doing anything else.
results <-   
  results %>% 
  pivot_longer(cols = c("X10K","X20K","Halfway","X30K","X40K","Gun.Time","Chip.Time"),
               names_to = "Distance",
               values_to = "Time")

glimpse(results)
# Observations: 125,517
# Variables: 6
# $ Race.Number      <chr> "14", "14", "14", "14", "14", "14", "14", "2...
# $ Gender           <chr> "Male", "Male", "Male", "Male", "Male", "Mal...
# $ Category         <chr> "MS", "MS", "MS", "MS", "MS", "MS", "MS", "M...
# $ Overall.Position <chr> "1", "1", "1", "1", "1", "1", "1", "2", "2",...
# $ Distance         <chr> "X10K", "X20K", "Halfway", "X30K", "X40K", "...
# $ Time             <chr> "00:31:54", "01:01:53", "01:05:06", "01:32:3...

## Cleaning
## 1. Need to split a couple of columns
## 2. Need appropriate data types for each column
## 3. Deal with missing values

# 'Category' column contains Gender info and Age info. We already have a Gender column.
unique(results$Category)
# [1] "MS"   "M45"  "M35"  "M40"  "FS"   "M50"  "F35"  "F40"  "M65"  "F45"  "M55" 
# [12] "M60"  "MU19" "F50"  "F55"  "F60"  "F65"  "M70"  "FU19" "M75"  "F75"  "F70" 
# [23] "M80"

results$Category <- 
  results$Category %>% 
  str_remove("[MF]") %>%               # Remove all the M for Male and F for Female.
  str_replace("U19","18") %>%          # Change U19 (under 19) to 18.
  str_replace("S","20")                # Change S (senior) to 20.

# Give the 'Category' column a more meaningful name, 'Age.Bracket'
results <- 
  results %>% 
  rename(Age.Bracket = Category)

unique(results$Age.Bracket)
#  [1] "20" "45" "35" "40" "50" "65" "55" "60" "18" "70" "75" "80"

# The Overall.Position column contains finishing place and a 'DNF' for those who didn't finish.
# Creating a logical column with TRUE or FALSE to indicate whether the runner finished.

results <-   
  results %>% 
  mutate(Finisher = (Overall.Position != "DNF")) %>% 
  select(1,2,3,4,7,5,6)          # Placing the new 7th column at the 5th position.

# The distance variable contains the old column names from the original wide format.
# We can make them more meaningful.

results$Distance <- 
  results$Distance %>% 
  str_remove_all("[XK]") %>%                  # X10K, X20K, X30K, X40K becomes 10,20,30,40
  str_replace("Halfway","21.1") %>%           # The halfway point is at 21.1 kilometers.
  str_replace("Chip.Time","42.2")             # The finish line is 42.2 kilometers

unique(results$Distance)
# [1] "10"       "20"       "21.1"     "30"       "40"       "Gun.Time" "42.2"
# We're going to use Gun.Time to fill in some of the blanks in the Time column  
# where Distance = 42.2 (formerly 'Chip.Time').

# Now to assign an appropriate type to each variable. This will coerce some of the 
# invalid entries to 'NA'.
#
# Race.Number (in theory) is based on predicted time when signing up for the race, so 
# choosing as.numeric() for this rather than factor or character.

results <-   
  results %>% 
  mutate(Race.Number = as.numeric(Race.Number),
         Gender = as.factor(Gender),
         Age.Bracket = as.numeric(Age.Bracket),
         Overall.Position = as.numeric(Overall.Position),
         Time = as.numeric(as_hms(Time))/60)   # Times are now expressed in minutes.

# Where distance = 42.2 and time is NA, replace from Gun.Time
# which is always the previous (lagging) entry.
results <-   
  results %>%
  mutate(Time = if_else(is.na(Time) & Distance == 42.2,
                        lag(Time),
                        Time))

# Now getting rid of the 'Gun.Time' rows and refactoring the Distance column as numeric.
results <- 
  results %>% 
  filter(Distance != "Gun.Time") %>%
  mutate(Distance = as.numeric(Distance))

glimpse(results)
# Observations: 107,586
# Variables: 7
# $ Race.Number      <dbl> 14, 14, 14, 14, 14, 14, 25, 25, 25, 25, 25, 25, 3, 3, ...
# $ Gender           <fct> Male, Male, Male, Male, Male, Male, Male, Male, Male, ...
# $ Age.Bracket      <dbl> 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20...
# $ Overall.Position <dbl> 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, ...
# $ Finisher         <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, ...
# $ Distance         <dbl> 10.0, 20.0, 21.1, 30.0, 40.0, 42.2, 10.0, 20.0, 21.1, ...
# $ Time             <dbl> 31.90000, 61.88333, 65.10000, 92.51667, 121.70000, 128...

# How many runners who finished are missing more than 1 checkpoint?
results %>%
  group_by(Race.Number) %>%
  filter(is.na(Time) & Finisher) %>% 
  count() %>%
  filter(n > 1)
# A tibble: 85 x 2
# Groups:   Race.Number [85]
#       Race.Number n
#          <dbl>  <int>
# 1         761     4
# 2        1648     3
# 3        2168     2
# 4        2461     5
# 5        3051     3
# 6        4511     3
# 7        4895     3
# 8        5067     3
# 9        5076     2
# 10        5506     2
# # ... with 75 more rows

# Going to join the count (n) to the results tibble
results <- 
  left_join(results, 
            results %>% 
              group_by(Race.Number) %>%
              filter(is.na(Time)) %>% 
              count(),
            by = "Race.Number")

results$n[is.na(results$n)] <- 0

# Showing one runner as an example:
results %>% filter(Race.Number == 19065) %>% select(1,5:8)
# This runner missed the 10k & Halfway checkpoint, so n = 2 in rightmost col.
# A tibble: 6 x 5
#     Race.Number Finisher   Distance  Time     n
#         <dbl>     <lgl>       <dbl>  <dbl> <dbl>
# 1       19065     TRUE         10     NA      2
# 2       19065     TRUE         20    196.     2
# 3       19065     TRUE         21.1   NA      2
# 4       19065     TRUE         30    304.     2
# 5       19065     TRUE         40    409.     2
# 6       19065     TRUE         42.2  432.     2

# I want to remove the Finishers who missed more than one checkpoint.
# A problem with the timing chips, possibly, or with the runners' start times.

# Have a a look at them first
results %>% 
  filter(n>1 & Finisher) %>%
  View()
# 510 results

# Now negate the filter to remove them.
results <- 
  results %>% 
  filter( ! (n>1 & Finisher))

# The remaining Finishers will have missed 0 or 1 checkpoint.
# Let's see what checkpoints were missed by them.
results %>% 
  group_by(Distance) %>% 
  filter(Finisher) %>% 
  summarise(Missing = sum(is.na(Time)))
#     A tibble: 6 x 2
#    Distance   Missing
#      <dbl>     <int>
# 1     10        44
# 2     20        18
# 3     21.1      64
# 4     30        15
# 5     40        10
# 6     42.2       0

# Lets have at all the times for the 44 runners with a missing 10K time.
results %>% 
  group_by(Race.Number) %>% 
  filter(Finisher,
         any(Distance == 10 & is.na(Time))) %>% 
  View()

# Some anomalous results. e.g. People missing the 10k checkpoint but reaching the 20k 
# in record breaking times and then taking another 3 hours to reach the finish line.
# They possibly started in an earlier wave than they were supposed to, messing up their
# times, or they joined the race later than the start line.

# Can see them better when the data is sorted.
# Will define a function because we'll use it for the other checkpoints as well.

missing.times <- function (d){
  return(
    results %>% 
      group_by(Race.Number) %>% 
      filter(Finisher,
             any(Distance == d & is.na(Time))) %>% 
      mutate(min.x = min(Time, na.rm = T)) %>% 
      arrange(min.x)
  )
}

View(missing.times(10))

# Visually scanning these, the weird results have 20k times that are less than 90 minutes.
# Can narrow the focus to these weird results with another filter
missing.times(10) %>% 
  filter(any(Distance == 20 & Time < 90)) %>% 
  View()
# 54 rows (9 runners)

# Can negate the filters used above and remove the 54 rows from the dataset.
results <- 
  results %>% 
  group_by(Race.Number) %>% 
  filter(! (Finisher &
           any(Distance == 10 & is.na(Time)) &
           any(Distance == 20 & Time < 90))
  )

View(missing.times(20))
# 108 rows, 18 runners.
# The only anomaly is the runner with a missing 20k time and a 10k time < 9 minutes.
# Removing:
results <- 
  results %>% 
  group_by(Race.Number) %>% 
  filter(! (Finisher &
            any(Distance == 20 & is.na(Time)) &
            any(Distance == 10 & Time < 9))
  )

# Runners with a missing 30k time
View(missing.times(30))
# 90 rows, 15 runners.
# No anomalies

# Runners with a missing 40k time
View(missing.times(40))
# 60 rows, 10 runners.
# Only weirdness is two runners who were going at a consistent pace to the 30k mark or beyond,
# but then dropped to way less than walking pace, and missed the 40k checkpoint.
# Removing:
results <- 
  results %>% 
  group_by(Race.Number) %>% 
  filter(! (Finisher &
           any(Distance == 40 & is.na(Time)) &
           any(Distance == 42.2 & Time > 345))
  )

# Let's have a look at the median times of the remaining runners. Mean times may not be 
# informative, because of outliers at either end of the spectrum.
# Also a column to show how the medians at each checkpoint relate to the medians at the
# previous and next checkpoints
results %>% 
  group_by(Distance) %>%
  summarise(median = median(Time, na.rm = T)) %>% 
  mutate(ratio = (median - lag(median, default = 0)) /
                 (lead(median) - lag(median, default = 0))) %>% 
  data.frame() # Convert to df so I can see the figures after decimal point.
#    Distance  median     ratio
# 1     10.0   56.2750  0.4999260
# 2     20.0  112.5667  0.9036789
# 3     21.1  118.5667  0.1016662
# 4     30.0  171.5833  0.4512057
# 5     40.0  236.0667  0.8192695
# 6     42.2  250.2917      NA

# Let's look again at what checkpoints were missed
results %>% 
  filter(Finisher) %>% 
  group_by(Distance) %>% 
  summarise(Missing = sum(is.na(Time)))
# A tibble: 6 x 2
# Distance Missing
# <dbl>   <int>
# 1     10        35
# 2     20        17
# 3     21.1      64
# 4     30        15
# 5     40         8
# 6     42.2       0

# Impute missing 10k times for Finishers.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 10 & Finisher,
                        0.499 * lead(Time),
                        Time))

# Impute missing 20k times.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 20 & Finisher,
                        lag(Time) + 0.904 * (lead(Time) - lag(Time)),
                        Time))

# Impute missing 21.1k times.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 21.1 & Finisher,
                        lag(Time) + 0.102 * (lead(Time) - lag(Time)),
                        Time))

# Impute missing 30k times.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 30 & Finisher,
                        lag(Time) + 0.451 * (lead(Time) - lag(Time)),
                        Time))

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 40 & Finisher,
                        lag(Time) + 0.819 * (lead(Time) - lag(Time)),
                        Time))

results %>% 
  filter(Finisher) %>% 
  group_by(Distance) %>% 
  summarise(Missing = sum(is.na(Time)))
#     A tibble: 6 x 2
#    Distance Missing
#      <dbl>     <int>
# 1     10         0
# 2     20         0
# 3     21.1       0
# 4     30         0
# 5     40         0
# 6     42.2       0

saveRDS(results, "results3.RDS") # saved at 18:58 18/11

# For the runners who didn't finish, I don't want gaps in their times.
# For example, they can miss the 30k, 40k, 42.2k checkpoint, but I'm going to delete
# them if they passed the 10k and 30k checkpoints but missed the 20k.

# To find them:
# We group_by() and arrange()
# Rows with NA times will automatically sort to the end of each group.
# Then we filter() for groups where any() row is out of order.
results %>% 
  group_by(Race.Number) %>% 
  arrange(Race.Number,Time) %>% 
  filter(any(Distance < lag(Distance, default = 0))) %>% 
  View()
# 138 rows; 23 runners.

# Negating the filter to remove the unwanted groups:
results <-   
  results %>% 
  group_by(Race.Number) %>% 
  arrange(Race.Number,Time) %>% 
  filter(! (any(Distance < lag(Distance, default = 0)))) %>% 
  ungroup()
  
# results %>% 
#   drop_na(Time) %>% 
#   group_by(Race.Number) %>%
#   filter(Distance == max(Distance)) %>%
#   ungroup() %>% 
#   group_by(Distance) %>% count()