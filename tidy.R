library(dplyr)
library(tidyr)
library(stringr)
library(hms)
library(magrittr)
library(ggplot2)
library(tweenr)
install.packages("gganimate")
library(gganimate)

setwd("C:/Users/Shane/Projects/marathon")

# Loading the list.of.dataframes that was previously scraped from 
# the marathon results website

list.of.dataframes <- readRDS("full_scrape_list_of_dataframes.rds")

results <- bind_rows(list.of.dataframes)
# Didn't work: "Error: Column `Gender.Position` can't be converted from integer to character"

# First, lapply to mutate_if over the list and then bind_rows
results <- 
  list.of.dataframes %>% 
  lapply(., mutate_if, is.integer, as.character) %>% 
  bind_rows() %>% 
  as_tibble()

# Visual inspection
View(results)
# Keeping just the columns I plan to use
results <- select(results,1,4,6,10,12,14,16,18,20,22,21)
#
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

# Other missing values from runners who finished:
results %>% 
  group_by(Distance) %>%
  filter(is.na(Time) & Finisher) %>% 
  count()
# A tibble: 5 x 2
# Groups:   Distance [5]
#    Distance   n
#     <dbl>   <int>
# 1     10     100
# 2     20      75
# 3     21.1   127
# 4     30      68
# 5     40      40

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

# Going to temporarily join the count (n) to the results tibble
results <- 
left_join(results, 
          results %>% 
            group_by(Race.Number) %>%
            filter(is.na(Time)) %>% 
            count(),
          by = "Race.Number")

results$n[is.na(results$n)] <- 0

# Showing one runner as an example:
results %>% filter(Race.Number == 19596) %>% select(5:8)
# Missing times from 10,20,21.1 & 42.2 checkpoint, so n = 4 in the rightmost column.
# A tibble: 6 x 4
# Finisher   Distance   Time     n
#   <lgl>       <dbl>   <dbl>   <dbl>
# 1 FALSE        10      NA       4
# 2 FALSE        20      NA       4
# 3 FALSE        21.1    NA       4
# 4 FALSE        30      16.4     4
# 5 FALSE        40     113.      4
# 6 FALSE        42.2    NA       4

# I want to remove runners where n > 1 and they're marked as a Finisher.
# A problem with the timing chips, possibly, or with the runners' start times.

# Have a a look at them first
results %>% 
  filter(n>1 & Finisher) %>%
  View()
# 510 results

# Now negate the filter to remove them.
results <- 
  results %>% 
  filter(!
           (n>1 & Finisher)
         )

# Let's temporarily place the runners who didn't finish into another tibble

results.dnf <-
  results %>% 
  filter(!Finisher)

results <- 
  results %>% 
  filter(Finisher)
# Now the results table has people who finished the race, and who have 0 or 1 missing times.

# Have a a look at the runners who missed 1 checkpoint
results %>% 
  filter(n == 1) %>%
  View()
# 906 results

# Let's see what checkpoints were missed
results %>% 
  group_by(Distance) %>% 
  summarise(Missing = sum(is.na(Time)))
# A tibble: 6 x 2
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
  filter(any(Distance == 10 & is.na(Time))) %>% 
  View()

# There's some weird results. People missing the 10k checkpoint but reaching the 20k in
# record breaking times and then taking another 3 hours to reach the finish line.
# They possibly started in an earlier wave than they were supposed to, messing up their times,
# or they joined the race later than the start line.

# Can see them better when the data is sorted.
# Will define a function because we'll use it for the other checkpoints as well.

missing.times <- function (d){
  return(
    results %>% 
      group_by(Race.Number) %>% 
      filter(any(Distance == d & is.na(Time))) %>% 
      mutate(min.x = min(Time, na.rm = T)) %>% 
      arrange(min.x)
  )
}

View(missing.times(10))

# Visually scanning these, the weird results have 20k times that are less than 90 minutes.
# Can narrow the focus to the weird results with another filter
missing.times(10) %>% 
  filter(any(Distance == 20 & Time < 90)) %>% 
  View()
# 54 rows (9 runners)

# Can negate the filters used above and remove the weird results from the dataset.
results <- 
  results %>% 
  group_by(Race.Number) %>% 
  filter(!
           (any(Distance == 10 & is.na(Time)) &
            any(Distance == 20 & Time < 90))
         )

# Runners with a missing 20k time
View(missing.times(20))
# 108 rows, 18 runners.
# The only anomaly is the runner with a missing 20k time and a 10k time of 8 minutes.
# Removing:
results <- 
  results %>% 
  group_by(Race.Number) %>% 
  filter(!
           (any(Distance == 20 & is.na(Time)) &
            any(Distance == 10 & Time < 10))
  )

# Runners with a missing 30k time
View(missing.times(30))
# 90 rows, 15 runners.
# No weirdness.

# Runners with a missing 40k time
View(missing.times(40))
# 60 rows, 10 runners.
# Only weirdness is two runners who were going at a consistent pace to the 30k mark or beyond,
# but then dropped to way less than walking pace, and missed the 40k checkpoint.
# Deleting:
results <- 
  results %>% 
  group_by(Race.Number) %>% 
  filter(!
           (any(Distance == 40 & is.na(Time)) &
            any(Distance == 42.2 & Time > 345))
  )

# Let's look again at what checkpoints were missed
results %>% 
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

# Let's have a look at the median times of the remaining runners. Mean times may not be 
# informative, because of outliers at either end of the spectrum.

results %>% 
  group_by(Distance) %>%
  summarise(m = median(Time, na.rm = T))

# A tibble: 6 x 2
#    Distance  m
#      <dbl>  <dbl>
# 1     10    56.3
# 2     20    113. 
# 3     21.1  119. 
# 4     30    172. 
# 5     40    236. 
# 6     42.2  250.

# Imputing missing times.

# Impute missing 10k times.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 10,
                        0.498 * lead(Time),
                        Time))

# Impute missing 20k times.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 20,
                        lag(Time) + 0.904 * (lead(Time) - lag(Time)),
                        Time))

# Impute missing 21.1k times.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 21.1,
                        lag(Time) + 0.101 * (lead(Time) - lag(Time)),
                        Time))

# Impute missing 30k times.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 30,
                        lag(Time) + 0.452 * (lead(Time) - lag(Time)),
                        Time))

# Impute missing 40k times.

results <- 
  results %>% 
  mutate(Time = if_else(is.na(Time) & Distance == 40,
                        lag(Time) + 0.82 * (lead(Time) - lag(Time)),
                        Time))

saveRDS(results, "results.RDS") # saved at 12.35 13/11

# Defining a function for 5-number summary
fiver <- function(df){
  df %>% 
    group_by(Distance) %>% 
    summarise(min = min(Time),
              q1 = quantile(Time, probs = .25),
              median = median(Time),
              mean = mean(Time),
              q3 = quantile(Time, probs = .75),
              max = max(Time),
              sd = sd(Time)) %>% 
    as.data.frame() %>% 
    round(2)
}
 
fiver(results) 
#    Distance  min     q1  median   mean    q3    max    sd
# 1     10.0   2.93  50.97  56.27  57.50  62.92 130.90  9.85
# 2     20.0  61.83 101.37 112.57 115.24 126.18 238.65 20.63
# 3     21.1  65.08 106.82 118.58 121.53 133.22 251.31 21.99
# 4     30.0  92.52 154.58 171.60 177.30 195.11 364.00 34.11
# 5     40.0 121.70 210.39 236.07 242.91 269.23 496.17 48.11
# 6     42.2 128.10 222.99 250.29 257.08 284.82 520.35 50.81

# There's some weirdness going on with the 10k times. The min is 2.9 minutes.
# Having a closer look in the View window.
results %>% 
  group_by(Race.Number) %>%  
  mutate(min.x = min(Time, na.rm = T)) %>% 
  arrange(min.x) %>% 
  View()

# Am going to examine the differences in the 10k splits to see if there's 
# an efficient way to spot anomalies
splits <- 
  results %>% 
  group_by(Race.Number) %>% 
  filter(Distance %in% c(10,20,30,40)) %>% 
  mutate(Time = Time - lag(Time, default = 0))%>% 
  select(1,2,3,4,6,7) 

View(splits)

fiver(splits)
#   Distance   min  q1   median  mean  q3    max    sd
# 1       10  2.93 50.97  56.27 57.50 62.92 130.90  9.85
# 2       20 29.97 50.38  56.13 57.73 63.15 118.27 10.98
# 3       30 30.63 52.33  59.17 62.07 69.25 138.15 14.12
# 4       40 29.18 54.77  63.43 65.61 74.43 147.08 15.12

splits <-   
  splits %>% 
  group_by(Race.Number) %>%
  mutate(min.x = min(Time),
         sd.x = sd(Time)) %>% 
  arrange(desc(sd.x))

View(splits)
# Sorting the runners according to the standard deviation of their splits shows
# six runners whose times don't make sense. Removing these.
results <-
  results %>% 
  filter(!
           (Race.Number %in% c(16391,20283,20201,21318,21319,18108))
         )
# Removed from splits also.
splits <-
  splits %>% 
  filter(!
           (Race.Number %in% c(16391,20283,20201,21318,21319,18108))
  )

splits %>% 
  group_by(Gender) %>% 
  summarise(sd = mean(sd.x))
# A tibble: 2 x 2
# Gender    sd
# <fct>  <dbl>
# 1 Female  4.30
# 2 Male    4.56

results %>%
  filter(Distance == 42.2) %>%
  ggplot(mapping = aes(Time)) +
  geom_density() +
  theme_minimal()
  
saveRDS(results, "results.RDS") # saved 31/11 12.38
results <- readRDS("results.RDS")

results <- ungroup(results) # results was group_by Race.Number since earlier in the script.

results %>% 
  group_by(Gender) %>% 
  summarise(count = n_distinct(Race.Number))
# # A tibble: 2 x 2
#   Gender count
#   <fct>  <int>
# 1 Female  6542
# 2 Male   11078


# Assigning an age to every runner, randomly distributed within their age category.
# This will be useful to avoid overplotting on the final plot.

gap  <-  0.2

random_age <- function(Age.Bracket, Gender){
  
  if (Age.Bracket >= 35) {     
    half.bracket <- 2.5             
  } else if (Age.Bracket == 20) {
    half.bracket <- 7.5
  } else {
    half.bracket <- 1
  }
  
  if (Gender == "Male") {
    return(Age.Bracket + runif(1, gap, half.bracket - gap))
  } else {
    return(Age.Bracket + runif(1, half.bracket + gap, 2 * half.bracket - gap))
  }
}

results <- 
  results %>%
  group_by(Race.Number) %>% 
  mutate(Age = random_age(Age.Bracket, Gender)) %>% 
  ungroup()

results <- select(results, 1,2,3,9,4,5,6,7)

p <- 
  results %>% 
  ggplot(mapping = aes(Time, Age, col = Gender)) +
  geom_point()

p

anim <- p + transition_ (Distance, transition_length = 2, state_length = 1)

results <- 
  results %>% 
  group_by(Race.Number) %>%
  mutate(Prev.Ckpt = lag(Distance, default = 0),
         Prev.Time = lag(Time, default = 0),
         KmPerMin = Time/Distance) %>% 
  ungroup()

newrows <- 
  results %>% 
  select(1:6) %>% 
  distinct() %>% 
  slice(rep(1:n(), each = 34)) %>% 
  mutate(Distance = NA,
         Time = rep(seq(15,510, by = 15),17620),
         Prev.Ckpt = NA,
         Prev.Time = NA,
         KmPerMin = NA)

View(newrows)

results2 <- 
  results %>% 
  rbind(newrows) %>% 
  arrange(Overall.Position, Time) %>% 
  group_by(Overall.Position) %>% 
  fill(Prev.Ckpt, .direction = "up") %>% 
  fill(Prev.Time, .direction = "up") %>% 
  fill(KmPerMin, .direction = "up")

results2 <-   
  results2 %>% 
  mutate(Distance = if_else(is.na(Distance),
                            Prev.Ckpt + (Time - Prev.Time)/KmPerMin,
                            Distance))

p <- 
  results2 %>%
  ggplot(mapping = aes(Distance, Age, col = Gender)) +
  geom_point(size = 0.4) + 
  xlim(0,42.2)

p

anim <- p + transition_states(Time)
anim
