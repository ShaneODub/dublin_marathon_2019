library(dplyr)
library(tidyr)
library(stringr)
library(hms)

setwd("C:/Users/Shane/Projects/marathon")

# Loading the list.of.dataframes that was previously scraped from 
# the marathon results website

list.of.dataframes <- readRDS(choose.files())

results <- bind_rows(list.of.dataframes)
# Didn't work: "Error: Column `Gender.Position` can't be converted from integer to character"

# First, lapply to mutate_if over the list and then bind_rows
results <- 
  list.of.dataframes %>% 
  lapply(., mutate_if, is.integer, as.character) %>% 
  bind_rows() %>% 
  as_tibble()

# Keeping just the columns I plan to use
results <- select(results,1,4,6,10,12,14,16,18,20,22,21)

results %>% View()

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

# 'Category' column contains Gender info, and Age info. We already have a Gender column.
unique(results$Category)
# [1] "MS"   "M45"  "M35"  "M40"  "FS"   "M50"  "F35"  "F40"  "M65"  "F45"  "M55" 
# [12] "M60"  "MU19" "F50"  "F55"  "F60"  "F65"  "M70"  "FU19" "M75"  "F75"  "F70" 
# [23] "M80"

results$Category <- 
  results$Category %>% 
  str_remove("[MF]") %>%               # Remove all the M for Male and F for Female.
  str_replace("U19","18") %>%          # Change U19 (under 19) to 18.
  str_replace("S","20")                # Change S (senior) to 20.

unique(results$Category)
#  [1] "20" "45" "35" "40" "50" "65" "55" "60" "18" "70" "75" "80"

# Give the 'Category' column a more meaningful name, 'Age.Bracket'
results <- 
  results %>% 
  rename(Age.Bracket = Category)

# The Overall.Position column contains finishing place, and a 'DNF' for those who didn't finish.
# Creating a logical column with TRUE or FALSE to indicate whether the runner finished.
results <-   
  results %>% 
  mutate(Finisher = (Overall.Position != "DNF")) %>% 
  select(1,2,3,4,7,5,6)

# In the original wide data we had a Gun.Time and Chip.Time as finishing times.
# I want to use Chip.Time but there are some missing values. Will replace from Gun.Time

# The distance variable contains the old column names from the original wide format.
# We can make them more meaningful.

results$Distance <- 
  results$Distance %>% 
  str_remove_all("[XK]") %>%
  str_replace("Halfway","21.1") %>%           # The halfway point is at 21.1 kilometers.
  str_replace("Gun.Time","42.2") %>%          # Finish point is at ca. 42.2 kilometers
  str_replace("Chip.Time","42.195")           # Differentiate Gun.Time & Chip.Time for now.

# Now to assign an appropriate type to each variable

# Race.Number (in theory) is based on predicted time when signing up for the race, so 
# choosing as.numeric() for this rather than factor or character.

# as.numeric() on a couple of columns will cause some 'DNF' entries to coerce to 'NA' 
# but we already captured the did-not-finsh information in the 'Finisher' column.

results <-   
  results %>% 
  mutate(Race.Number = as.numeric(Race.Number),
         Gender = as.factor(Gender),
         Age.Bracket = as.factor(Age.Bracket), # For now. Maybe as.numeric() later.
         Overall.Position = as.numeric(Overall.Position),
         Distance = as.numeric(Distance),      # Distances are expressed in kilometers
         Time = as.numeric(as_hms(Time))/60)   # Times are now expressed in minutes.

results[results$Distance > 42,] <- 
  results %>%
  filter(Distance > 42) %>% 
  fill(Time) %>%  View() # <<<<< Finished here
          
  

glimpse(results)
