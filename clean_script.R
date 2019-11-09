library(dplyr)
library(tidyverse)
library(magrittr)
library(hms)

library(tibble)

library(lubridate)
install.packages("hms")


library(xml2)
library(rvest)
library(plyr)


setwd("C:/Users/Shane/Projects/marathon")

## Get the data

# Full results for the Dublin Marathon were scraped from tables on a website. (180 tables with 100 entrants on each table.) Tables were compiled into a list called 'list.of.dataframes'.
# 
# ```{r Scraping the data}

# Set up an empty list to store our downloaded tables.
list.of.dataframes <- vector("list",180)

# Store the URL in a variable, minus the page number:
url <- "https://www.tdleventservices.co.uk/event-results/events?event=3492&page="

# Now we loop through 180 pages; add each page number to the URL; extract and store the data.
page <- 177
while (page <=180){
  list.of.dataframes[[page]] <-
    paste(url,page,sep="") %>%
    read_html() %>%                                                                     # xml2
    html_nodes(xpath='/html/body/div[2]/div/div/div/div/div[2]/div[2]/table') %>%       # rvest
    html_table() %>%                    
    data.frame()
  page <- page + 1
  Sys.sleep(5) # Pause 45s after downloading each table, to avoid being mistaken for mischief-maker.
}

list.of.dataframes <- readRDS(choose.files())
# Once all are downloaded, bind all the results into a single dataframe.


all.results <- bind_rows(list.of.dataframes)
# Didn't work: "Error: Column `Gender.Position` can't be converted from integer to character"

# First, lapply to mutate_if over the list and then bind_rows
all.results <- 
  list.of.dataframes %>% 
  lapply(., mutate_if, is.integer, as.character) %>% 
  bind_rows()

View(all.results)

# Keeping just the columns I plan to use
all.results <- select(all.results,1,4,6,10,12,16,18,20,22,21)

all.results <- as_tibble(all.results)
glimpse(all.results)
View(all.results)

unique(all.results$Category)
# [1] "MS"   "M45"  "M35"  "M40"  "FS"   "M50"  "F35"  "F40"  "M65"  "F45"  "M55"  "M60" 
# [13] "MU19" "F50"  "F55"  "F60"  "F65"  "M70"  "FU19" "M75"  "F75"  "F70"  "M80" 

# Remove the gender info from category, and tidy it a bit.
all.results$Category <- 
  all.results$Category %>% 
  str_remove("[MF]") %>%
  str_replace("U19","18") %>% 
  str_replace("S","20")

all.results  <- 
  rename(all.results, c("Category" = "Age.Bracket"))

# Check the Age.Bracket column now:
unique(all.results$Age.Bracket)
# [1] "20" "45" "35" "40" "50" "65" "55" "60" "18" "70" "75" "80"

# Change all the time columns from strings to hms. It will NA the blanks at the same time.
all.results[,4:9] <- lapply(all.results[,4:9], as_hms)

# Now convert them to numerics, specifically minutes
all.results[,4:9] <- 
  all.results[,4:9]  %>%
  lapply(as.numeric) %>%
  lapply("/",60) 

# Gender and Age.Bracket column as factors
all.results$Gender <- as.factor(all.results$Gender)
all.results$Age.Bracket <- as.factor(all.results$Age.Bracket)

glimpse(all.results)
# Observations: 17,931
# Variables: 10
# $ Race.Number      <chr> "14", "25", "3", "1", "8", "24", "5", "...
# $ Gender           <fct> Male, Male, Male, Male, Male, Male, Mal...
# $ Age.Bracket      <fct> 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,...
# $ X10K             <time> 00:31:54, 00:31:53, 00:31:53, 00:31:54...
# $ X20K             <time> 01:01:53, 01:03:30, 01:01:53, 01:01:52...
# $ X30K             <time> 01:32:31, 01:35:00, 01:32:31, 01:33:17...
# $ X40K             <time> 02:01:42, 02:05:18, 02:04:49, 02:05:41...
# $ Gun.Time         <time> 02:08:06, 02:12:01, 02:12:05, 02:13:02...
# $ Chip.Time        <time> 02:08:06, 02:12:01, 02:12:04, 02:13:01...
# $ Overall.Position <chr> "1", "2", "3", "4", "5", "6", "7", "8",...

### Missing values

# There are no missing values for Race.Number, Gender, Age.Bracket
# or Overall.Position

# When deciding what to do with runners who have missing times, I'm going to take
# into account how many other times are missing.

# Create a function count_na to count the NAs across the checkpoint columns...
count_na <- function(x) sum(is.na(x[4:7]))
# ...and put the results in a new column callled count_na
all.results <- 
  all.results %>%
  mutate(count_na = apply(., 1, count_na))

# Let's start with the 10k checkpoint times
all.results %>%
  filter(is.na(X10K)) %>%
  View()
# 149 results.

# Filter again, to rows where runners missed 10k checkpoints and between 1 and 2 other checkpoints.
all.results %>% 
  filter(is.na(X10K) & count_na %in% c(2,3)) %>% 
  View()
# 57 results

# Negate the filter to remove the runners who missed the 10k checkpoint 
# and 1 or 2 other checkpoints.
all.results <- 
all.results %>% 
  filter(!
           (is.na(X10K) & (count_na %in% c(2,3)))
         )

# Having a look at runners who missed 20k chkpt and 1 more chkpt:
all.results %>% 
  filter(is.na(X20K) & (count_na == 2)) %>% 
  View()
# 7 results: some of them don't make much sense:
#     X10K    X20K  X30K    X40K    Gun.Time  Chip.Time count_na
# 1 00:01:19	 NA	   NA	  03:07:23	03:20:26	   NA	       2
# 2	00:47:33	 NA	   NA	  03:20:50	03:35:10	03:34:10	   2
# 3	00:47:17	 NA	   NA	  03:20:31	03:35:36	03:34:38	   2
# 4	00:22:26	 NA	   NA	  04:05:47	04:25:06	   NA	       2
# 5	01:00:44	 NA	   NA	  04:28:32	04:48:11	04:45:29	   2
# 6	01:03:08	 NA	   NA	  04:34:16	04:51:51	04:50:54	   2
# 7	00:57:50	 NA	   NA	  03:56:47	   NA	       NA	       2

# There's just a few. Getting rid.
all.results <- 
  all.results %>% 
  filter(!
           (is.na(X20K) & (count_na == 2))
         )

# Having a look at runners who missed 30k chkpt and 40k chkpt:
all.results %>% 
  filter(is.na(X30K) & (count_na == 2)) %>% 
  View()
# 82 results

# A lot of these are just people who didn't finish. We want to keep those for now.
# Want to get rid of people with finish times, who missed the 30k & 40k chkpts.
# Just filter first, to have a look at them:
all.results %>% 
  filter(is.na(X30K) & (count_na == 2) & Overall.Position != "DNF") %>% 
  View()
# 13 runners.

#Getting rid of these:
all.results <- 
  all.results %>% 
  filter(!
           (is.na(X30K) & (count_na == 2) & Overall.Position != "DNF")
         )

# One more thing. People who missed 2 or 3 checkpoints but have a finish time:
all.results %>% 
  filter(!is.na(Gun.Time) & (count_na >= 2)) %>% 
  View()
# 13 rows

# Can't trust these results. Getting rid.
all.results <- 
  all.results %>% 
  filter(!
           (!is.na(Gun.Time) & (count_na >= 2))
         )

# We're missing some Chip.Times
all.results %>% 
  filter(is.na(Chip.Time)) %>% 
  View()

# In some cases it's because they didn't finish. Filter without the DNF runners.
all.results %>% 
  filter(is.na(Chip.Time) & Overall.Position != "DNF") %>% 
  View()
# 81 results.

# I'm setting Chip.Time equal to Gun.Time for all runners who are missing a Chip.Time.
all.results$Chip.Time[is.na(all.results$Chip.Time)] <- 
  all.results$Gun.Time[is.na(all.results$Chip.Time)]

# Now we have 17,841 runners from the original 17,931 (?)
# Many didn't finish, so we have checkpoint results for 0 to 4 checkpoints.
# Of those who finished, some are missing 1 checkpoint out of 4, which we're going to impute.

glimpse(all.results)

# Have a look at the rows where only the 10K checkpoint is blank
all.results %>% 
  filter(is.na(X10K) & count_na == 1) %>% 
  View()
# 54 results

summary(all.results %>% 
          filter(count_na == 1) %>% 
          select(4:9))
#      X10K              X20K             X30K             X40K          Gun.Time       Chip.Time    
# Min.   :  8.717   Min.   : 25.20   Min.   : 94.93   Min.   :180.6   Min.   :185.7   Min.   :185.6  
# 1st Qu.: 51.300   1st Qu.: 98.32   1st Qu.:155.05   1st Qu.:226.8   1st Qu.:238.9   1st Qu.:238.0  
# Median : 56.300   Median :111.88   Median :173.20   Median :251.1   Median :272.1   Median :270.6  
# Mean   : 57.270   Mean   :111.85   Mean   :183.80   Mean   :271.6   Mean   :290.9   Mean   :288.7  
# 3rd Qu.: 61.267   3rd Qu.:123.60   3rd Qu.:197.49   3rd Qu.:294.2   3rd Qu.:325.9   3rd Qu.:324.2  
# Max.   :115.150   Max.   :233.42   Max.   :349.60   Max.   :496.2   Max.   :520.7   Max.   :520.4  
# NA's   :54        NA's   :23       NA's   :16       NA's   :46      NA's   :38      NA's   :38                       



# Impute 10k times by multiplying the relevant 20k time by 0.503.
# The 0.503 is calculated from the medians in the summary table above.
all.results <- 
  all.results %>%
  mutate(X10K = if_else(is.na(X10K) & count_na == 1, X20K*0.503, X10K))

# Impute missing 20k times from the 10k & 30k times.
# The 20k times should be below he midpoint of these two times.
all.results <- 
  all.results %>%
  mutate(X20K = if_else(is.na(X20K) & count_na == 1, X10K + 0.48*(X30K - X10K), X20K))

# Impute missing 30k times from the 20k & 40k times.
# The 30k times should be below he midpoint of these two times.
all.results <- 
  all.results %>%
  mutate(X30K = if_else(is.na(X30K) & count_na == 1, X20K + 0.44*(X40K - X20K), X30K))

# Impute missing 40k times from the Chip.Time & 30k times.
# Have to be careful here not to alter the DNF runners
all.results <- 
  all.results %>%
  mutate(X40K = if_else(is.na(X40K) & count_na == 1 & Overall.Position != "DNF",
                        X30K + 0.79*(Chip.Time - X30K),
                        X40K))

# Now the only missing times are from people who didn't finish.

# Going to plit out the 'DNF' info to another column
all.results <- 
  all.results %>% 
  mutate(Finisher = if_else(Overall.Position =="DNF",
                            "No",
                            "Yes"))

# Convert the Overall.Position column to numeric
all.results$Overall.Position <- as.numeric(all.results$Overall.Position)

saveRDS(all.results, file = "all.results.rds") # just in case I mess up

# Questions to answer:
# How does pace vary over time?
# Is the answer to this question dependent on any of the following:
#             Gender, Age.Bracket, Overall.Position.

splits <- 
  all.results %>% 
  transmute(split_1 = X10K,
            split_2 = X20K - X10K,
            split_3 = X30K - X20K,
            split_4 = X40K - X30K,
            Gender,
            Age.Bracket,
            Finisher)

summary(splits)
glimpse(splits)

qqnorm(splits[,1:4])

pairs(splits[,12:15])
cor(splits[,12:15], use = "complete.obs")

var.test()

boxplot(splits[,12:15])

ggplot(splits, mapping = aes(split_3,split_4, colour = Gender)) + 
  geom_point(size = 0.4, na.rm = T) +
  ylim(0,150) +
  xlim(0,150) +
  geom_segment(aes(0,0,xend = 150, yend = 150))
