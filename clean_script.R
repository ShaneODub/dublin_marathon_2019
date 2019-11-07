library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)
install.packages("hms")
library(hms)
install.packages("convert")
library(convert)

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

all.results %>% distinct(Category)

# Remove the gender info from category, and tidy it a bit.
all.results$Category <- 
  all.results$Category %>% 
  str_remove("[MF]") %>%
  str_remove("[U]") %>% 
  str_replace("19","18") %>% 
  str_replace("S","20") %>% 
  as.numeric()

# Change all the time columns to hms. It will NA the blanks at the same time.
all.results[,4:9] <- lapply(all.results[,4:9], as_hms)

# Gender column as category
all.results$Gender <- as.factor(all.results$Gender)

### Missing values

# There are no missing values for Race.Number, Gender, or Category( i.e. age bracket)

# Let's start with the 10k checkpoint times # <<<<<< Left off here.
all.results %>% filter(is.na(X10K)) %>% View()
# 149 entries in total. Most of them missed more than one checkpoint. I'm just going to delete all.
all.results <- filter(all.results, !is.na(all.results$X10K)) %>% View()
  
# There are no blanks in the Gun.Time, so will use these to fill in the blanks in the Chip.Time
all.results$Chip.Time[all.results$Chip.Time == ""] <- 
  all.results$Gun.Time[all.results$Chip.Time == ""]

all.results %>% 
  filter(Overall.Position != "DNF") %>% 
  View()

# Convert the timestamp columns to POSIXct format
all.results.df[,4:9] <- data.frame(apply(all.results.df[,4:9],2,as.POSIXct, format = "%H:%M:%S"))
# Convert it to numeric and subtract today's date (lubridate package)
all.results.df[,4:9] <- data.frame(apply(all.results.df[,4:9],2,as.numeric))- as.numeric(as.POSIXct(today()))
# Convert from seconds to minutes for readability
all.results.df[,4:9] <- all.results.df[,4:9]/60
str(all.results.df)

View(all.results.df[is.na(all.results.df$X10K),])
# There are 122 runners with no time recorded for the 10k checkpoint. Scrolling through 
# them on the viewer, many of them are missing other checkpoints as well, or have implausible times
# at the later checkpoints. We won't lose anything by deleting these.
all.results.df <- all.results.df[!is.na(all.results.df$X10K),]

# There are also 8 runners whose 10k times are very low, but their later times are very high.
# There's something wrong with these 10k times, so deleting these rows also.
all.results.df <- all.results.df[all.results.df$X10K > 30,]

View(all.results.df[is.na(all.results.df$X20K),])
# The guys with no 20k time are not worth keeping; deleting, except for the DNF runners.
all.results.df <- all.results.df[!is.na(all.results.df$X20K) | all.results.df$Overall.Position == "DNF",]

View(all.results.df[is.na(all.results.df$X30K),])
# The guys with no 20k time are not worth keeping; deleting, except for the DNF runners.
all.results.df <- all.results.df[!is.na(all.results.df$X30K) | all.results.df$Overall.Position == "DNF",]

View(all.results.df[is.na(all.results.df$X40K) & all.results.df$Overall.Position!="DNF",])
# There's only a few runner who are missing 40k times, and some of them are worth keeping
# Interpolating their 40k times based on their 30k times, and finish-line times.
all.results.df[is.na(all.results.df$X40K) & all.results.df$Overall.Position!="DNF","X40K"] <-
  all.results.df[is.na(all.results.df$X40K) & all.results.df$Overall.Position!="DNF","X30K"] +
  (all.results.df[is.na(all.results.df$X40K) & all.results.df$Overall.Position!="DNF","Chip.Time"] -
     all.results.df[is.na(all.results.df$X40K) & all.results.df$Overall.Position!="DNF", "X30K"]) * 10.0/12.2

# There are 66 runners who have no chip time, but have a gun-time.
View(all.results.df[is.na(all.results.df$Chip.Time) & all.results.df$Overall.Position!="DNF",])
# Will just make their chip-time their gun-time.
all.results.df[is.na(all.results.df$Chip.Time) & all.results.df$Overall.Position!="DNF","Chip.Time"] <- 
  all.results.df[is.na(all.results.df$Chip.Time) & all.results.df$Overall.Position!="DNF","Gun.Time"]

# There are a few runners who passed the 40k line but didn't pass the finish line.
View(all.results.df[!is.na(all.results.df$X40K) & is.na(all.results.df$Chip.Time),])
# Let's give them a gun-time:
all.results.df[!is.na(all.results.df$X40K) & is.na(all.results.df$Chip.Time),"Gun.Time"] <- 
  all.results.df[!is.na(all.results.df$X40K) & is.na(all.results.df$Chip.Time),"X30K"] +
  (all.results.df[!is.na(all.results.df$X40K) & is.na(all.results.df$Chip.Time),"X40K"] -
     all.results.df[!is.na(all.results.df$X40K) & is.na(all.results.df$Chip.Time),"X30K"]) * 12.2/10
# And a chip time:
all.results.df[!is.na(all.results.df$X40K) & is.na(all.results.df$Chip.Time),"Chip.Time"] <- 
  all.results.df[!is.na(all.results.df$X40K) & is.na(all.results.df$Chip.Time),"Gun.Time"]
# And remove the DNF from their overall position column:
all.results.df[all.results.df$Race.Number %in% c(20714,7858,10141,18002),"Overall.Position"] <- "unknown"

all.results.df <- all.results.df[all.results.df$Race.Number != "10492",] # anomaly
all.results.df <- all.results.df[all.results.df$Race.Number != "16339",] # heart attack

# Let's see where we are:
str(all.results.df)

saveRDS(all.results.df, file = "all.results.df") # save it as a precaution

# Change the Gender & Age category columns from strings to factors
all.results.df[,2:3] <- lapply(all.results.df[,2:3], as.factor)

# Remove the gender info from category, and tidy it a bit.
all.results.df$Category <- gsub("F","", all.results.df$Category)
all.results.df$Category <- gsub("M","", all.results.df$Category)
all.results.df$Category <- gsub("U19","18", all.results.df$Category)
all.results.df$Category <- gsub("S","20", all.results.df$Category)
# all.results.df$Category <- as.numeric(all.results.df$Category) # maybe later
