library("dplyr")
library("lubridate")
install.packages("car")
library(car)
library(ggplot2)
library(gganimate)
install.packages("move")
library(move)
install.packages("animation")
library(animation)
install.packages("data.table")
library(data.table)
library(tidyverse)

install.packages("magrittr")
library(magrittr)
library(xml2)
library(rvest)
install.packages("naniar")
library(naniar)
install.packages("chron")
library("chron")
library(plyr)

setwd("C:/Users/Shane/projects/marathon")
getwd()

url <- "https://www.tdleventservices.co.uk/event-results/events?event=3492&page="

list.of.dataframes <- vector("list",180)
page <- 131
while (page <=180){
  list.of.dataframes[[page]] <-
    paste(url,page,sep="") %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/div/div/div[2]/div[2]/table') %>% 
    html_table() %>%
    data.frame()
  page <- page + 1
  Sys.sleep(28)
}

bigdf <- bind_rows(list.of.dataframes)
View(bigdf)


######################################## Rough work below this line

# downloaded positions 1 to 18000; 
saveRDS(list.of.dataframes, file = "listofdataframes18000.rds") # saved all 18000
saveRDS(bigdf, file = "bigdf18000.rds") # saved all 18000
saveRDS(smalldf, file = "smalldf.rds") # saved smalldf just in case; last 3 pages from site;178:180
saveRDS(frames_df,file = "frame_df.rds") # 05/11/2019 00:08
saveRDS(frames_df,file = "frame_df_2.rds") # 05/11/2019 17:02
frames_df <-  readRDS(choose.files())

list.of.dataframes <- readRDS(file.choose())
bigdf <- readRDS(choose.files())
View(bigdf)
str(bigdf)

list.of.dataframes[80:81]# downloaded positions 1 to 8000; 
saveRDS(list.of.dataframes, file = "listofdataframes.rds")
list.of.dataframes <- readRDS(file.choose())
list.of.dataframes[80:81]

smalldf <- bind_rows(list.of.dataframes[178:180]) # didn't work
smalldf <- rbind.fill(list.of.dataframes[178:180])
View(smalldf)
is.na(smalldf) <- smalldf == "DNF" # replace all the DNF with NA so I can as.numeric the columns
str(smalldf)
smalldf[,c(5,7,11,13,15,17,19,21,23)] <- data.frame(apply(smalldf[,c(5,7,11,13,15,17,19,21,23)],2,as.numeric)) # from chr to num
bigdf <- rbind(bigdf,smalldf)


df_copy <- bigdf
View(df_copy)
str(df_copy)

frames_df <- select(df_copy,Race.Number,Gender,Category,X10K,X20K,X30K,X40K,Gun.Time,Chip.Time)
str(frames_df)

# Apply difftime columnwise # most displays are hours; column 4 is minutes
# frames_df[,4:9] <- data.frame(apply(frames_df[,4:9],2,as.difftime))
# frames_df[,4:9] <- data.frame(apply(frames_df[,4:9],2,as.double,units = "secs"))
# scrap this, trying date-time and then numeric instead

# Apply as.POSIXct columnwise
frames_df[,4:9] <- data.frame(apply(frames_df[,4:9],2,as.POSIXct, format = "%H:%M:%S"))
# Convert it to numeric and subtract today's date from it
frames_df[,4:9] <- data.frame(apply(frames_df[,4:9],2,as.numeric))- as.numeric(as.POSIXct(today()))
# Convert from seconds to minutes for readability
frames_df[,4:9] <- frames_df[,4:9]/60
str(frames_df)

# Convert the Gender and Category columns to factors.
frames_df[,2:3] <- data.frame(apply(frames_df[,2:3],2,as.factor))
View(frames_df)
summary(frames_df)

summary(frames_df[frames_df$Gender == "Female",])

table(frames_df$Gender,frames_df$Category)

frames_df <- frames_df[complete.cases(frames_df[,4:9]),]

frames_df$split_1 <- frames_df$X10K
frames_df$split_2 <- frames_df$X20K - frames_df$X10K
frames_df$split_3 <- frames_df$X30K - frames_df$X20K
frames_df$split_4 <- frames_df$X40K - frames_df$X30K

boxplot(frames_df$Chip.Time)
hist(frames_df$Chip.Time)
boxplot(Chip.Time ~ Gender, data = frames_df, horizontal = T)
hist(frames_df$Chip.Time, freq =F)

frames_df$Category <- gsub("F","",frames_df$Category)
frames_df$Category <- gsub("M","",frames_df$Category)
frames_df$Category <- gsub("U19","18",frames_df$Category)
frames_df$Category <- gsub("S","20",frames_df$Category)
frames_df$Category <- as.numeric(frames_df$Category)

gap <- 0.25
frames_df$age <- frames_df$Category + runif(17481, gap, 5-gap)
frames_df$age[frames_df$Category == 18] <- 
  frames_df$Category[frames_df$Category == 18] + runif(16, gap, 2-gap)
frames_df$age[frames_df$Category == 20] <- 
  frames_df$Category[frames_df$Category == 20] + runif(2755, gap, 15-gap)

gap <- 0.25
frames_df[frames_df$Gender == "Male",]$age <- 
  frames_df[frames_df$Gender == "Male",]$Category + runif(10989,gap,2.5)
frames_df[frames_df$Gender == "Female",]$age <- 
  frames_df[frames_df$Gender == "Female",]$Category + runif(6492,2.5,5-gap)

frames_df[frames_df$Category == 18,]$age <- 
  frames_df[frames_df$Category == 18,]$Category + runif(16,gap,2-gap)

frames_df[frames_df$Category == 20 & frames_df$Gender== "Male",]$age <- 
  frames_df[frames_df$Category == 20 & frames_df$Gender== "Male",]$Category + 
  runif(1794,gap,7.5)

frames_df[frames_df$Category == 20 & frames_df$Gender== "Female",]$age <- 
  frames_df[frames_df$Category == 20 & frames_df$Gender== "Female",]$Category + 
  runif(961,7.5,15-gap)

with(frames_df[frames_df$Gender == "Female" & frames_df$Category == 18,],
     {age <- Category + runif(3,1,2-gap)})
with(frames_df[frames_df$Gender == "Male",],age <- Category + runif(10989,gap,2.5))
with(frames_df[frames_df$Gender == "Female",],age <- Category + runif(6492,2.5,5-gap))
frames_df$age[frames_df$Category == 18] <- 
  frames_df$Category[frames_df$Category == 18] + runif(16, gap, 2-gap)
frames_df$age[frames_df$Category == 20] <- 
  frames_df$Category[frames_df$Category == 20] + runif(2755, gap, 15-gap)

frames_df$X15min <- 10 * 15 / frames_df$X10K
frames_df$X30min <- 10 * 30 / frames_df$X10K
frames_df$X45min <- 20 * 45 / frames_df$X20K
frames_df$X60min <- 20 * 60 / frames_df$X20K
frames_df$X75min <- 30 * 75 / frames_df$X30K
frames_df$X90min <- 30 * 90 / frames_df$X30K
frames_df$X105min <- 40 * 105 / frames_df$X40K
frames_df$X120min <- 40 * 120 / frames_df$X40K
frames_df$X135min <- 42 * 135 / frames_df$Chip.Time
frames_df$X150min <- 42 * 150 / frames_df$Chip.Time
frames_df$X165min <- 42 * 165 / frames_df$Chip.Time
frames_df$X180min <- 42 * 180 / frames_df$Chip.Time
frames_df$X195min <- 42 * 195 / frames_df$Chip.Time
frames_df$X210min <- 42 * 210 / frames_df$Chip.Time
frames_df$X225min <- 42 * 225 / frames_df$Chip.Time
frames_df$X240min <- 42 * 240 / frames_df$Chip.Time
frames_df$X255min <- 42 * 255 / frames_df$Chip.Time
frames_df$X270min <- 42 * 270 / frames_df$Chip.Time
frames_df$X285min <- 42 * 285 / frames_df$Chip.Time
frames_df$X300min <- 42 * 300 / frames_df$Chip.Time
frames_df$X315min <- 42 * 315 / frames_df$Chip.Time
frames_df$X330min <- 42 * 330 / frames_df$Chip.Time
frames_df$X345min <- 42 * 345 / frames_df$Chip.Time
frames_df$X360min <- 42 * 360 / frames_df$Chip.Time
frames_df$X375min <- 42 * 375 / frames_df$Chip.Time
frames_df$X390min <- 42 * 390 / frames_df$Chip.Time
frames_df$X405min <- 42 * 405 / frames_df$Chip.Time
frames_df$X420min <- 42 * 420 / frames_df$Chip.Time
frames_df$X435min <- 42 * 435 / frames_df$Chip.Time
frames_df$X450min <- 42 * 450 / frames_df$Chip.Time
frames_df$X465min <- 42 * 465 / frames_df$Chip.Time
frames_df$X480min <- 42 * 480 / frames_df$Chip.Time
frames_df$X495min <- 42 * 495 / frames_df$Chip.Time
frames_df$X510min <- 42 * 510 / frames_df$Chip.Time
frames_df$X135min[frames_df$X30K > 135] <- 30 * 135 / frames_df$X30K[frames_df$X30K > 135]
frames_df$X135min[frames_df$X20K > 135] <- 20 * 135 / frames_df$X20K[frames_df$X20K > 135]
frames_df$X150min[frames_df$X30K > 150] <- 30 * 150 / frames_df$X30K[frames_df$X30K > 150]
frames_df$X150min[frames_df$X30K > 150] <- 20 + 10 * frames_df$X20K[frames_df$X30K > 150] / frames_df$X30K[frames_df$X30K > 150] # new one
frames_df$X150min[frames_df$X20K > 150] <- 20 * 150 / frames_df$X20K[frames_df$X20K > 150]
frames_df$X165min[frames_df$X30K > 165] <- 30 * 165 / frames_df$X30K[frames_df$X30K > 165]
frames_df$X165min[frames_df$X20K > 165] <- 20 * 165 / frames_df$X20K[frames_df$X20K > 165]
frames_df$X180min[frames_df$X30K > 180] <- 30 * 180 / frames_df$X30K[frames_df$X30K > 180]
frames_df$X180min[frames_df$X20K > 180] <- 20 * 180 / frames_df$X20K[frames_df$X20K > 180]
frames_df$X195min[frames_df$X30K > 195] <- 30 * 195 / frames_df$X30K[frames_df$X30K > 195]
frames_df$X195min[frames_df$X20K > 195] <- 20 * 195 / frames_df$X20K[frames_df$X20K > 195]
frames_df$X210min[frames_df$X30K > 210] <- 30 * 210 / frames_df$X30K[frames_df$X30K > 210]
frames_df$X210min[frames_df$X20K > 210] <- 20 * 210 / frames_df$X20K[frames_df$X20K > 210]
frames_df$X225min[frames_df$X30K > 225] <- 30 * 225 / frames_df$X30K[frames_df$X30K > 225]
frames_df$X240min[frames_df$X30K > 240] <- 30 * 240 / frames_df$X30K[frames_df$X30K > 240]
frames_df$X255min[frames_df$X30K > 255] <- 30 * 255 / frames_df$X30K[frames_df$X30K > 255]
frames_df$X270min[frames_df$X30K > 270] <- 30 * 270 / frames_df$X30K[frames_df$X30K > 270]
frames_df$X285min[frames_df$X30K > 285] <- 30 * 285 / frames_df$X30K[frames_df$X30K > 285]
frames_df$X300min[frames_df$X30K > 300] <- 30 * 300 / frames_df$X30K[frames_df$X30K > 300]
frames_df$X315min[frames_df$X30K > 315] <- 30 * 315 / frames_df$X30K[frames_df$X30K > 315]
frames_df$X330min[frames_df$X30K > 330] <- 30 * 330 / frames_df$X30K[frames_df$X30K > 330]

myfunc <- function(rowx,current_time){
  chkpt_times <- data.frame(c(0,rowx[1:4])) # Times: 0k, 10k, 20k, 30k, 40k chkpts.
  current_section <- findInterval(current_time,chkpt_times) # from 1 to  5
  print(current_section) # <<<
  upper_chkpt_d <- current_section * 10 # from 10k to 50k
  print(upper_chkpt_d) # <<<
  lower_chkpt_d <- upper_chkpt_d - 10 # ranges from 0k to 40k
  print(lower_chkpt_d) # <<<
  lower_time <- chkpt_times[current_section]
  print(lower_time) # <<<
  # print(max(current_section)) # <<<
  # if (max(current_section) == 5){
  #   upper_time <- lower_time
  # } else {
  #   upper_time <- chkpt_times[current_section + 1]
  # }
  upper_time <- chkpt_times[current_section + 1] # <<<
  current_distance <- 
    lower_chkpt_d + 10 * (current_time - lower_time)/(upper_time - lower_time)
  return(current_distance)
}

myfunc2 <- function(rowx){
  # print("rowx") # <<<
  # print(rowx) # <<<
  time_intervals <- seq(15,510,by = 15)
  # print("time intervals")
  # print(time_intervals)
  # str(time_intervals)
  t_200k <- rowx[5] + (160/2.2)*(rowx[5] - rowx[4]) # A dummy time; needed later
  # print("t_200k") # <<<
  # print(t_200k) # <<<
  chkpt_times <- c(0,rowx[1:5],t_200k) # Times: 0k, 10k, 20k, 30k, 40k, 42k,200k.(deleted  dataf())
  # print("chkpt_times") # <<<
  # print(chkpt_times) # <<<
  # str(chkpt_times)
  chkpt_distances <- c(0,10,20,30,40,42,200)
  # print("chkpt_distances") # <<<
  # print(chkpt_distances) # <<<
  current_section <- findInterval(time_intervals,chkpt_times) # from 1 to  7
  # print("current_section") # <<<
  # print(current_section) # <<<
  lower_chkpt_d <- chkpt_distances[current_section]
  # print("lower_chkpt_d") # <<<
  # print(lower_chkpt_d) # <<<
  upper_chkpt_d <- chkpt_distances[current_section + 1]
  # print("upper_chkpt_d") # <<<
  # print(upper_chkpt_d) # <<<
  lower_time <- chkpt_times[current_section]
  # print("lower_time") # <<<
  # print(lower_time) # <<<
  upper_time <- chkpt_times[current_section + 1] # <<<
  # print("upper_time") # <<<
  # print(upper_time) # <<<
  current_distance <- 
    lower_chkpt_d + 
    (upper_chkpt_d - lower_chkpt_d)*(time_intervals - lower_time)/(upper_time - lower_time)
  return(current_distance)
}

huge_df <- huge_df %>% 
  rename(
    t1 = "1",
    t2 = "2",
    t3 = "3",
    t4 = "4",
    t5 = "5",
    t6 = "6",
    t7 = "7",
    t8 = "8",
    t9 = "9",
    t10 = "10",
    t11 = "11",
    t12 = "12",
    t13 = "13",
    t14 = "14",
    t15 = "15",
    t16 = "16",
    t17 = "17",
    t18 = "18",
    t19 = "19",
    t20 = "20",
    t21 = "21",
    t22 = "22",
    t23 = "23",
    t24 = "24",
    t25 = "25",
    t26 = "26",
    t27 = "27",
    t28 = "28",
    t29 = "29",
    t30 = "30",
    t31 = "31",
    t32 = "32",
    t33 = "33",
    t34 = "34"
  )

times_df <- apply(frames_df[5000:5001,c(4:7,9)],1,function(x) myfunc2(x)) ### <- <- <- works
times_df <- apply(frames_df[,c(4:7,9)],1,function(x) myfunc2(x)) ### <- <- <- works

t_times_df <- t(times_df)
huge_df <- cbind(frames_df,t_times_df)
View(huge_df)

# frames_df$bin15 <-  findInterval(15,frames_df[,c(4:7,9)]) doesn't work
# myfunc <- function(x,runner){
#   findInterval(x,runner[c(4:7,9)])
# }
# frames_df$bin15 <- apply(frames_df,1,myfunc,x = 15,runner = frames_df) # doesnt work
# frames_df$bin15 <- findInterval(15,c(frames_df$X10K,frames_df$X20K,frames_df$X30K,frames_df$X40K))
# frames_df <- transform(frames_df, bin15 = findInterval(X10K,X20K,X30K))

# myfunc <- function(t,i){
#   section <- findInterval(t,i)
#   i <- as.numeric(i)
#   print(t)
#   print(section)
#   print(i[section - 1])
#   km <- 10 * (1 + (t - i[section - 1])/(i[section] - i[section - 1]))
# }

# frames_df$bin15 <- apply(frames_df[c(4:7,9)],1,function(x) findInterval(15,x))
# frames_df$bin30 <- apply(frames_df[c(4:7,9)],1,function(x) findInterval(30,x))
# frames_df$bin45 <- apply(frames_df[c(4:7,9)],1,function(x) findInterval(45,x))
# frames_df$bin60 <- apply(frames_df[c(4:7,9)],1,function(x) findInterval(60,x))
# frames_df$bin75 <- apply(frames_df[c(4:7,9)],1,function(x) findInterval(75,x))
# frames_df$bin90 <- apply(frames_df[c(4:7,9)],1,function(x) findInterval(90,x))
# frames_df$bin105 <- apply(frames_df[c(4:7,9)],1,function(x) findInterval(105,x))
# frames_df$bin120 <- apply(frames_df[c(4:7,9)],1,function(x) findInterval(120,x))

# frames_df$d15m <- apply(frames_df[c(4:7,9)],1,function(x) 150/x[frames_df$bin15]) doesn't work
# frames_df$d15m <- 150/frames_df[4 + frames_df$bin15] disaster, created 50,000 columns
# frames_df <- frames_df[-c(18:17506)]
# 
# frames_df$bin75 <- apply(frames_df,1,function(x) myfunc(75,x[c(4:7,9)]))
# frames_df$bin105 <- apply(frames_df,1,function(x) myfunc(105,x[c(4:7,9)]))
  


# myfunc <- function(runner){findInterval(seq(15,510,by=15),runner[c(4:7,9)])}
data4plot <- frames_df
final_plot <- ggplot(data = data4plot,mapping = aes(X120min, age, colour = Gender))

final_plot +
  geom_point(size = 0.6, na.rm = T) +
  theme_void() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values=c("#1E8F89", "#EE5A45")) +
  xlim(0,42) +
  guides(colour = guide_legend(override.aes = list(size=3)))

# geom_hline(aes(yintercept = data4plot$Category)) +
i = "X15min"
saveGIF({
  for (i in names(huge_df)[49:82]) {
    p = ggplot(data = huge_df,mapping = aes_string(i, "age", colour = "Gender")) +
      geom_point(na.rm = T, size = .6) +
      theme_void() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      scale_color_manual(values=c("#1E8F89", "#EE5A45")) +
      guides(colour = guide_legend(override.aes = list(size=3))) +
      geom_vline(data = data.frame(v = c(0,10,20,30,40,42.2)),
                 mapping = aes(xintercept = v),linetype = "dotted") +
      xlim(0,42.2)
    print(p)
  }
}, movie.name="test.gif", ani.width = 1200)

tidy_df <- pivot_longer(huge_df,cols = t1:t34, names_to = "time_ran", values_to = "distance_ran")
View(tidy_df)
tidy_df$time_ran <- as.numeric(gsub("t","",tidy_df$time_ran))
tidy_df$time_ran <- tidy_df$time_ran * 15

ggplot(data = tidy_df,mapping = aes(time_ran,age,colour = Gender)) +
  geom_point(size= 0.6)

anim <- final_plot + 
  transition_reveal(X30min,
                    transition_length = 2,
                    state_length = 1)

anim

men <- frames_df[frames_df$Gender == "Male",]
women <- frames_df[frames_df$Gender == "Female",]

plot(frames_df$Chip.Time,frames_df$age)
abline(h = c(20,35,40,45,50,55,60,65,70,75,80))

plot(women$Chip.Time,women$Race.Number)
plot(men$Chip.Time,men$Race.Number)

plot(density(men$Chip.Time))
lines(density(women$Chip.Time))
sd(men$Chip.Time)
sd(women$Chip.Time)
mean(men$Chip.Time)
mean(women$Chip.Time)
median(men$Chip.Time)
median(women$Chip.Time)

leveneTest(Chip.Time ~ Gender, data = frames_df)

boxplot(men[,10:13])
boxplot(women[,10:13])

summary(men$split_4 - men$split_1)
head(sort(men$split_4 - men$split_1))
men[order(men$split_4 - men$split_1),][1:10,]

summary(women$split_4 - women$split_1)
head(sort(women$split_4 - women$split_1))
women[order(women$split_4 - women$split_1),][1:10,]

summary(men$split_3 - men$split_1)
head(sort(men$split_3 - men$split_1))
men[order(men$split_3 - men$split_1),][1:10,]

summary(women$split_3 - women$split_1)
head(sort(women$split_3 - women$split_1))
women[order(women$split_3 - women$split_1),][1:10,]

summary(men$split_3 - men$split_2)
head(sort(men$split_3 - men$split_2))
men[order(men$split_3 - men$split_2),][1:10,]

summary(women$split_3 - women$split_2)
head(sort(women$split_3 - women$split_2))
women[order(women$split_3 - women$split_2),][1:10,]

summary(men$split_4 - men$split_2)
head(sort(men$split_4 - men$split_2))
men[order(men$split_4 - men$split_2),][1:10,]

summary(women$split_4 - women$split_2)
head(sort(women$split_4 - women$split_2))

## scraping stuff
url <- "http://results.dublinmarathon.ie/results.php?search&race=90&sort=placeall&asc&from="

# Reads the table from the page but stores it in a list
results.table <-
  url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="content"]/table[1]') %>% 
  html_table() %>%
  data.frame()

View(results.table)
str(results.table)

list.of.lists <- vector("list",4)
page <- 0
while (page <=40){
  list.of.lists[[1+page/10]] <-
    paste(url,page,sep="") %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="content"]/table[1]') %>% 
    html_table() %>%
    data.frame()
  page <- page + 10
}

url <- "https://www.tdleventservices.co.uk/event-results/events?event=3492&page="

list.of.lists <- vector("list",180)
page <- 1
while (i <=180){
  list.of.lists[[page]] <-
    paste(url,page,sep="") %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="content"]/table[1]') %>% 
    html_table() %>%
    data.frame()
  page <- page + 1
}

# Reads the table from the page but stores it in a list


View(results.table)
str(results.table)

list.of.lists[1:3]
