# load required packages

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# load data

booking <- read_csv("fresnoBooking1718_clean.csv") %>%
  mutate(START_DATE = as.POSIXct(START_DATE, format = "%m/%d/%Y %I:%M:%S %p"),
         END_DATE = as.POSIXct(END_DATE, format = "%m/%d/%Y %I:%M:%S %p"),
         time_jailed2 = difftime(END_DATE,START_DATE, unit = "days"))

glimpse(booking)

# converting bail amounts to numeric that read in properly as characters

booking$`bail 23` <- as.numeric(as.character(booking$`bail 23`))
booking$`bail 24` <- as.numeric(as.character(booking$`bail 24`))
booking$`bail 25` <- as.numeric(as.character(booking$`bail 25`))
booking$`bail 26` <- as.numeric(as.character(booking$`bail 26`))
booking$`bail 27` <- as.numeric(as.character(booking$`bail 27`))
booking$`bail 28` <- as.numeric(as.character(booking$`bail 28`))
booking$`bail 29` <- as.numeric(as.character(booking$`bail 29`))
booking$`bail 30` <- as.numeric(as.character(booking$`bail 30`))
booking$`bail 31` <- as.numeric(as.character(booking$`bail 31`))
booking$`bail 32` <- as.numeric(as.character(booking$`bail 32`))
booking$`bail 33` <- as.numeric(as.character(booking$`bail 33`))
booking$`bail 34` <- as.numeric(as.character(booking$`bail 34`))
booking$`bail 35` <- as.numeric(as.character(booking$`bail 35`))
booking$`bail 36` <- as.numeric(as.character(booking$`bail 36`))


# release types

release_types <- booking %>%
  group_by(RELEASE_TYPE) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percent = round(count/sum(count)*100, 2))

# zip codes 

zip_codes <- booking %>%
  group_by(ZIP) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# race

race <- booking %>%
  group_by(RACE) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percent = round(count/sum(count)*100,2))

# release type by race

release_race <- booking %>%
  group_by(RELEASE_TYPE, RACE) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percent = round(count/sum(count)*100,2))

# mean and median time jailed by race

time_race_mean <- booking %>%
  group_by(RACE) %>%
  summarize(mean_days_jailed = round(mean(time_jailed, na.rm = TRUE),2)) %>%
  arrange(desc(mean_days_jailed))

time_race_median <- booking %>%
  group_by(RACE) %>%
  summarize(median_days_jailed = round(median(time_jailed, na.rm = TRUE),2)) %>%
  arrange(desc(median_days_jailed))


# distribution of time in jail

ggplot(booking, aes(x=time_jailed)) + geom_histogram()

# create columns for felonies and misdemeanors

booking <- booking %>%
  mutate(charges_1_m = ifelse(grepl("\\[M\\]", `charges 1_clean`)==TRUE,1,0),
         charges_1_f = ifelse(grepl("\\[F\\]", `charges 1_clean`)==TRUE,1,0),
         charges_2_m = ifelse(grepl("\\[M\\]", `charges 2_clean`)==TRUE,1,0),
         charges_2_f = ifelse(grepl("\\[F\\]", `charges 2_clean`)==TRUE,1,0),
         charges_3_m = ifelse(grepl("\\[M\\]", `charges 3_clean`)==TRUE,1,0),
         charges_3_f = ifelse(grepl("\\[F\\]", `charges 3_clean`)==TRUE,1,0),
         charges_4_m = ifelse(grepl("\\[M\\]", `charges 4_clean`)==TRUE,1,0),
         charges_4_f = ifelse(grepl("\\[F\\]", `charges 4_clean`)==TRUE,1,0),
         charges_5_m = ifelse(grepl("\\[M\\]", `charges 5_clean`)==TRUE,1,0),
         charges_5_f = ifelse(grepl("\\[F\\]", `charges 5_clean`)==TRUE,1,0),
         charges_6_m = ifelse(grepl("\\[M\\]", `charges 6_clean`)==TRUE,1,0),
         charges_6_f = ifelse(grepl("\\[F\\]", `charges 6_clean`)==TRUE,1,0),
         charges_7_m = ifelse(grepl("\\[M\\]", `charges 7_clean`)==TRUE,1,0),
         charges_7_f = ifelse(grepl("\\[F\\]", `charges 7_clean`)==TRUE,1,0),
         charges_8_m = ifelse(grepl("\\[M\\]", `charges 8_clean`)==TRUE,1,0),
         charges_8_f = ifelse(grepl("\\[F\\]", `charges 8_clean`)==TRUE,1,0),
         charges_9_m = ifelse(grepl("\\[M\\]", `charges 9_clean`)==TRUE,1,0),
         charges_9_f = ifelse(grepl("\\[F\\]", `charges 9_clean`)==TRUE,1,0),
         charges_10_m = ifelse(grepl("\\[M\\]", `charges 10_clean`)==TRUE,1,0),
         charges_10_f = ifelse(grepl("\\[F\\]", `charges 10_clean`)==TRUE,1,0),
         charges_11_m = ifelse(grepl("\\[M\\]", `charges 11_clean`)==TRUE,1,0),
         charges_11_f = ifelse(grepl("\\[F\\]", `charges 11_clean`)==TRUE,1,0),
         charges_12_m = ifelse(grepl("\\[M\\]", `charges 12_clean`)==TRUE,1,0),
         charges_12_f = ifelse(grepl("\\[F\\]", `charges 12_clean`)==TRUE,1,0),
         charges_13_m = ifelse(grepl("\\[M\\]", `charges 13_clean`)==TRUE,1,0),
         charges_13_f = ifelse(grepl("\\[F\\]", `charges 13_clean`)==TRUE,1,0),
         charges_14_m = ifelse(grepl("\\[M\\]", `charges 14_clean`)==TRUE,1,0),
         charges_14_f = ifelse(grepl("\\[F\\]", `charges 14_clean`)==TRUE,1,0),
         charges_15_m = ifelse(grepl("\\[M\\]", `charges 15_clean`)==TRUE,1,0),
         charges_15_f = ifelse(grepl("\\[F\\]", `charges 15_clean`)==TRUE,1,0),
         charges_16_m = ifelse(grepl("\\[M\\]", `charges 16_clean`)==TRUE,1,0),
         charges_16_f = ifelse(grepl("\\[F\\]", `charges 16_clean`)==TRUE,1,0),
         charges_17_m = ifelse(grepl("\\[M\\]", `charges 17_clean`)==TRUE,1,0),
         charges_17_f = ifelse(grepl("\\[F\\]", `charges 17_clean`)==TRUE,1,0),
         charges_18_m = ifelse(grepl("\\[M\\]", `charges 18_clean`)==TRUE,1,0),
         charges_18_f = ifelse(grepl("\\[F\\]", `charges 18_clean`)==TRUE,1,0),
         charges_19_m = ifelse(grepl("\\[M\\]", `charges 19_clean`)==TRUE,1,0),
         charges_19_f = ifelse(grepl("\\[F\\]", `charges 19_clean`)==TRUE,1,0),
         charges_20_m = ifelse(grepl("\\[M\\]", `charges 20_clean`)==TRUE,1,0),
         charges_20_f = ifelse(grepl("\\[F\\]", `charges 20_clean`)==TRUE,1,0),
         charges_21_m = ifelse(grepl("\\[M\\]", `charges 21_clean`)==TRUE,1,0),
         charges_21_f = ifelse(grepl("\\[F\\]", `charges 21_clean`)==TRUE,1,0),
         charges_22_m = ifelse(grepl("\\[M\\]", `charges 22_clean`)==TRUE,1,0),
         charges_22_f = ifelse(grepl("\\[F\\]", `charges 22_clean`)==TRUE,1,0),
         charges_23_m = ifelse(grepl("\\[M\\]", `charges 23_clean`)==TRUE,1,0),
         charges_23_f = ifelse(grepl("\\[F\\]", `charges 23_clean`)==TRUE,1,0),
         charges_24_m = ifelse(grepl("\\[M\\]", `charges 24_clean`)==TRUE,1,0),
         charges_24_f = ifelse(grepl("\\[F\\]", `charges 24_clean`)==TRUE,1,0),
         charges_25_m = ifelse(grepl("\\[M\\]", `charges 25_clean`)==TRUE,1,0),
         charges_25_f = ifelse(grepl("\\[F\\]", `charges 25_clean`)==TRUE,1,0),
         charges_26_m = ifelse(grepl("\\[M\\]", `charges 26_clean`)==TRUE,1,0),
         charges_26_f = ifelse(grepl("\\[F\\]", `charges 26_clean`)==TRUE,1,0),
         charges_27_m = ifelse(grepl("\\[M\\]", `charges 27_clean`)==TRUE,1,0),
         charges_27_f = ifelse(grepl("\\[F\\]", `charges 27_clean`)==TRUE,1,0),
         charges_28_m = ifelse(grepl("\\[M\\]", `charges 28_clean`)==TRUE,1,0),
         charges_28_f = ifelse(grepl("\\[F\\]", `charges 28_clean`)==TRUE,1,0),
         charges_29_m = ifelse(grepl("\\[M\\]", `charges 29_clean`)==TRUE,1,0),
         charges_29_f = ifelse(grepl("\\[F\\]", `charges 29_clean`)==TRUE,1,0),
         charges_30_m = ifelse(grepl("\\[M\\]", `charges 30_clean`)==TRUE,1,0),
         charges_30_f = ifelse(grepl("\\[F\\]", `charges 30_clean`)==TRUE,1,0),
         charges_31_m = ifelse(grepl("\\[M\\]", `charges 31_clean`)==TRUE,1,0),
         charges_31_f = ifelse(grepl("\\[F\\]", `charges 31_clean`)==TRUE,1,0),
         charges_32_m = ifelse(grepl("\\[M\\]", `charges 32_clean`)==TRUE,1,0),
         charges_32_f = ifelse(grepl("\\[F\\]", `charges 32_clean`)==TRUE,1,0),
         charges_33_m = ifelse(grepl("\\[M\\]", `charges 33_clean`)==TRUE,1,0),
         charges_33_f = ifelse(grepl("\\[F\\]", `charges 33_clean`)==TRUE,1,0),
         charges_34_m = ifelse(grepl("\\[M\\]", `charges 34_clean`)==TRUE,1,0),
         charges_34_f = ifelse(grepl("\\[F\\]", `charges 34_clean`)==TRUE,1,0),
         charges_35_m = ifelse(grepl("\\[M\\]", `charges 35_clean`)==TRUE,1,0),
         charges_35_f = ifelse(grepl("\\[F\\]", `charges 35_clean`)==TRUE,1,0),
         charges_36_m = ifelse(grepl("\\[M\\]", `charges 36_clean`)==TRUE,1,0),
         charges_36_f = ifelse(grepl("\\[F\\]", `charges 36_clean`)==TRUE,1,0))
        
# create columns for total misdemeanors and felonies

booking <- booking %>%
  mutate(total_m = charges_1_m + charges_2_m + charges_3_m + charges_4_m + charges_5_m +
           charges_6_m + charges_7_m + charges_8_m + charges_9_m + charges_10_m +
           charges_11_m + charges_12_m + charges_13_m + charges_14_m + charges_15_m +
           charges_16_m + charges_17_m + charges_18_m + charges_19_m + charges_20_m +
           charges_21_m + charges_22_m + charges_23_m + charges_24_m + charges_25_m +
           charges_26_m + charges_27_m + charges_28_m + charges_29_m + charges_30_m +
           charges_31_m + charges_32_m + charges_33_m + charges_34_m + charges_35_m +
           charges_36_m,
         total_f = charges_1_f + charges_2_f + charges_3_f + charges_4_f + charges_5_f +
           charges_6_f + charges_7_f + charges_8_f + charges_9_f + charges_10_f +
           charges_11_f + charges_12_f + charges_13_f + charges_14_f + charges_15_f +
           charges_16_f + charges_17_f + charges_18_f + charges_19_f + charges_20_f +
           charges_21_f + charges_22_f + charges_23_f + charges_24_f + charges_25_f +
           charges_26_f + charges_27_f + charges_28_f + charges_29_f + charges_30_f +
           charges_31_f + charges_32_f + charges_33_f + charges_34_f + charges_35_f +
           charges_36_f)

glimpse(booking)

# create columns for total bail

booking <- booking %>%
  mutate(total_bail = rowSums(select(., contains("bail")), na.rm = TRUE))

glimpse(booking)


# creating a factor variable for race in order to run a linear regression

booking$race.f <- factor(booking$RACE)
is.factor(booking$race.f)

print(levels(booking$race.f))

booking$race.f = factor(booking$race.f,levels(booking$race.f)[c(6,3,4,2,1,5)])


#creating factor variable for gender

booking$gender.f <- factor(booking$GENDER)
is.factor(booking$gender.f)

print(levels(booking$gender.f))
booking$gender.f = factor(booking$gender.f, levels(booking$gender.f)[c(3,1,2,4)])


# distribution of total bail

hist_total_bail <- ggplot(booking, aes(x=total_bail)) + geom_histogram()

plot(hist_total_bail)

# bail, our outcome variable, is highly skewed, so let's going to perform a transformation on the bail column and then drop the infinites values we've created
booking <- booking %>%
  mutate(total_bail_transform = log10(total_bail))

booking$total_bail_transform[which(is.infinite(booking$total_bail_transform))] = NA

# now, looking at distribution of transformed variable

hist_total_bail_transform <- ggplot(booking, aes(x=total_bail_transform)) + geom_histogram()

plot(hist_total_bail_transform)

# filter out those who weren't given bail

booking <- booking %>%
  mutate(no_bail = ifelse(grepl("NO BAIL", CHARGES)==TRUE,"NO BAIL","BAILABLE"))

booking_bailable <- booking %>%
  filter(no_bail == "BAILABLE")

# diagnostic code
# zerobail <- booking %>%
#   filter(total_bail==0 & no_bail =="BAILABLE")


# multiple linear regression 

fit <- lm(total_bail_transform ~ race.f + total_m + total_f + AGE, data=booking_bailable)
summary(fit)


# now creating a training and test data set to test my model

train_idx <- sample(1:nrow(booking_bailable),10991,replace=FALSE)
booking_bailable_training <- booking_bailable[train_idx,] # selecting all these rows
booking_bailable_test <- booking_bailable[-train_idx,] # selecting all but these rows

# testing fit on training and test data

fit_training <- lm(total_bail_transform ~ race.f + total_m + total_f + AGE, data=booking_bailable_training)
summary(fit_training)

fit_test <- lm(total_bail_transform ~ race.f + total_m + total_f + AGE, data=booking_bailable_test)
summary(fit_test)
