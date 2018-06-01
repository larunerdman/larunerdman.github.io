# load libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library (reshape2)

# initalize folders
home <- "/Users/xing"
project <- paste0(home, "/Desktop/Budget_R/Data")

# read csv
dat1 <- read.csv(paste0(project, "/budget1.csv"))
  dat2 <- read.csv(paste0(project, "/budget2.csv"))

# function to get rid of whitespaces
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

dat1$person <- trim(dat1$person)
dat1$description <- trim(dat1$description)
dat1$details <- trim(dat1$details)

# change date format
dat1$date <- as.Date(dat1$date, format = "%m/%d/%Y")

#change columns into factors
dat1$person <- as.factor(dat1$person)
dat1$description <- as.factor(dat1$description)

# subset data for breakfast, lunch, dinner, and 2nd dinner
dat_meals <- dat1[dat1$description == "breakfast" | dat1$description == "lunch" | 
                    dat1$description == "dinner" | dat1$description == "2nd_dinner",]

# subset using dplyr
dat_meals <- dat1 %>%
  filter(description == "breakfast" | description == "lunch" | 
           description == "dinner" | description == "2nd_dinner")

# for each meal and person, get average expenditure
dat_compare <- dat_meals %>%
  group_by(person, description) %>%
  summarise(total_exp = sum(expenditure), na.rm = T)

# let's plot this shit! plot dat_compare with barplot
ggplot(dat_compare, aes(person, total_exp, group = description, fill = description)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) +
  xlab("Person") + ylab("Expenditure") + ggtitle("Spending by Person") +
  scale_fill_manual(values = c("darkseagreen", "darkslategray3", "darkolivegreen4", "gray68"), 
                    name = "Meals", 
                    breaks = c("2nd_dinner", "breakfast", "dinner", "lunch"),
                    labels = c("2nd Dinner", "Breakfast", "Dinner", "Lunch")) +
  theme_igray() + scale_colour_tableau()
  
# one sided two sample t-test of expenditure comparing ben and xing's expenditure
# null hypothesis H0: meanBen = meanXing
# alternative H1: meanBen > meanXing
# P value is the probability that what we observe would occur if the null hypothesis were true
t.test(dat1$expenditure[dat1$person == "Ben"], dat1$expenditure[dat1$person =="Xing"],
       alternative = "greater")

##############################################################################################

# change 'snack' to 'snacks' in description column

dat1$description <- ifelse(dat1$description == "snack", "snacks", dat1$description)

# make a column with broader categories (breakfast, lunch, dinner, 2nd_dinner, and snacks all under "food";
# # )
# dat1$categories <- ifelse(dat1$description == "breakfast" | dat1$description == 'lunch' | 
#                             dat1$description == 'dinner' | dat1$description == '2nd_dinner' |
#                             dat1$description == 'snacks' | dat1$description == 'drinks', "Food_Drinks",
#                           ifelse(dat1$description == "coffee", "Coffee",
#                                  ifelse(dat1$description == "admin" | dat1$description == "home" | dat1$description =="phone" |
#                                           dat1$description == "parking" | dat1$description == "transport", "Home_Admin",
#                                         ifelse(dat1$description == "recreation" | dat1$description == "travel", "Recreation",
#                                                ifelse(dat1$description == "clothes" |  dat1$description == "medicine", "Necessities",
#                                                       ifelse(dat1$description == "gift" | dat1$description == "donation", "Gift", 
#                                                               "Groceries"))))))
#                                                                               
      
# use the grepl function to recode 
food_drinks <- "breakfast|lunch|dinner|2nd_dinner|snacks|drinks"
coffee <- "coffee"
home_admin <- "admin|home|parking|transport|phone"
recreation <- "recreation|travel"
neccessities <- "clothes|medicine"
gifts <- "gift|donation" 

dat1$categories <- ifelse(grepl(food_drinks, dat1$description), "Food_Drinks", 
               ifelse(grepl(coffee, dat1$description), "Coffee",
                      ifelse(grepl(home_admin, dat1$description), "Home_Admin",
                             ifelse(grepl(recreation, dat1$description), "Recreation",
                                    ifelse(grepl(neccessities, dat1$description), "Necessities",
                                           ifelse(grepl(gifts, dat1$description), "Gift", "Groceries"))))))
  


# make bar graph using ggplot2 to represent this
ggplot(dat1, aes(categories, expenditure)) +
  geom_bar(stat = "identity") + 
  xlab("Category") +
  ylab("Expenditure") +
  ggtitle("How We Spend Our Money")

# make bar graph using ggplot2 to represent this

dat_category <- dat1 %>%
  group_by(person, categories) %>%
  summarise(total_sum = sum(expenditure, na.rm = T))

ggplot(dat_category, aes(person, total_sum, group = categories, fill = categories)) +
  geom_bar(stat = "identity", alpha = 0.6) + 
  xlab("Category") +
  ylab("Expenditure") +
  ggtitle("How We Spend Our Money") + theme_fivethirtyeight()

##############################################################################

# make line graph of ben's cumulative pizza vs coffee consumption

ben_pc <- dat1 %>%
filter(person == "Ben" & details == "pizza" | person == "Ben" & description == "coffee")

# change 35.0 to 3.5
ben_pc$expenditure <- ifelse(ben_pc$expenditure == 35.00, 3.5, ben_pc$expenditure)
dat1$expenditure <- ifelse(dat1$expenditure == 35.00 & dat1$details == "pizza", 3.5, dat1$expenditure)

# replace dinner and 2nd_dinner with pizza so that pizza and coffee are in same column
ben_pc$description <- ifelse(ben_pc$description == "dinner" | ben_pc$description == "2nd_dinner", 
                             "pizza", ben_pc$description)

# remove details column
ben_pc$details <- NULL

# cumulative sum of expenditure for pizza and coffee
for (i in unique(ben_pc$description)) {
  ben_pc$cum_exp[ben_pc$description == i] <- cumsum(ben_pc$expenditure[ben_pc$description == i]) 
  print(i)
}

# plot it!
ggplot(ben_pc, aes(x=date, y=cum_exp, group = description, col = description)) +
  geom_line(size = 1) +
  xlab("Date") +
  ylab("Cumulative Expenditure") +
  ggtitle("Ben's Spending on Pizza vs Coffee") + theme_economist()

##########################################################################################

### Today's tasks ### 

# using dat1
# 1. make line graphs to compare ben and xing's cumulative spending over time
# 2. make a bar graph to see where we buy our coffee

# using dat2
# 1. make bar graph showing  averagepoops for ben and xing by day of the week
# 2. make bar graph to show which day of the week we have the most/least sex

# clean up dat2: first show only rows without na, change xing_poop to integer, date as date format, remove white
# spaces in factor columns
dat2 <- dat2[complete.cases(dat2),]

dat2$xing_poop <- as.integer(dat2$xing_poop)
dat2$date <- as.Date(dat2$date, format = "%m/%d/%Y")

trim <- function(x) gsub("^\\s+|\\s+$", "", x)
dat2$sex <- trim(dat2$sex)
dat2$sex <- as.factor(dat2$sex)
dat2$exercise_ben <- trim(dat2$exercise_ben)
dat2$exercise_ben <- as.factor(dat2$exercise_ben)
dat2$exercise_xing<- trim(dat2$exercise_xing)
dat2$exercise_xing <- as.factor(dat2$exercise_xing)

# line graph to show ben, xing, both's cum spending over time
# make for loop for new column of cumulative spending by person
for (i in unique(dat1$person)) {
  dat1$cum_prsn_exp[dat1$person == i] <- cumsum(dat1$expenditure[dat1$person == i])
  print(i)
}

ggplot(dat1, aes(x=date, y=cum_prsn_exp, group = person, col = person)) +
  geom_line(size = 1) +
  xlab("Date") +
  ylab("Cumulative Spending") +
  ggtitle("Cumulative Spending by Person") +
  theme_economist()

# bar graph to see where we buy our coffee
coffee <- dat1 %>%
  filter(description == "coffee")

coffee$details <- as.factor(coffee$details)

# sort into categories of starbucks, jimmys, tim_hortons, chinese_lady, other using grepl 
jimmys <- "jimmys_coffee"
tim_hortons <- "tim_hortons"
starbucks <- "starbucks|startbucks"
lavazza <- "lavazza"
bagel_stop <- "chinese_lady"
other <- "airport|aroma|balty|breakfast|gas_station|havana_cafe|hawaii|little_nickys|
  ottawa|science"

coffee$store <- ifelse(grepl(jimmys, coffee$details), "Jimmys",
                       ifelse(grepl(tim_hortons, coffee$details), "Tim_Hortons",
                              ifelse(grepl(starbucks, coffee$details), "Starbucks",
                                           ifelse(grepl(lavazza, coffee$details), "Lavazza",
                                                  ifelse(grepl(bagel_stop, coffee$details), "Bagel_Stop",
                                                         "Other")))))

# plot expenditure by store
coffee <- coffee %>%
  group_by(store) %>%
  summarise(total_exp = sum(expenditure))

ggplot(coffee, aes(reorder(store, -total_exp), total_exp)) +
  geom_bar(stat = "identity", alpha = 0.6) +
  xlab("Store Name") +
  ylab("Total Spending") +
  ggtitle("Coffee Expenditure by Store") +
  theme_economist()


### bar graph showing ben and xing average poops per day of week
# bp <- dat2 %>%
#   select(day_of_week, ben_poop) %>%
#   group_by(day_of_week) %>%
#   summarise(poop = mean(ben_poop))
# bp$name <- "Ben"
# 
# xp <- dat2 %>%
#   select(day_of_week, xing_poop) %>%
#   group_by(day_of_week) %>%
#   summarise(poop = mean(xing_poop))
# xp$name <- "Xing"
# 
# total_p <- rbind(bp, xp)

#relevel factors (organize day of week)

# ben and xing poops for days of the week (the RIGHT way) ...using melt

poops <- dat2 %>%
  group_by(day_of_week) %>%
  summarise(ben_p = mean(ben_poop), xing_p = mean(xing_poop))

poops_melt <- melt(poops, id.vars = "day_of_week")

poops_melt$day_of_week <- factor(poops_melt$day_of_week, levels = c("Sunday", "Monday", "Tuesday",
                                                                    "Wednesday", "Thursday", "Friday",
                                                                    "Saturday"))


ggplot(poops_melt, aes(day_of_week, value, group = variable, fill = variable)) +
  geom_bar(stat = "identity", alpha = 0.6, position = "dodge") +
  xlab("Day of Week") +
  ylab("Average Poops") +
  ggtitle("Average Poops Per Day of Week") +
  scale_fill_manual(name = "Person", 
                    breaks = c("ben_p", "xing_p"), 
                    labels = c("Ben", "Xing"),
                    values = c("Green", "Blue")) +
  theme_economist()


## for both
ggplot(total_p, aes(x = day_of_week, y = poop, group = name, fill = name)) +
  geom_bar(stat = "identity", alpha = 0.6, position = "dodge") +
  xlab("Day of Week") +
  ylab("Average Poops") +
  ggtitle("Average Poops Per Day of Week") +
  theme_economist()
  

#### which days of the week do we have most/least sex? ###

sex_day <- dat2 %>%
  select(day_of_week, sex)

sex_day$sex <- ifelse(sex_day$sex == "none", 1, 0)

sw <- sex_day %>% 
  group_by(day_of_week) %>%
  summarise(total_sex = sum(sex))

ggplot(sw, aes(day_of_week, total_sex)) +
  geom_bar(stat = "identity", alpha = 0.6) +
  xlab("Day of Week") +
  ylab("Total Sex") +
  ggtitle("Sex By Day of Week") +
  theme_economist()

sw$day_of_week <- factor(sw$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                                    "Saturday", "Sunday"))

###############################################################################
# make graphs with weather data!
library(weatherData)
library(devtools)

dat2$date <- as.Date(dat2$date, format = "%m/%d/%Y")
temp <- getWeatherForDate("YYZ", "2015-11-04", "2016-06-27")
temp$Date <- as.Date(temp$Date, format = '%m/%d/%Y')

# join the two data sets 
dat2 <- cbind(dat2, temp)
dat2$Date <- NULL

# graph mean weather over time
ggplot(dat2, aes(date, Mean_TemperatureC)) +
  geom_line(size = 1)

# make weather into categories
dat2$weather_cat <- ifelse(dat2$Mean_TemperatureC < 0, "Freezing",
                                  ifelse(dat2$Mean_TemperatureC > 0 & dat2$Mean_TemperatureC <= 10, "Very Cold",
                                               ifelse(dat2$Mean_TemperatureC > 10 & dat2$Mean_TemperatureC <= 20, "Warm",
                                                      ifelse(dat2$Mean_TemperatureC > 20 & dat2$Mean_TemperatureC <= 30,
                                                             "Very Warm", "Hot"))))

# determine total expenditure in each category of weather, plot in bar graph with 95 % CI
exp_by_weather <- dat2 %>%
  group_by(weather_cat) %>%
  summarise(mean.ew = mean(expenditure[expenditure < 200], na.rm = T),
            sd.ew = sd(expenditure[expenditure < 200], na.rm = T),
            n.ew = n()) %>%
  mutate(se.ew = sd.ew / sqrt(n.ew),
         lower.ci.ew = mean.ew - qt(1 - (0.05 / 2), n.ew - 1) * se.ew,
         upper.ci.ew = mean.ew + qt(1 - (0.05 / 2), n.ew - 1) * se.ew)

exp_by_weather$weather_cat <- factor(exp_by_weather$weather_cat, levels = c("Freezing", "Very Cold", "Warm", "Very Warm", "Hot"))

ggplot(exp_by_weather, aes(weather_cat, mean.ew)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin=lower.ci.ew, ymax=upper.ci.ew),
                width=.2) +
  xlab("Weather") +
  ylab("Total Expenditure") +
  ggtitle("Spending by Weather, Purchases < $200") +
  theme_economist()

##########################################################################################

# Make bar graph for average expenditure by person (Ben, Xing, Both) with CI

sbp <- dat1 %>%
  group_by(person) %>%
  summarise(mean.exp = mean(expenditure, na.rm = T),
            sd.exp = sd(expenditure, na.rm = T),
            n.exp = n()) %>%
mutate(se.exp = sd.exp / sqrt(n.exp),
       lower.ci.exp = mean.exp - qt(1 - (0.05 / 2), n.exp - 1) * se.exp,
       upper.ci.exp = mean.exp + qt(1 - (0.05 / 2), n.exp - 1) * se.exp)

ggplot(sbp, aes(person, mean.exp)) +
  geom_bar(stat = "identity", alpha = 0.6) +
  geom_errorbar(aes(ymin = lower.ci.exp, ymax = upper.ci.exp),
                width = .2) +
  xlab("Person") +
  ylab("Mean Expenditure") +
  ggtitle("Mean Expenditure With CI") +
  theme_economist_white()
  


