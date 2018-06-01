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
dat1 <- read.csv(paste0(project, "/budget20162017.csv"))

#### CLEAN DATA ####
# function to get rid of whitespaces
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

dat1$person <- trim(dat1$person)
dat1$description <- trim(dat1$description)
dat1$details <- trim(dat1$details)

# change date format
dat1$date <- as.Date(dat1$date, format = "%m/%d/%Y")

#change columns into factors
dat1$person <- as.factor(dat1$person)

#change snack into snacks for consistency
dat1$description <- ifelse(dat1$description == "snack", "snacks", dat1$description)
dat1$description <- ifelse(dat1$description == "transport", "transportation", dat1$description)
#turn description into factor
dat1$description <- as.factor(dat1$description)

#summary of dat1 descriptions
summary(dat1$description)

##### CLEANED DATA #####

#let's see how much each person spent
exp_by_person <- dat1 %>%
  group_by(person, description) %>%
  summarise(exp_person = sum(expenditure), na.rm = T)
  
#plotted
ggplot(exp_by_person, aes(person, exp_person, group = description, fill = description)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) + 
  xlab("Person") + ylab("Expenditure") + ggtitle("Spending by Person Nov 4, 2016 - Jan 2017") +
  theme_fivethirtyeight()

# cumulative expenditure per each description category
for (i in dat1$description) {
  dat1$cum_exp[dat1$description == i] <- cumsum(dat1$expenditure[dat1$description == i]) 
}

# cumulative expenditure on lunch
cum_exp_lunch <- dat1[dat1$description == "lunch",]

ggplot(cum_exp_lunch, aes(x=date, y=cum_exp)) +
  geom_line() +
  geom_smooth() +
  xlab("Date") +
  ylab("Cumulative Expenditure on Lunch") +
  ggtitle("Lunch Spending") + theme_economist()

# cumulative spending on dinner
cum_exp_dinner <- dat1[dat1$description == "dinner",]

ggplot(cum_exp_dinner, aes(x=date, y=cum_exp)) +
  geom_line() +
  geom_smooth() +
  xlab("Date") +
  ylab("Cumulative Expenditure on Dinner") +
  ggtitle("Dinner Spending") + theme_economist()

# line graph showing cumulative expenditure for all description categories 
ggplot(dat1, aes(x=date, y=cum_exp, color = description)) +
  geom_line(size = .75, alpha = 0.6) +
  xlab("Date") +
  ylab("Cumulative Expenditure") +
  ggtitle("Cumulative Expenditure Over Time by Description") +
  theme_fivethirtyeight()
