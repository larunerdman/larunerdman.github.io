library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(reshape2)
library(readr)

dat1 <- read_csv("/Users/xing/Documents/Budget/Data/budget2018.csv")

##### data cleaning ##### 

# function to get rid of whitespaces
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

dat1$person <- trim(dat1$person)
dat1$description <- trim(dat1$description)
dat1$details <- trim(dat1$details)

# make all lowercase
dat1$person <- tolower(dat1$person)

# change date format
dat1$date <- as.Date(dat1$date, format = "%m-%d-%Y")

# as.factor for person, payment type
dat1$person <- as.factor(dat1$person)
dat1$description <- as.factor(dat1$description)
dat1$payment_type <- as.factor(dat1$payment_type)

# delete notes
dat1$notes <- NULL

##### analysis #####

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
  theme_minimal() +
  scale_colour_manual(name = "Person",
                      values = c('#328cc1', '#ff9900', '#45a06c', '#504168'),
                      breaks = c("ben", "both", "databrew","xing"),
                      labels = c("Ben", "Both", "Databrew", "Xing"))

# show what each person spending money on

exp_by_person <- dat1 %>%
  filter(person != 'databrew' & description != 'home_darcy') %>%
  group_by(person, description) %>%
  summarise(exp_person = sum(expenditure), na.rm = T)

ggplot(exp_by_person, aes(person, exp_person, group = description, fill = description)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) + 
  xlab("Person") + ylab("Expenditure") + ggtitle("Spending by Person Nov 24 to Jan 27, 2018") +
  theme_minimal()

# cumulative expenditure per each description category
for (i in dat1$description) {
  dat1$cum_exp[dat1$description == i] <- cumsum(dat1$expenditure[dat1$description == i]) 
}

# line graph showing cumulative expenditure for all description categories 

ben <- dat1 %>%
  filter(person == 'ben') 

for (i in ben$description) {
  ben$cum_exp[ben$description == i] <- cumsum(ben$expenditure[ben$description == i]) 
}

ggplot(ben, aes(x=date, y=cum_exp, color = description)) +
  geom_line(size = 1, alpha = 0.6) +
  geom_point() +
  xlab("Date") +
  ylab("Cumulative Expenditure") +
  ggtitle("Ben's Expenditure Over Time by Description") +
  theme_minimal()

xing <- dat1 %>%
  filter(person == 'xing')

for (i in xing$description) {
  xing$cum_exp[xing$description == i] <- cumsum(xing$expenditure[xing$description == i]) 
}

ggplot(xing, aes(x=date, y=cum_exp, color = description)) +
  geom_line(size = 1, alpha = 0.6) +
  geom_point() +
  xlab("Date") +
  ylab("Cumulative Expenditure") +
  ggtitle("Xing's Expenditure Over Time by Description") +
  theme_minimal()

both <- dat1 %>%
  filter(person == 'both' & expenditure <= 500)

for (i in both$description) {
  both$cum_exp[both$description == i] <- cumsum(both$expenditure[both$description == i]) 
}

ggplot(both, aes(x=date, y=cum_exp, color = description)) +
  geom_line(size = 1, alpha = 0.6) +
  geom_point() +
  xlab("Date") +
  ylab("Cumulative Expenditure") +
  ggtitle("Both Expenditure Over Time, Purchases under $500") +
  theme_minimal()

## payment type ## 
