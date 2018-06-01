# load libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)

#import data
home <- '/Users/xing'
project <- paste0(home, '/Documents/Budget/Data')

dat1 <- read.csv(paste0(project, '/budget20162017.csv'))
dat2 <- read.csv(paste0(project, '/budget20152016.csv'))

#### CLEAN DATA ####

#remove white spaces
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
dat1$description <- trim(dat1$description)
dat1$details <- trim(dat1$details)

dat2$description <- trim(dat2$description)
dat2$details <- trim(dat2$details)
dat2$person <- trim(dat2$person)

# change description names for consistency 
dat1$description <- ifelse(dat1$description == "transport", "transportation", dat1$description)

dat1$description <- as.factor(dat1$description)
dat1$details <- as.factor(dat1$details)
dat1$date <- as.Date(dat1$date, format = '%m/%d/%Y')

dat2$description <- as.factor(dat2$description)
dat2$details <- as.factor(dat2$details)
dat2$date <- as.Date(dat2$date, format = '%m/%d/%Y')

########   DATA READY FOR COMPARISON AND ANALYSIS #########

# select only nov-jan from dat2
dat2 <- dat2[1:252,]

# make column with cumulative expenditure for dat1 and dat2
dat1$cumsexp <- cumsum(dat1$expenditure)
dat2$cumexp <- cumsum(dat2$expenditure)

# exp by person each year side by side
expbyprsn2017 <- dat1 %>%
  group_by(person) %>%
  summarise(exp = sum(expenditure))

expbyprsn2016 <- dat2 %>%
  group_by(person) %>%
  summarise(exp = sum(expenditure))

# merge into one and melt
expmerged <- merge(expbyprsn2016, expbyprsn2017, by = "person")
expmelt <- melt(expmerged, idvars = "person")

# plot it
ggplot(expmelt, aes(x=variable, y=value, fill=person)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Year") + 
  ylab("Expenditure") +
  ggtitle("Expenditure by Person Nov 2015-Jan 2016\nand Nov 2016-Jan 2017") +
  scale_x_discrete(labels=c("exp.x" = "11/15-01/16", "exp.y" = "11/16-01/17")) +
  scale_fill_manual(values=c("grey", "orange", "purple")) +
  theme_fivethirtyeight()
  
  
