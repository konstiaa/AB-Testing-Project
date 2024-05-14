library(dplyr)
library(tidyverse)
library(sjmisc)
library(pastecs)
library(ggplot2)
library(stats)
library(tidyr)
library(gt)
##Loading and previewing the data

data <-read.csv("online_ad_AB.csv")
head(data)
glimpse(data)

##Data Cleaning
count_na(data)
##the data has no null values

#Data Processing
#We need to convert the test.group variable to a factor
data$test.group <- as.factor(data$test.group)
data$made_purchase <- as.logical(data$made_purchase)
data$test.group <- str_to_upper(data$test.group)

#Then we need to create bins for peak ad hours to simplify data profiling

data$time_of_day <- cut(data$peak.ad.hours, breaks = c(-1, 6, 12, 18, 24), labels = c("0-6", "6-12", "12-18", "18-24"))

#Data Profliing (checking the distribution of the data in a numeric form)

frq(data$test.group)
#as we can see, 60% of the total sample is in the experimental group and 40% in the control group
data %>% group_by(test.group) %>% frq(made_purchase)
#we can already notice that the experimental group has a higher conversion rate
#however, we need to check if this difference is statistically significant later on
frq(data$time_of_day)

#As we can see, around 80% of the peak ad hours are between 12 a.m and 12 p.m


#Data Analysis

#We will be conducting a proportion z-test

total <- data %>% group_by(test.group) %>% count()

successes <- data %>% filter(made_purchase == 1) %>% 
  group_by(test.group) %>% count()

prop.test(successes$n, total$n, alternative = "greater", conf.level = 0.95)
#the p-value is way less than 0.5 
#(actually, it is the minimal p-value that is allowed by R), 
#thus, we can reject the null hypothesis

#Data Visualization
bar_chart <- data %>% ggplot(aes(x = made_purchase)) + geom_bar(aes(fill = made_purchase), position = 'dodge') + 
  labs(title = "Conversions by Test Group", x = "Made Purchase", y = "Count") + scale_x_discrete(breaks = c(FALSE,TRUE),
                                                                                                   labels = c('No Coversion', 'Coversion')) +
  facet_wrap(~test.group) + theme(legend.position = "none")
bar_chart

histogram <- data %>% ggplot(aes(x = time_of_day)) + geom_histogram(aes(fill = made_purchase), stat = 'count') + 
  labs(title = "Conversions by Time of Day", x = "Time of Day", y = "Count") +
  facet_wrap(~made_purchase, labeller = as_labeller(c('FALSE' = 'No Conversion',
                                                      'TRUE' = 'Conversion'))) + theme(legend.position = "none")
histogram


ggsave('histogram_time_dist.png', plot = last_plot())
ggsave('histogram_conversions.png', plot = last_plot())
ggsave('bar_chart.png', plot = last_plot())
