# AB-Testing-Project



**Introduction**

A large company with a substantial user base plans to increase sales through advertisement on its website. However, they still have not made up their minds about whether the ads increase sales or not. In order to determine whether this is the case, 20000 customers were subjected to A/B testing for 31 days. The company wants to know if the ads have a significant impact on sales.

**Part 1: Data Loading**

Firstly, the libraries required to conduct the test will be imported.

```{r}
library(dplyr)
library(tidyverse)
library(sjmisc)
library(pastecs)
library(ggplot2)
library(stats)
library(tidyr)
library(gt)
```

Then, the data will be loaded and previewed.

```{r}
data <-read.csv("online_ad_AB.csv")
head(data)
```
We can also get a glimpse of the data to understand its structure and the data types.

```{r}
glimpse(data)
```
**Part 2: Data Cleaning and Processing**

To ensure that the data is clean, we will check for any missing values.

```{r}
count_na(data)
```
As we can see, the data has no null values. Thus, we can move on to the other parts of the data processing phase.

We need to convert the test.group variable to a factor and the made_purchase variable to a logical type. In addition, we will modify the appearance of the test.group variable to ensure that it looks nice in the visualizations, which will be created later on.

```{r}
data$test.group <- as.factor(data$test.group)
data$made_purchase <- as.logical(data$made_purchase)
data$test.group <- str_to_upper(data$test.group)
```

Then, we need to create bins as a separate column for peak ad hours to simplify data profiling.

```{r}
data$time_of_day <- cut(data$peak.ad.hours, breaks = c(-1, 6, 12, 18, 24), labels = c("0-6", "6-12", "12-18", "18-24"))
```

**Part 3: Data Profiling**

We will check the distribution of the test.group variable in text format to get a better understanding of the variables we are about to analyze.

```{r}
frq(data$test.group)
```
As we can see, 60% of the total sample is in the experimental group and 40% in the control group. 

Next, we will check the distribution of the made_purchase variable grouped by the test.group variable to see the differences in conversions.

```{r}
data %>% group_by(test.group) %>% frq(made_purchase)
```

We can already notice that the experimental group has a higher conversion rate. However, we need to check if this difference is statistically significant later on.

Lastly, we will check the distribution of the time_of_day variable to understand the distribution of the peak ad hours.

```{r}
frq(data$time_of_day)
```
As we can see, around 80% of the peak ad hours are between 12 a.m and 12 p.m

**Part 4: Making Hypotheses**

The next step in the A/B testing process is to formulate a hypothesis. In this case, we hypothesize that the use of ads can lead to an increase in sales.

Null hypothesis is that there is no significant difference in sales between the 'ads' group and the 'psa' group.

Alternative hypothesis is that the purchase rate in the 'ads' group is higher than that in the 'psa' group.

**Part 5: Data Analysis**

Since we have done the proper profliing of the data in the steps prior, we can now easily determine the statistical criteria for the A/B test. For binary, user-level conversion metrics it is better to use the proportional Z-Test. There is no need to check the assumption of normality of the distribution since we can safely approximate the distribution of the Binomial proportional statistics using normal distributions.
(Reference: https://towardsdatascience.com/how-to-select-the-right-statistical-tests-for-different-a-b-metrics-c8a1865851e#f15d)

Before we start the test, we need to conduct some preliminary calculations, so that we can easily insert the results into the function.

```{r}
total <- data %>% group_by(test.group) %>% count()

successes <- data %>% filter(made_purchase == 1) %>% 
  group_by(test.group) %>% count()
```

We have created two data frames, columns from which will be derived to calculate the test statistics. Now, we can conduct the test.

```{r}
prop.test(successes$n, total$n, alternative = "greater", conf.level = 0.95)
```
As we can see, the p-value is less than 0.05, which means that we can reject the null hypothesis. This means that the ads have a significant impact on sales.

**Part 6: Data Visualization**

It is essential to prepare some visualizations for stakeholders to understand the results better. We will start by creating a bar chart to show the distribution of the test.group variable.

```{r}
bar_chart <- data %>% ggplot(aes(x = made_purchase)) + geom_bar(aes(fill = made_purchase), position = 'dodge') + 
  labs(title = "Conversions by Test Group", x = "Made Purchase", y = "Count") + scale_x_discrete(breaks = c(FALSE,TRUE),                                                                   labels = c('No Coversion', 'Coversion')) +
  facet_wrap(~test.group) + theme(legend.position = "none")
bar_chart
```

This plot demonstrates that the experimental group has both a bigger amount of subjects and a higher conversion rate than the control group.

Next, we will create a histogram to show the conversions by the time of day.

```{r}
histogram <- data %>% ggplot(aes(x = time_of_day)) + geom_histogram(aes(fill = made_purchase), stat = 'count') + 
  labs(title = "Conversions by Time of Day", x = "Time of Day", y = "Count") +
  facet_wrap(~made_purchase, labeller = as_labeller(c('FALSE' = 'No Conversion',
                                                      'TRUE' = 'Conversion'))) + theme(legend.position = "none")
histogram
```

**Part 7: Conclusion**

In conclusion, the A/B test has shown that the ads have a significant impact on sales. The experimental group has a higher conversion rate than the control group. This means that the company should continue to use ads to increase sales.

