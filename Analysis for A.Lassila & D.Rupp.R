# Preliminary Set-Up:

## Installing Necessary Packages
library(tidyverse)
install.packages('rstatix'); library(rstatix)
install.packages('gtsummary'); library(gtsummary)

## Loading in data & naming it
PreSurvey <- read.csv("https://raw.githubusercontent.com/abbyvenz/Analysis_for_A.Lassila_and_D.Rupp/main/Culture%20of%20Safety%20Pre%20Survey.csv")
PostSurvey <- read.csv("https://raw.githubusercontent.com/abbyvenz/Analysis_for_A.Lassila_and_D.Rupp/main/Culture%20of%20Safety%20Post%20Survey.csv")

## Recoding the Likert Scales (1-5)
recode_vec = c("Strongly disagree" = 1, "Somewhat disagree" = 2, "Neither agree nor disagree" = 3, "Somewhat agree" = 4, "Strongly agree" = 5)

Recode <- function(x) {
  recode(x, !!!recode_vec)
}

Recoded.PreSurvey = PreSurvey %>% mutate(across(where(~any(. %in% names(recode_vec))), Recode))
Recoded.PostSurvey = PostSurvey %>% mutate(across(where(~any(. %in% names(recode_vec))), Recode))

## Combining the Pre and Post Surveys into one dataset
Combined.Data = merge(Recoded.PreSurvey, Recoded.PostSurvey, by = "UniqueID")



# Analyzing the Data: 

## Summary Statistics of Pre- and Post- Survey Questions
get_summary_stats(Combined.Data[,c(8, 9, 10, 11, 12, 13, 20, 21, 22, 23, 24, 25)], 
                  type = "mean_sd")

## Two-Sample Paired t-Tests for Differences in Means
### Q1
t.test(Combined.Data$Q1_1.y, Combined.Data$Q1_1.x, 
       paired = TRUE)

### Q2
t.test(Combined.Data$Q2_1.y, Combined.Data$Q2_1.x, 
       paired = TRUE)

### Q3
t.test(Combined.Data$Q3_1.y, Combined.Data$Q3_1.x, 
       paired = TRUE)

### Q4
t.test(Combined.Data$Q4_1.y, Combined.Data$Q4_1.x, 
       paired = TRUE)

### Q5
t.test(Combined.Data$Q5_1.y, Combined.Data$Q5_1.x, 
       paired = TRUE)

### Q6
t.test(Combined.Data$Q6_1.y, Combined.Data$Q6_1.x, 
       paired = TRUE)


## Summary of Demographic Info

### Creating a subset of the demographic columns 
data.subset = Combined.Data[,c(15, 16, 17, 18, 19)]

### Cleaning up the "Years as a Nurse" column 
### - to make less unique responses
data.subset[data.subset$Q10.y == "1", "Q10.y"] <- "< 5"
data.subset[data.subset$Q10.y == "3", "Q10.y"] <- "< 5"
data.subset[data.subset$Q10.y == "4", "Q10.y"] <- "< 5"
data.subset[data.subset$Q10.y == "4.5", "Q10.y"] <- "< 5"

data.subset[data.subset$Q10.y == "5", "Q10.y"] <- "5-10"

data.subset[data.subset$Q10.y == "13", "Q10.y"] <- "10+"
data.subset[data.subset$Q10.y == "17", "Q10.y"] <- "10+"
data.subset[data.subset$Q10.y == "28", "Q10.y"] <- "10+"

### Using that subset to summarize the demographic info
data.subset %>% tbl_summary(
  statistic = list(all_categorical() ~ "{n} ({p})")
)

