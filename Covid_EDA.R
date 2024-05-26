#import libraries that will be used
library(ggplot2)
library(dplyr)
library(moments)

#import the dataset
df <- read.csv("Global COVID-19 Dataset.csv")

#visualise the dataset
View(df)

#inspect the dataset
str(df)
head(df)
summary(df)

#check for duplicates
df[duplicated(df),]
#there are no duplicates except for the null values

#remove rows with null values
df <- na.omit(df)

#remove the "S..No." column since there is no use for it
df <- df[,-1]

#create columns with percentages of deaths and recovered on the total number of cases
df <- df %>% 
  mutate(DeathRate = df$Deaths/df$Cases * 100,
         RecoveryRate = df$Recovered/df$Cases * 100)

#Summary statistics of each numeric column
summary(df$Cases)
summary(df$Deaths)
summary(df$Recovered)
summary(df$DeathRate)
summary(df$RecoveryRate)

#countries with the highest and the lowest number of cases
df[df$Cases == max(df$Cases) | df$Cases == min(df$Cases), c("Country.Name","DeathRate" ) ]

#countries with the highest and the lowest Death Rates
df[df$DeathRate == max(df$DeathRate) | df$DeathRate == min(df$DeathRate), c("Country.Name","DeathRate" )]

#check the correlation between Cases and Death Rates
ln_Cases_DeathRate <- lm(df$DeathRate~df$Cases)
plot(df$Cases,df$DeathRate, col='red')
abline(ln_Cases_DeathRate, col='purple',lwd='2')
ln_Cases_DeathRate

#check countries that recovered the highest percentage of people
Top_10_Recovery <- df[head(order(df$RecoveryRate, decreasing=TRUE)[1:10]), c("Country.Name","RecoveryRate")]
Top_10_Recovery

# Histogram of RecoveryRate
ggplot(df, aes(x = RecoveryRate)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Recovery Rates", x = "Recovery Rate", y = "Frequency")

skewness(df$RecoveryRate) 
kurtosis(df$RecoveryRate)
