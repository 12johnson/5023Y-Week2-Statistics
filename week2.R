library(tidyverse)
library(skimr)
library(rstatix)

### adding in packages which are used in the script


darwin<- read.csv("Data/darwin.csv")

### reading the dataset in and assigning it as an object

head(darwin)  
nrow(darwin) 
str(darwin) 
unique(darwin$pot)
skim(darwin) 
is.na(darwin)

### checking that the data is tidy, seeing if there are any missing values or errors, etc.

darwin <- darwin %>% 
  pivot_longer(cols=c("Self":"Cross"), names_to="type", values_to="height") %>% 
  mutate(pair=as.factor(pair), pot=as.factor(pot), type=as.factor(type))

###tidying data
### pivot longer to be able to make a cross and self one column and making the heights all into one column
### changing column data types to more suitable ones

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()

### plots a dotplot comparing the heights of cross and self-pollinated maize plants
### plot suggests that the mean height may be taller for cross-pollinated than self-pollinated (inbred)

model1 <- lm(height~1, data=darwin)
model1

### code calculates the mean height, we can now use this value as an intercept in our dotplot
### mean calculated was 18.88

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_abline(intercept=18.88, 
              slope=0, 
              linetype="dashed")

### adds dashed line indicating the mean height of all plants to the dotplot

darwin %>% 
  summarise(mean=mean(height))

### code calculates the mean Height to confirm result from lm() function
### mean calculated was 18.9 which confirms the mean is accurately estimated

model2 <- lm(height~type, data=darwin)
model2

### this linear model analyses a dependent variable (height) as a function 
### of a categorical independent variable (type of pollination)
### values calculated is a new intercept 20.192 and a slope -2.617
### this slope indicates the difference between the mean heights for cross and self-pollinated
### 20.192 is the mean for cross-pollinated 
### and 20.192-2.617= 17.575 is the mean for self-pollinated (indicated by -2.617 being under typeself)

model3 <- lm(height~1+type, data=darwin)
model3

### this code produces the same output as model 2

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

### plots a dotplot with a dashed line connecting the two means
### code manually specifies two means and includes their values 
### so R does not create an average as shown in previous graph

summary(model2)

### produces test statistics. 

anova_test(height~type, data=darwin)

### ANOVA test matches the last line of the summary() data confirming it is accurate.
### signal-to-noise ratio F= 5.9, p value = 0.02

pf(q=5.9395, df1=1, df2=28, lower.tail=FALSE)

### the code above tests the null hypothesis by calculating the p value. 
### the code is to see the probability of the F statistic hence pf() 
### the signal-to-nose ratio is the F statistic but is
### represented as q in this code
### F statistic and df and P-value must be given together
### as the F statistic and df are required to check the P-value.


### we can replicate this result with the pf() which is why it is vital 
### that when you report results, you always report the test-statistic (here F), 
### degrees of freedom (df) and P-value together, 
### Your significance or probability value can only be calculated/
### checked when we have the sample size and signal-to-noise ratio.

### the p value shows that it is a 2% chance that we have produced a false positive
### 5% is the cut off, so...

### we can reject the null hypothesis that there is no difference between the height of
### cross and self-pollinated maize and assume H1 to be correct.

###The cross-pollinated plants were on average significantly taller (20.2 inches)
###than the self pollinated plants (17.5 inches) F1,28= 5.94, P = 0.02.
### above is an example of the write up you would use in a paper.