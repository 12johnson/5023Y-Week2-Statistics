library(tidyverse)
library(skimr)

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





