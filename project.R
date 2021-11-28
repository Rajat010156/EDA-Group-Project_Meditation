# Cleaning environment
rm(list=ls())
graphics.off()

# Load the libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(gmodels)
library(webr)

# Set the directory
setwd("C:/Users/Rajat Gaur/Desktop/Working Projects/")

# Needed things
source("C:/Users/Rajat Gaur/Desktop/Working Projects/needed.R")

# Load the data
meditation <- read.csv("meditation.csv", header = TRUE)
meditation %>% glimpse()


###Here we have dropped the non required data of time and additional two columns named x and x.1 which had randomly text but no header //seems it was an error from Google form end.
##now we have stored clean data in data frame named "med"
med <- subset(meditation, select = -c(Timestamp, X ,X.1))
med %>% glimpse()

#To assign names to columns  to make the analysis and codes legible (#the abbreviated fom and their explanation is attached in a text file in the folder submission)
names(med) <- c("m_whether", "m_form", "m_reason","m_when" , "m_often", "m_duration", "stress","anger","emotions", "calm", "confidence", "health",                                                                 
"clarity","energy", "motivation" ,"focus" ,"sleep","recommend", "age",   "gender",  "name", "comments" )
med %>% glimpse()
med <- med %>% mutate_at(1:20, as.factor)

# Write the file
write.csv(med, "clean_data.csv")

## In order to filter out contradictory data points, filter operation have been used to visualize  
med1 <- med %>% 
  filter((m_whether=="Yes" & m_when!="Don't meditate") | (m_whether=="No" & m_when=="Don't meditate"))

med1 %>% glimpse()
respondents <- table(med1$gender);respondents   

### Visualizatin_1 # For understanding the gender distribution of our Respondents

#1
whether <- table(med1$m_whether); whether   
per = round((whether/nrow(med1))*100);per
lbl = paste(names(whether), per, "%");lbl
pie(whether, main = "Ratio of Responses" , col = c(7,8), labels =lbl)

#2
age <- table(med1$age); age   
per = round((age/nrow(med1))*100);per
lbl = paste0(names(age),' - ',per , "%");lbl
pie(age, main = "Ratio of Responses based on age" , col = c(2,3,4,7,8), labels =lbl)

#3
gen <- table(med1$gender); gen 
per = round((gen/nrow(med1))*100);per
lbl = paste(names(gen), per, "%");lbl
pie(gen, main = "Male and Female Percentage" , col = c(2,4,8), labels =lbl)


#4
### Visualizatin_2 # For understanding the gender wise distribution of our Respondents who do meditation or not.
med1 %>% 
    group_by(m_whether, gender) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(x=m_whether, y = count, fill=gender)) +
    geom_bar(stat = 'identity') + coord_flip() + 
    labs(x= "Do you Meditate" , title = "Gender Wise Distribution of Meditation")

#5
### Visualization_ # Understanding the age wise count of people who do meditation or not!

# ggplot(data = med1,  mapping = aes(x=age,fill=m_whether))+
#   geom_bar(position="dodge")+
#   labs(x="Different age groups",y="Number of people",
#        title="Perception of People towards Meditation - Age Wise")+
#   annotate("text",x=c(1,1.8,2.2,2.8,3.2,3.8,4.2),y=c(1,41,24,11,18,1,5)+1,
#            label=c(1,41,24,11,18,1,5))

df <- med1 %>% 
    group_by(m_whether, age) %>% 
    summarise(count = n())
PieDonut(df, aes(m_whether, age, count = count), showPieName = F,
         showRatioThreshold = 0.01, title="Perception of People towards Meditation - Age Wise")    


#6
##Visualization_ ## For understanding the well being factors of respondents basis they meditate or not!
#Stress 
#Anger 
#Emotional 
#Calm

p1 <- ggplot(data = med1, mapping = aes(x =m_whether, fill=stress))   + geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Stress", fill="");p1
p2 <- ggplot(data = med1, mapping = aes(x =m_whether, fill=anger))    + geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Anger Management", fill="");p2
p3 <- ggplot(data = med1, mapping = aes(x =m_whether, fill=emotions)) + geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Emotional Balance", fill="");p3
p4 <- ggplot(data = med1, mapping = aes(x =m_whether, fill=calm))     + geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Calmness", fill="");p4

ggarrange(p1,p2,p3,p4,common.legend = T, nrow = 1)

p5 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=confidence)) + geom_bar(width = 0.5) + my_theme + labs(x= "" ,  title = "Confidence", fill="");p5
p6 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=health))     + geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Health", fill="");p6
p7 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=clarity))    + geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Clarity", fill="");p7
p8 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=energy))     + geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Energy", fill="");p8
ggarrange(p5,p6,p7,p8,common.legend = T, nrow = 1)

p9 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=motivation)) + geom_bar(width = 0.5) + my_theme + labs(x= "" ,  title = "Motivation", fill="");p1
p10 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=focus)) +     geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Focus", fill="");p2
p11<- ggplot(data = med2, mapping = aes(x =m_whether, fill=sleep)) +      geom_bar(width = 0.5) + my_theme + labs(x= "", title = "Sleep", fill="");p3

ggarrange(p9,p10,p11,common.legend = T, nrow = 1)

#7
## most practiced  form of meditation 
med2 %>% 
    filter(m_whether == 'Yes') %>% 
    group_by(m_form) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(m_form, count)) + geom_bar(stat = 'identity') + coord_flip()

## Conclusion: breathwork most practised method
# 
# med2 %>% 
#     filter(m_whether == 'Yes') %>% 
#     group_by(m_form, stress) %>% 
#     summarise(count = n()) %>% 
#     ggplot(aes(m_form, count, fill = stress)) + geom_bar(stat = 'identity', position = 'stack') + coord_flip()


#8
### Visualization_ Respondents Experience of Meditation - Month/year!
##########################################################################
med3 <- med1 %>% filter(m_whether == "Yes")
exp1 <- ggplot(data = med3)+geom_bar(aes(x=m_when,fill=age))    + my_theme + labs(x = "", y = "m_when", title = "") + coord_flip();exp1
exp2 <- ggplot(data = med3)+geom_bar(aes(x=m_often,fill=age))   + my_theme + labs(x = "", y = "m_often", title = "") + coord_flip();exp2
exp3 <- ggplot(data = med3)+geom_bar(aes(x=m_duration,fill=age))+ my_theme + labs(x = "", y = "m_duration", title = "") + coord_flip();exp3

ggarrange(exp1,exp2,exp3,nrow=1,common.legend = T)



# med2 %>% filter(m_whether == "No") %>% group_by(stress) %>% summarise(n())

#9
###Top 2 worst affected factors in terms of perception of people who don't meditate and comparing with the perception of people who are meditating 

med2 %>% 
    filter(stress == "agree" | stress == "Strongly Agree") %>% 
    group_by(m_whether, stress) %>% 
    summarise(count = n())

med2 %>% 
    filter(anger == "Disagree" | anger == "Strongly Disagree") %>% 
    group_by(m_whether, anger) %>% 
    summarise(count = n())

med2 %>% 
    filter(emotions == "Disagree" | emotions == "Strongly Disagree") %>% 
    group_by(m_whether, emotions) %>% 
    summarise(count = n())

med2 %>% 
    filter(calm == "Disagree" | calm == "Strongly Disagree") %>% 
    group_by(m_whether, calm) %>% 
    summarise(count = n())

med2 %>% 
    filter(confidence == "Disagree" | confidence == "Strongly Disagree") %>% 
    group_by(m_whether, confidence) %>% 
    summarise(count = n())

med2 %>% 
    filter(health == "Disagree" | health == "Strongly Disagree") %>% 
    group_by(m_whether, health) %>% 
    summarise(count = n())

med2 %>% 
    filter(clarity == "Disagree" | clarity == "Strongly Disagree") %>% 
    group_by(m_whether, clarity) %>% 
    summarise(count = n())

med2 %>% 
    filter(energy == "Disagree" | energy == "Strongly Disagree") %>% 
    group_by(m_whether, energy) %>% 
    summarise(count = n())

med2 %>% 
    filter(motivation == "Disagree" | motivation == "Strongly Disagree") %>% 
    group_by(m_whether, motivation) %>% 
    summarise(count = n())

med2 %>% 
    filter(focus == "Disagree" | focus == "Strongly Disagree") %>% 
    group_by(m_whether, focus) %>% 
    summarise(count = n())

med2 %>% 
    filter(sleep == "Disagree" | sleep == "Strongly Disagree") %>% 
    group_by(m_whether, sleep) %>% 
    summarise(count = n())


df <- data.frame(var =  names(med1)[7:17],
                 freq = c(24,11,13,9,11,17,13,7,9,10,17))
df %>% 
    ggplot(aes(var, freq)) + 
    geom_bar(stat = 'identity', fill = "steelblue3") + coord_flip() +
    my_theme


#10 
### Comparing worst 2 factors - stress and sleep for people whho meditate and who don't 
## Stress 
st1 <- med2 %>% group_by(m_whether, stress) %>% 
    summarise(count=n()) %>% 
    ggplot(aes(x=m_whether, y = count, fill = stress)) + geom_bar(stat='identity', position = 'stack')

st2 <- med2 %>% group_by(m_when, stress) %>% 
    summarise(count=n()) %>% 
    ggplot(aes(x=m_when, y = count, fill = stress)) + geom_bar(stat='identity', position = 'stack')

st3 <- med2 %>% group_by(m_often, stress) %>% 
    summarise(count=n()) %>% 
    ggplot(aes(x=m_often, y = count, fill = stress)) + geom_bar(stat='identity', position = 'stack')

ggarrange(st1,st2,st3, common.legend = T)


## Sleep  

sl1 <- med2 %>% group_by(m_whether, sleep) %>% 
    summarise(count=n()) %>% 
    ggplot(aes(x=m_whether, y = count, fill = sleep)) + geom_bar(stat='identity', position = 'stack')

sl2 <- med2 %>% group_by(m_when, sleep) %>% 
    summarise(count=n()) %>% 
    ggplot(aes(x=m_when, y = count, fill = sleep)) + geom_bar(stat='identity', position = 'stack')

sl3 <- med2 %>% group_by(m_often, sleep) %>% 
    summarise(count=n()) %>% 
    ggplot(aes(x=m_often, y = count, fill = sleep)) + geom_bar(stat='identity', position = 'stack')

ggarrange(sl1,sl2,sl3, common.legend = T)


#11 Reason for not being able to build up the habit of Mediation 
med2 %>% filter(m_reason!= "") %>% 
  group_by(m_reason) %>% 
  summarise(count = n())  %>% 
  ggplot(aes(x=m_reason, y=count)) + geom_bar(stat = 'identity') + coord_flip()


#12
##Recommendation of People to do meditation 

med2 %>% 
    filter(recommend != '') %>% 
    group_by(m_whether, recommend) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(recommend, count, fill = m_whether)) + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    my_theme
