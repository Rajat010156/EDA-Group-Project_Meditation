# Cleaning environment
rm(list=ls())
graphics.off()

# Load the libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(gmodels)
library(webr)
library(ggrepel)

# Set the directory
setwd("C:/Users/USER/Documents")

# Needed things
source("needed.R")

# Load the data
meditation <- read.csv("MED_DATA.csv", header = TRUE)
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
## 1
pd1 <- med1 %>% 
  group_by(m_whether) %>% 
  summarise(count = n())
PieDonut(pd1, aes(m_whether,count = count), showPieName = F,r0=0.2)+
   labs(title="                      Ratio of Responses")   



#whether <- table(med1$m_whether); whether   
#per = round((whether/nrow(med1))*100);per
#lbl = paste(names(whether), per, "%");lbl
#pie(whether, main = "Ratio of Responses" , col = c(7,8), labels =lbl)



## 2
pd2 <- med1 %>% 
  group_by(age) %>% 
  summarise(count = n())
PieDonut(pd2, aes(age,count = count),showRatioThreshold = 0.001, showPieName = F,r0=0.2)+
  labs(title="                      Ratio of Responses Based on Age")   






#age <- table(med1$age); age   
#per = round((age/nrow(med1))*100);per
#lbl = paste0(names(age),' - ',per , "%");lbl
#pie(age, main = "Ratio of Responses based on age" , col = c(2,3,4,7,8), labels =lbl)



## 3
pd3 <- med1 %>% 
  group_by(gender) %>% 
  summarise(count = n())
PieDonut(pd3, aes(gender,count = count),showRatioThreshold = 0.01, showPieName = F,r0=0.2)+
  labs(title="                Male and Female Percentage")   




#gen <- table(med1$gender); gen 
#per = round((gen/nrow(med1))*100);per
#lbl = paste(names(gen), per, "%");lbl
#pie(gen, main = "Male and Female Percentage" , col = c(2,4,8), labels =lbl)


### Visualizatin_2 # For understanding the gender wise distribution of our Respondents who do meditation or not.

## 4
g1 <- med1 %>% 
  group_by(m_whether, gender) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x=m_whether, y = count, fill=gender)) +
  geom_bar(stat = 'identity',position ="dodge") +scale_fill_manual(values = c("black","grey","blue"))+  
  geom_text(aes(y=count+1,label = count), position = position_dodge(0.9))+
  labs(x= "Do you Meditate" , title = "Gender Wise Distribution of Meditation")+
  my_theme
print(g1+labs(fill="Gender"))

### Visualization_ # Understanding the age wise count of people who do meditation or not!

## 5
g2 <- ggplot(data = med1,  mapping = aes(x=age,fill=m_whether))+
    geom_bar(position="dodge")+scale_fill_manual(values=c("#054C70","#05C3DE"))+
    labs(x="Different age groups",y="Number of people",
       title="Perception of People towards Meditation - Age Wise")+
  annotate("text",x=c(1,1.8,2.2,2.8,3.2,3.8,4.2),y=c(1,41,24,11,18,1,5)+1,
           label=c(1,41,24,11,18,1,5))+my_theme
print(g2+labs(fill="Do You Meditate"))

#df <- med1 %>% 
 # group_by(m_whether, age) %>% 
#  summarise(count = n())
#PieDonut(df, aes(m_whether, age, count = count), showPieName = F,
 #        showRatioThreshold = 0.01, title="Perception of People towards Meditation - Age Wise")    

##Visualization_ ## For understanding the well being factors of respondents basis they meditate or not!
#Stress 
#Anger 
#Emotional 
#Calm
med2 <- med1 
str(med2)


unique(med2$m_when)
levels(med2$m_when) <- c("Less than 3 months",
                         "More than 1 year",                          
                         "Don't meditate",                             
                         "3 to 6 months", 
                         "6 months to 1 year")


unique(med2$m_duration)
med2$m_duration <- recode_factor(med2$m_duration,
                `More than 15 minutes but less than 30 minutes` ="15 to 30 minutes", 
                `More than 30 minutes but less than 45 minutes` = "30 to 45 minutes")
levels(med2$m_duration)

View(med2)

## 6
meds1 <- xtabs(~m_whether+stress,med2)
meds1
meds1 <- as.data.frame(meds1)

p1 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=stress)) +
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Stress", fill="")+  
  geom_text(meds1,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p1

meds2 <- xtabs(~m_whether+anger,med2)
meds2
meds2 <- as.data.frame(meds2)

p2 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=anger)) + 
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Anger Management",fill="")+
  geom_text(meds2,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p2

meds3 <- xtabs(~m_whether+emotions,med2)
meds3
meds3 <- as.data.frame(meds3)

p3 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=emotions)) +
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Emotional Balance", fill="")+
  geom_text(meds3,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p3

meds4 <- xtabs(~m_whether+calm,med2)
meds4
meds4 <- as.data.frame(meds4)

p4 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=calm)) +
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Calmness", fill="")+
  geom_text(meds4,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p4


t1 <- ggarrange(p1,p2,p3,p4,common.legend = T, nrow = 1)
annotate_figure(t1,top = text_grob("Perception Of People Towards Well-Being Factors"))        


meds5 <- xtabs(~m_whether+confidence,med2)
meds5
meds5 <- as.data.frame(meds5)

p5 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=confidence)) + 
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate" ,  title = "Confidence", fill="")+
  geom_text(meds5,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p5

meds6 <- xtabs(~m_whether+health,med2)
meds6
meds6 <- as.data.frame(meds6)

p6 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=health)) + 
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Health", fill="")+
  geom_text(meds6,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p6


meds7 <- xtabs(~m_whether+clarity,med2)
meds7
meds7 <- as.data.frame(meds7)

p7 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=clarity)) +
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Clarity", fill="")+
  geom_text(meds7,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p7


meds8 <- xtabs(~m_whether+energy,med2)
meds8
meds8 <- as.data.frame(meds8)

p8 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=energy)) +
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Energy", fill="")+
  geom_text(meds8,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p8


t2 <- ggarrange(p5,p6,p7,p8,common.legend = T, nrow = 1)
annotate_figure(t2,top = text_grob("Perception Of People Towards Well-Being Factors"))



meds9 <- xtabs(~m_whether+motivation,med2)
meds9
meds9 <- as.data.frame(meds9)

p9 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=motivation)) +
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate" ,  title = "Motivation", fill="")+
  geom_text(meds9,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p9


meds10 <- xtabs(~m_whether+focus,med2)
meds10
meds10 <- as.data.frame(meds10)

p10 <- ggplot(data = med2, mapping = aes(x =m_whether, fill=focus)) +
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Focus", fill="")+
  geom_text(meds10,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p10

meds11 <- xtabs(~m_whether+sleep,med2)
meds11
meds11 <- as.data.frame(meds11)

p11<- ggplot(data = med2, mapping = aes(x =m_whether, fill=sleep)) +
  geom_bar(width = 0.5) + my_theme + labs(x= "Do You Meditate", title = "Sleep", fill="")+
  geom_text(meds11,mapping = aes(y=Freq,label= Freq), stat="identity",
            position = "stack",vjust=1);p11


t3 <- ggarrange(p9,p10,p11,common.legend = T, nrow = 1)
annotate_figure(t3,top = text_grob("Perception Of People Towards Well-Being Factors"))


## most practiced  form of meditation 

## 7
m_form <- c("Pranayama","Vipasana/Buddhist meditation","Breathwork",
            "Sudarshan Kriya","Shambhavi Mahamudra Kriya")
freq <- rep(0,5)
freq[1] <- length(grep("Pranayama", med2$m_form))
freq[2] <- length(grep("Vipasana/Buddhist meditation", med2$m_form))
freq[3] <- length(grep("Breathwork", med2$m_form))
freq[4] <- length(grep("Sudarshan Kriya", med2$m_form))
freq[5] <- length(grep("Shambhavi Mahamudra Kriya", med2$m_form))
df <- data.frame(m_form, freq)
ggplot(df, aes(m_form, freq))+geom_bar(stat = 'identity', fill = "tomato4")+
  coord_flip()+my_theme+labs(y = "Frequency", x = " Forms of Meditation",
                              title = "Distribution of Meditative Forms ")



## Conclusion: breathwork most practised method

#med2 %>% 
 # filter(m_whether == 'Yes') %>% 
  #group_by(m_form, stress) %>% 
  #summarise(count = n()) %>% 
  #ggplot(aes(m_form, count, fill = stress)) + geom_bar(stat = 'identity', position = 'stack') + coord_flip()


### Visualization_ Respondents Experience of Meditation - Month/year!
##########################################################################

## 8
med3 <- med2 %>% filter(m_whether == "Yes")

exp1 <- med3 %>% 
  group_by(m_when, age) %>% 
  summarise(count = n()) 

e1 <- ggplot(exp1, aes(m_when, count, fill = age)) + geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(y=count+0.3,label = count), position = position_dodge(0.9))+
  scale_fill_manual(values = c("#fcb103","blue","green","red"))+
  labs(x="Time(in months/year)",title="Experience in Meditation(Month/Year-wise)")+
  my_theme
print(e1+labs(fill="Age"))

exp2 <- med3 %>% 
  group_by(m_often, age) %>% 
  summarise(count = n()) 

e2 <- ggplot(exp2, aes(m_often, count, fill = age)) + geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(y=count+0.3,label = count), position = position_dodge(0.9))+
  scale_fill_manual(values = c("#fcb103","blue","green","red"))+
  labs(x="Time(in weeks)",title = "Frequency in a Week")+my_theme
print(e2+labs(fill="Age"))

exp3 <- med3 %>% 
  group_by(m_duration, age) %>% 
  summarise(count = n()) 

e3 <- ggplot(exp3, aes(m_duration, count, fill = age)) + geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(y=count+0.5,label = count), position = position_dodge(0.9))+
  scale_fill_manual(values = c("#fcb103","blue","green","red"))+
  labs(x="Time(per day)",title="Daily Frequency")+my_theme
print(e3+labs(fill="Age"))


#ggarrange(exp1,exp2,exp3,nrow=1,common.legend = T)

####Visualization_ For understanding since when people have been meditating! - 

test2a <- xtabs(~m_when+stress,med2)
test2a
View(test2a)

test2a <- as.data.frame(test2a)
class(test2a)
ggplot(data=test2a, mapping = aes(x=m_when, y=Freq, fill=stress)) +
  geom_bar(stat = "identity")



####Visualization_ For understanding how often do they meditate! 
#test2 <- xtabs(~m_often+stress,med2)
# test2
# View(test2)
# 
# test2 <- as.data.frame(test2)
# class(test2)
# ggplot(data=test2, mapping = aes(x=m_often, y=Freq, fill=stress)) + 
#   geom_bar(stat = "identity")

med2 %>% filter(m_whether == "No") %>% group_by(stress) %>% summarise(n())

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
  geom_bar(stat = 'identity', fill = "steelblue3") + coord_polar()+
  labs(x="Factors",y="Frequency",title = "Distribution of Impact On Non-Practitioners  ")

#med2 %>% 
# filter(recommend != '') %>% 
#  group_by(m_whether, recommend) %>% 
# summarise(count = n()) %>% 
#ggplot(aes(recommend, count, fill = m_whether)) + 
#geom_bar(stat = 'identity', position = 'dodge') + 
#my_theme


### Comparing worst 2 factors - stress and sleep for people who meditate and who don't 


## 10

## Stress 
st1 <- med2 %>% group_by(m_whether, stress) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=m_whether, y = count, fill = stress)) +
  geom_bar(stat='identity', position = 'dodge')+
  geom_text(aes(y=count+0.5,label = count), position = position_dodge(0.9))+
  labs(x="Do You Meditate",title="            Meditators VS Non-Meditators\n                     On Basis of Stress")+
  coord_polar();st1


## Sleep  

s1 <- med2 %>% group_by(m_whether, sleep) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=m_whether, y = count, fill = sleep)) +
  geom_bar(stat='identity', position = 'dodge')+
  geom_text(aes(y=count+0.5,label = count), position = position_dodge(0.9))+
  labs(x="Do You Meditate",title="            Meditators VS Non-Meditators\n                     On Basis of Sleep")+
  coord_polar();s1


##

h1 <- med2 %>% group_by(m_whether, health) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=m_whether, y = count, fill = health)) +
  geom_bar(stat='identity', position = 'dodge')+
  geom_text(aes(y=count+0.5,label = count), position = position_dodge(0.9))+
  labs(x="Do You Meditate",title="            Meditators VS Non-Meditators\n                     On Basis of Health")+
  coord_polar();h1





## 11 
#Reason for not being able to build up the habit of Mediation 

m_reason <- c("Lack of spare time","Lack of concentration","Don't believe in meditation",
              "Lack of consistency","Also the lack of awareness,\n guidance, & knowledge",
              "I have never thought\n about doing meditation")
freq <- rep(0,6)
freq[1] <- length(grep("Lack of spare time", med2$m_reason))
freq[2] <- length(grep("Lack of concentration", med2$m_reason))
freq[3] <- length(grep("Don't believe in meditation", med2$m_reason))
freq[4] <- length(grep("Lack of consistency", med2$m_reason))
freq[5] <- length(grep("Also the lack of awareness, guidance, & knowledge", med2$m_reason))
freq[6] <- length(grep("I have never thought about doing meditation", med2$m_reason))
df <- data.frame(m_reason, freq)
ggplot(df, aes(m_reason, freq))+geom_bar(stat = 'identity', fill = "tomato2")+
  coord_flip() + labs(x="Reasons",y="Frequency",title="Reasons for not meditating")+
  my_theme

## 12
#Recommendation of People to do meditation 

r1 <- med2 %>% 
  filter(recommend != '') %>% 
  group_by(m_whether, recommend) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(recommend, count, fill = m_whether)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  annotate("text",x=c(0.8,1.25,1.75,2.25,2.75,3.25),y=c(9,1,28,7,16,39)+1,
           label=c(9,1,28,7,16,39))+labs(x="Recommend",title="Distribution of recommendation for meditation")+
  my_theme
print(r1+labs(fill="Do you Meditate"))







