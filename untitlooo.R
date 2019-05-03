remove(list =ls()) #Clean work space

setwd("/Users/Albert/Desktop/bioinformatics/High Throughput anaylses/Homework 1/hw1_final")


#Question 1

library(babynames)
library(tidyverse)

#A
babynames %>%
  filter(sex=="F") %>%   # leaves only female names
  filter(str_detect(name, pattern = "^P")) %>%   # leaves only names starting with P
  group_by(name) %>%   # groups by name
  summarise(total_n = sum(n)) %>%   # counts all names regardless of the year
  arrange(desc(total_n))-> summed_girlnames_P   

top5females<-head(summed_girlnames_P, 5)

#B

namebyyears<-babynames %>% filter(name %in% top5females$name) %>% filter(sex=="F")

ggplot(data=namebyyears, aes(y=n, x=year, col=name))+
  geom_line()+theme_bw()

#Question 2
arwen1990<- babynames %>% filter(name =="Arwen", year=="1990")
arwen2004<- babynames %>% filter(name =="Arwen", year=="2004")
# LORT baby


#Question 3

flowers<-read_tsv("flowers.txt")

petals<-data.frame(width=flowers$Petal.Width, length=flowers$Petal.Length, species=flowers$Species, type=rep("Petal",nrow(flowers)))

sepals<-data.frame(width=flowers$Sepal.Width, length=flowers$Sepal.Length, species=flowers$Species, type=rep("Sepal",nrow(flowers)))
             
newflowers<-rbind(petals,sepals)

ggplot(data=newflowers, aes(y=width, x=length, col=species))+
  geom_point()+ geom_smooth(method="lm", fullrange=TRUE)+facet_wrap(~type, scales="free")+theme_bw()

#Question 4

chip<-read_tsv("chip_mm5.txt")

chip<-mutate(chip, bswidth=end-start)

#We use kruskal wallis 

kruskal.test(score ~ as.factor(chr), data=chip)

kruskal.test(bswidth ~ as.factor(chr), data=chip)

## finish
