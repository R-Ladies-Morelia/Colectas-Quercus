####Maribel Arenas Navarro
####Colectas de Quercus de GBIF
####R ladies Morelia

setwd("~/R_ladies")
#library(rgbif)   ### para descargar datos

base <- read.csv("Colectores_Quer_BD2.csv") #base de lo que vas a descargar ejemplo Quercus
head(base)
dim(base)
str(base)

library(tidyverse)
library(ggplot2)


base$Collectors.gender<-as.factor(base$Collectors.gender)

base$a?o<-as.factor(base$year)   #133 a?os 1830 a 2020

summary(base$a?o)
table(base$a?o)

base$mes<-as.factor(base$month) 

#Por g?nero
colectoras <- subset(x=base, base$Collectors.gender == "Female")
colectores<- subset(x=base, base$Collectors.gender == "Male")

colectoras$recordedBy<-as.factor(colectoras$recordedBy) 

#graficas sencillas

#Todos los colectores
barplot(table(base$Collectors.gender),
        horiz = 'F',
        col = 'aquamarine1',
        main = 'Colectas de Quercus',
        xlab = 'G?nero',
        ylab = 'Colectas')

ggplot(data = base,
       mapping = aes(x = factor(Collectors.gender))) +
  geom_bar() +
  coord_flip()

ggplot(data = base,
       mapping = aes(x = factor(a?o),  fill=Collectors.gender)) +
  geom_bar() +
  coord_flip()

ggplot(data = base) + 
  geom_point(mapping = aes(x = a?o, y = elevation, color = Collectors.gender))

ggplot(data = base) + 
  geom_point(mapping = aes(x = a?o, y = elevation, color = Collectors.gender, shape = Collectors.gender))


#colectas generales

barplot(table(base$year),
        horiz = 'F',
        col = 'chocolate1',
        main = 'Colectas de Quercus',
        xlab = 'A?o',
        ylab = 'Colectas')

barplot(table(colectoras$year),
        horiz = 'F',
        col = 'aquamarine1',
        main = 'Colectas de Quercus',
        xlab = 'A?o',
        ylab = 'Colectas')

barplot(table(colectores$year),
        horiz = 'F',
        col = 'cornflowerblue',
        main = 'Colectas de Quercus',
        xlab = 'A?o',
        ylab = 'Colectas')

######  Descriptivos general

ggplot(aes(individualCount), data=subset(base, !is.na(Collectors.gender))) +
  geom_histogram(binwidth=30) +
  facet_wrap(~Collectors.gender)

ggplot(aes(x=Collectors.gender, y=year), data=subset(base, !is.na(Collectors.gender))) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=4)



###por especie de encino

#BD general
ggplot(aes(year,species), data=base) +
  geom_jitter(alpha=.25, shape=21, fill=I("#F79420")) +
  ggtitle("colectas especie y tiempo") 

#BD colectoras
ggplot(aes(year,species), data=colectoras) +
  geom_jitter(alpha=.25, shape=21, fill=I("#F79420")) +
  ggtitle("colectas de mujeres por especie y tiempo") 

#Explorando en grafica polar

bar <- ggplot(data = base) +
  geom_bar(
    mapping = aes(x = a?o, fill = Collectors.gender),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_polar()


bar1 <- ggplot(data = colectoras) +
  geom_bar(
    mapping = aes(x = factor(year), fill = species),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar1 + coord_polar()


bar2<- ggplot(data = colectores) +
  geom_bar(
    mapping = aes(x = factor(year), fill = species),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar2 + coord_polar()


#Separamos una nueva base de datos
suppressMessages(library(reshape2))

year_gender <- dcast(base,year ~ Collectors.gender,
                                  value.var="individualCount")
head(year_gender)
summary(year_gender)
dim(year_gender)

#Plot the ratio of the female to male median

ggplot(aes(year, Female/Male), data=year_gender) +
  geom_line() +
  geom_hline(yintercept=1, linetype=2, alpha=0.3)

ggplot(aes(year, Male/Female), data=year_gender) +
  geom_line() +
  geom_hline(yintercept=1, linetype=2, alpha=0.3)



