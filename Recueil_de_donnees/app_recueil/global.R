#setwd("C:/Users/james/OneDrive/Desktop/DocParisDescartes/S3/Recueil_D_Donn?es/got-master/data")


load("james1.RData")
load("james2.RData")




library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(sf)


characters = read_csv("characters.csv")
episodes = read_csv("episodes.csv")
scenes = read_csv("scenes.csv")
appearances = read_csv("appearances.csv")



locations=st_read("Locations.shp",crs=4326)
lakes=st_read("Lakes.shp",crs=4326)
conts=st_read("Continents.shp",crs=4326)
land=st_read("Land.shp",crs=4326)
wall=st_read("Wall.shp",crs=4326)
islands=st_read("Islands.shp",crs=4326)
kingdoms=st_read("Political.shp",crs=4326)
landscapes=st_read("Landscape.shp",crs=4326)
roads=st_read("Roads.shp",crs=4326)
rivers=st_read("Rivers.shp",crs=4326)



p1 <- {
  labels = scenes %>% filter(duration>400)
  ggplot(scenes %>% left_join(episodes))+
    geom_boxplot(aes(x=factor(episodeId),y=duration,fill=factor(seasonNum)))+
    geom_text(data=labels ,aes(x=factor(episodeId),y=duration,label=subLocation),hjust = "right",vjust="top")+
    scale_x_discrete("N° épisode",as.character(seq(1,73, by=5)))+
    scale_fill_brewer(palette="Spectral",guide="none")+
    ylab("Durée des scènes (min)")+
    ggtitle("Répartition des durées des scènes par épisodes")+
    theme_bw()
}



p2 <- {screenTimePerSeasons = appearances %>% left_join(scenes) %>% 
  left_join(episodes) %>% 
  group_by(name,seasonNum) %>% 
  summarise(screenTime=sum(duration)) %>% 
  arrange(desc(screenTime)) 
screenTimeTotal = screenTimePerSeasons %>% 
  group_by(name) %>% 
  summarise(screenTimeTotal=sum(screenTime))
mainCharacters = screenTimeTotal %>% 
  filter(screenTimeTotal>60*60) %>% 
  arrange(screenTimeTotal) %>% 
  mutate(nameF=factor(name,levels = name))
data = screenTimePerSeasons %>% left_join(mainCharacters) %>% filter(!is.na(nameF))
ggplot(data)+
  geom_bar(aes(y=nameF,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
  scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
  geom_text(data=mainCharacters,aes(y=nameF,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
  scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
  ylab("")+ggtitle("Temps d'apparition cumulé par personnage et saison")}


p3={scenes_stats=scenes %>% left_join(episodes) %>% 
  group_by(episodeTitle,seasonNum) %>% 
  summarize(nb_scenes=n(),duration_max=max(duration),nbdeath=sum(nbdeath))


labels = scenes_stats %>% filter(duration_max>400|nb_scenes>200)
ggplot(scenes_stats,aes(x=nb_scenes,y=duration_max,col=factor(seasonNum)))+
  
  geom_point(aes(size=nbdeath))+
  geom_text(data=labels,aes(label=episodeTitle),vjust=-0.6)+
  scale_x_continuous("Nombre de scène",limits = c(0,280))+
  scale_y_continuous("Durée de la scène la plus longue",limits = c(100,800))+
  scale_color_brewer("Saison",palette ="Spectral")+
  guides(colour = "legend", size = "legend")+
  theme_bw()}




#Quels sont les deux personnages qui passent le plus de temps ensembles ?
p4={
  
  appearances %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% 
    filter(name.x!=name.y) %>% 
    left_join(scenes %>% select(sceneId,duration)) %>%
    group_by(name.x,name.y) %>% 
    summarise(commonTime=sum(duration)) %>% 
    arrange(desc(commonTime)) 
  
}


#Quels sont les deux personnages qui passent le plus de scènes ensembles ?
p5={
  
  
  appearances %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% 
    filter(name.x!=name.y) %>% 
    group_by(name.x,name.y) %>% 
    summarise(nbs=n()) %>% 
    arrange(desc(nbs))
}

#Trouvez le lieux précis ou le plus de personanges meurent ?
p6={
  
  scenes %>% group_by(subLocation) %>% 
    summarise(nbd=sum(nbdeath)) %>% 
    arrange(desc(nbd))
}

# Temps de pr?sence par ?pisode de John Snow
p7 <- {
  jstime = appearances %>% filter(name=="Jon Snow") %>% 
    left_join(scenes) %>% 
    group_by(episodeId) %>% 
    summarise(time=sum(duration))
  ggplot(jstime) + 
    geom_line(aes(x=episodeId,y=time))+
    theme_bw()+
    xlab("?pisode")+ylab("temps")+
    ggtitle("Temps de présence par épisode de John Snow")
  
}


p8={
  colforest="#c0d7c2"
  colriver="#7ec9dc"
  colriver="#87cdde"
  colland="ivory"
  borderland = "ivory3"  
  ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
    geom_sf(data=islands,fill=colland,col="ivory3")+
    geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
    geom_sf(data=rivers,col=colriver)+
    geom_sf(data=lakes,col=colriver,fill=colriver)+
    geom_sf(data=wall,col="black",size=1)+
    geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
    theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
    theme(panel.background = element_rect(fill = colriver,color=NA)) +
    labs(title = "GoT",caption = "Etiennne Côme, 2020",x="",y="")
}



p9={
  main_char= c("Jon Snow", "Tyrion Lannister","Daenerys Targaryen","Sansa Stark","Cersei Lannister","Arya Stark")
  landpol = st_union(st_geometry(land)) 
  islandpol = st_union(st_geometry(islands))
  backpol=st_union(landpol,islandpol)
  background = st_as_sf(data.frame(name=main_char,geometry=rep(backpol,6)))
  loc_time=appearances %>% filter(name %in% main_char) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
  loc_time_mc = scenes_locations %>% left_join(loc_time)
  
  
  
  ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
    geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
    geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
    coord_sf(expand = 0,ndiscr = 0)+
    scale_color_discrete(guide="none")+
    scale_size_area("Durée (min) :",max_size = 12,breaks=c(30,60,120,240))+
    facet_wrap(~name)+
    theme(panel.background = element_rect(fill = colriver,color=NA),
          text = element_text(family="Palatino",face = "bold",size = 14),
          legend.key = element_rect(fill="#ffffff"),
    ) +
    labs(title = "Temps de présence des personnages principaux",caption = "@comeetie, 2020",x="",y="")
  
}






