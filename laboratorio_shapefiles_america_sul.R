library(readxl)
library(geobr)
library(tidyverse)
library(sf)
library(spData)
library(ggrepel)


data("world")


#mapa mundi

#Terra, Caetano

world %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  scale_fill_continuous_sequential(palette= "Heat" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 30)
  )


world %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat", trans= "log2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População", 30)
  )



#Mapa américa do sul
#Soy loco por ti América
world %>%
  filter(continent == "South America") %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  scale_fill_continuous_sequential(palette= "Heat" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 30)
  )



world %>%
  filter(continent == "South America") %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat", trans= "log2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População", 30)
  )


#Mapa da América do Sul com os nomes dos países e coordenadas

southamerica<-
  world %>%
  filter(continent=="South America") 



southamerica$lon<- sf::st_coordinates(sf::st_centroid(southamerica$geom))[,1]   
southamerica$lat<- sf::st_coordinates(sf::st_centroid(southamerica$geom))[,2]

world %>%
  filter(continent == "South America") %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat", trans= "log2" )+
  theme_light() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População", 30)
  )+
  geom_text_repel(data = southamerica,
            aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
            color = "black", 
            fontface = "bold", 
            size = 2.9)



sf::st_centroid(franca[[11]][[1]])

#Mapa da França
franca<-
  world %>%
  filter(iso_a2 == "FR")
  

brasil_spdata<-
  world %>%
  filter(iso_a2 == "BR")


world %>%
  filter(iso_a2 == "FR") %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat", trans= "log2" )+
  theme_light() +
  theme(
    #panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População", 30)
  )



#Mapa da América do Sul incluindo a França


fab<-
insee::get_dataset_list()

fab2<-
  insee::get_insee_idbank("TCRED-ESTIMATIONS-POPULATION")


idbank_list <- insee::get_idbank_list('TCRED-ESTIMATIONS-POPULATION')

data_guiana<-
  insee::get_insee_idbank("001760178")




ggplot() +
  geom_sf(data = southamerica, fill="#DCDCDC")+
  geom_sf(data= brasil, fill= "#808080")+
  geom_sf(data=amazonia_legal_sf, aes(fill=populacao_residente/10^6), color = "white") +
  geom_sf(data=estados, fill=NA)+
  #geom_sf(data= sf_populacao_milhao, color="black", size=1)+
  geom_sf_text(data = sf_populacao_milhao,
               aes(label= str_wrap(paste0(name_muni,":"," ", round(populacao_residente/10^6,1)),20)), 
               color = "black", 
               fontface = "bold", 
               size = 2.9,
               nudge_y = -0.2,
               
               show.legend = FALSE
  )+
  geom_text(data = southamerica_names,
            aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
            color = "black", 
            fontface = "bold", 
            size = 2.9
  )+
  
  coord_sf(xlim = c(xmin,xmax), ylim=c(ymin,ymax))+
  scale_fill_continuous_sequential(palette= "Heat" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 30)
  )



  

southamerica<-
  world %>%
  filter(continent=="South America" |
           iso_a2=="FR" ) 
