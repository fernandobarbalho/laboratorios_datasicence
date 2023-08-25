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

southamerica %>%
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
  geom_text_repel(aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.9)





#Mapa da França
france<-
  world %>%
  filter(iso_a2 == "FR")
  


france %>%
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

southamerica %>%
  bind_rows(france) %>%
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
  geom_text_repel(aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.9)



#Mapa recortado da América do Sul incluindo a França



southamerica %>%
  bind_rows(france) %>%
  mutate(lon= ifelse(iso_a2=="FR", france[[11]][[1]][[1]][[1]][1,1], lon),
         lat= ifelse(iso_a2=="FR",france[[11]][[1]][[1]][[1]][1,2], lat)) %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat", trans= "log2" )+
  geom_text_repel(aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.9)+
  coord_sf(xlim = c(-82,-35), ylim=c(-60,15))+
  theme_light() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População", 30)
  )




#Acrescenta os dados de população da Guiana Francesa

data_guiana<-
  insee::get_idbank_list('TCRED-ESTIMATIONS-POPULATION') %>%
  filter(str_detect(REF_AREA_label_fr,"Guyane")) %>%
  filter(AGE == "00-") %>% #all ages
  filter(SEXE == 0) %>% #men and women
  pull(idbank) %>%
  insee::get_insee_idbank()


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
