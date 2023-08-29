library(readxl)
library(geobr)
library(tidyverse)
library(sf)
library(spData)
library(ggrepel)


get_municipalies_data<- function(){
  # Load required libraries
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)
  library(colorspace)
  
  # API endpoint URL
  api_url <- "https://apisidra.ibge.gov.br/values/t/4714/n6/all/v/all/p/all/d/v614%202"
  
  data_list <- fromJSON(api_url)
  
  
  names_df<- data_list[1,]
  data_list <- data_list[-1,]
  
  names(data_list) <- names_df
  
  data_list <- janitor::clean_names(data_list)
  
  ibge_municipios<-
    data_list %>%
    mutate(valor =as.numeric(valor)) %>%
    select(municipio_codigo,
           municipio,
           variavel,
           valor,
           ano) %>%
    pivot_wider(id_cols = c(municipio_codigo, municipio), names_from = variavel, values_from = valor) %>%
    separate(municipio, into = c("municipio", "uf"), sep = " - ")
  
  
  ibge_municipios<- janitor::clean_names(ibge_municipios)
  
  ibge_municipios
  
  
}



data("world")


#mapa mundi


world %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 10)
  )


world %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat 2", trans= "log2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be"),
    legend.position = "none"
  ) 



#Mapa américa do sul

world %>%
  filter(continent == "South America") %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 10)
  )



world %>%
  filter(continent == "South America") %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat 2", trans= "log2" )+
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
  geom_sf(aes(fill=pop/10^6)) +
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  theme_light() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 10)
  )+
  geom_text_repel(aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.8)





#Mapa da França
france<-
  world %>%
  filter(iso_a2 == "FR")
  


france %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat 2", trans= "log2" )+
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
  geom_sf(aes(fill=pop/10^6)) +
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  theme_light() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 10)
  )+
  geom_text_repel(aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.8)



#Mapa recortado da América do Sul incluindo a França



southamerica %>%
  bind_rows(france) %>%
  mutate(lon= ifelse(iso_a2=="FR", france[[11]][[1]][[1]][[1]][1,1], lon),
         lat= ifelse(iso_a2=="FR",france[[11]][[1]][[1]][[1]][1,2], lat)) %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat 2", trans= "log2" )+
  geom_text_repel(aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.8)+
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
  insee::get_insee_idbank() %>%
  filter(TIME_PERIOD == "2023") %>% 
  select(TITLE_EN,OBS_VALUE) %>%
  mutate(iso_a2 = "FR")

data_guiana <- janitor::clean_names(data_guiana)

southamerica %>%
  bind_rows(france) %>%
  left_join(data_guiana) %>%
  mutate(pop=ifelse(iso_a2=="FR",obs_value,pop))%>%
  mutate(lon= ifelse(iso_a2=="FR", france[[11]][[1]][[1]][[1]][1,1], lon),
         lat= ifelse(iso_a2=="FR",france[[11]][[1]][[1]][[1]][1,2], lat)) %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  geom_text_repel(aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.8)+
  coord_sf(xlim = c(-82,-35), ylim=c(-60,15))+
  theme_light() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 10)
  )


#incluindo a América Central

central_america<-
  world %>%
  filter(subregion == "Central America")


southamerica %>%
  bind_rows(france) %>%
  left_join(data_guiana) %>%
  mutate(pop=ifelse(iso_a2=="FR",obs_value,pop))%>%
  mutate(lon= ifelse(iso_a2=="FR", france[[11]][[1]][[1]][[1]][1,1], lon),
         lat= ifelse(iso_a2=="FR",france[[11]][[1]][[1]][[1]][1,2], lat)) %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  geom_sf(data= central_america,fill= "#808080")+
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  geom_text_repel(aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.8)+
  coord_sf(xlim = c(-82,-35), ylim=c(-60,15))+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões", 10)
  )


##Mapa com população dos estados do Brasil
central_america<-
  world %>%
  filter(subregion == "Central America")



brasil<- geobr::read_country()
estados<- geobr::read_state()

#dados de população

ibge2022<-
  get_municipalies_data()

estados<-
  estados %>%
  inner_join(
    ibge2022 %>%
      rename(abbrev_state = uf) %>%
      summarise(.by=abbrev_state,
                pop = sum(populacao_residente)
      )
  )

southamerica %>%
  filter(iso_a2!="BR") %>%
  bind_rows(france) %>%
  left_join(data_guiana) %>%
  mutate(pop=ifelse(iso_a2=="FR",obs_value,pop))%>%
  mutate(lon= ifelse(iso_a2=="FR", france[[11]][[1]][[1]][[1]][1,1], lon),
         lat= ifelse(iso_a2=="FR",france[[11]][[1]][[1]][[1]][1,2], lat)) %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  geom_sf(data=estados, aes(fill=pop/10^6)) +
  geom_sf(data=brasil,fill=NA, color="#00A859", lwd=1.2)+
  geom_sf(data= central_america,fill= "#808080")+
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  geom_text_repel(aes(x=lon, y=lat, 
                      label= str_wrap(name_long,20)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.8)+
  coord_sf(xlim = c(-82,-35), ylim=c(-60,15))+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões", 10)
  )


##Mapa com população destaque para territórios de maiores populações


estados$lon<- sf::st_coordinates(sf::st_centroid(estados$geom))[,1]   
estados$lat<- sf::st_coordinates(sf::st_centroid(estados$geom))[,2]


most_populated<-
  southamerica %>%
  filter(iso_a2 !="BR") %>%
  rename(name= name_long) %>%
  as_tibble() %>%
  select(name, pop, lat, lon) %>%

  bind_rows(
    estados %>%
      rename(name= name_state) %>%
      as_tibble() %>%
      select(name, pop, lat, lon)
  ) %>%
  slice_max(order_by = pop, n=8)
    
  

southamerica %>%
  filter(iso_a2!="BR") %>%
  bind_rows(france) %>%
  left_join(data_guiana) %>%
  mutate(pop=ifelse(iso_a2=="FR",obs_value,pop))%>%
  mutate(lon= ifelse(iso_a2=="FR", france[[11]][[1]][[1]][[1]][1,1], lon),
         lat= ifelse(iso_a2=="FR",france[[11]][[1]][[1]][[1]][1,2], lat)) %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  geom_sf(data=estados, aes(fill=pop/10^6)) +
  geom_sf(data=brasil,fill=NA, color="#00A859", lwd=1.2)+
  geom_sf(data= central_america,fill= "#808080")+
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  geom_text_repel(data= most_populated,
                  aes(x=lon, y=lat, 
                      label= str_c(str_wrap(name,10),": ",round(pop/10^6,1))), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.9)+
  coord_sf(xlim = c(-82,-35), ylim=c(-60,15))+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões", 10)
  )



