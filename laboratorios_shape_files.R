
url<-"https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/amazonia_legal/2022/Municipios_da_Amazonia_Legal_2022.xlsx"

download.file(url, destfile = "Municipios_da_Amazonia_Legal_2022.xlsx", mode = "wb")

library(readxl)
library(geobr)
library(tidyverse)
library(viridis)
library(sf)
library(spData)
library(ggrepel)


Municipios_da_Amazonia_Legal_2022 <- read_excel("Municipios_da_Amazonia_Legal_2022.xlsx")

Municipios_da_Amazonia_Legal_2022 <- janitor::clean_names(Municipios_da_Amazonia_Legal_2022)

mapa_municipios <- readRDS("~/Github/laboratorios_datasicence/mapa_municipios.RDS")

mapa_municipios <- geobr::read_municipality(simplified = FALSE)

estados<- geobr::read_state()
sedes_municipios<- geobr::read_municipal_seat()
brasil<- geobr::read_country()

gera_tabela_ibge_municipios<- function(){
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

#Exemplo de uso
ibge2022<-
  gera_tabela_ibge_municipios()


  
sedes_amazonia<-
  sedes_municipios %>%
  inner_join(
    Municipios_da_Amazonia_Legal_2022 %>%
      mutate(code_muni = as.numeric(cd_mun))
  )  
  

sedes_amazonia$lon<- sf::st_coordinates(sedes_amazonia$geom)[,1]
sedes_amazonia$lat<- sf::st_coordinates(sedes_amazonia$geom)[,2]  
  

xmin<- min(sedes_amazonia$lon) -1 
xmax <- max(sedes_amazonia$lon) +1

ymin<- min(sedes_amazonia$lat) -1
ymax <- max(sedes_amazonia$lat) +1


top_10_populacao<-
ibge2022 %>%
  rename(cd_mun = municipio_codigo) %>%
  inner_join(Municipios_da_Amazonia_Legal_2022) %>%
  slice_max(order_by = populacao_residente, n=10)

populacao_milhao<-
  ibge2022 %>%
  rename(cd_mun = municipio_codigo) %>%
  inner_join(Municipios_da_Amazonia_Legal_2022) %>%
  filter(populacao_residente >= 10^6)


sf_top_10_populacao<-
  sedes_amazonia %>%
  inner_join(
    top_10_populacao %>%
      mutate(code_muni = as.numeric(cd_mun))
  )


sf_populacao_milhao<-
  sedes_amazonia %>%
  inner_join(
    populacao_milhao %>%
      mutate(code_muni = as.numeric(cd_mun))
  )


data("world")

southamerica<-
  world %>%
  filter(continent=="South America" |
           iso_a2=="FR" ) 



southamerica$lon<- sf::st_coordinates(sf::st_centroid(southamerica$geom))[,1]   
southamerica$lat<- sf::st_coordinates(sf::st_centroid(southamerica$geom))[,2]




southamerica_names<-
southamerica %>%
  filter(iso_a2 %in% c("BO","PE","CO","VE", "GY", "SR", "FR"))  %>%
  mutate(lon = ifelse(iso_a2== "FR", -52.3354,lon)) %>%
  mutate(lat = case_when(
    iso_a2 == "FR" ~ 4.9380,
    iso_a2 == "VE" ~ 4.9380,
    .default = lat
  )

    
)
  


amazonia_legal_sf<-  
mapa_municipios %>%
  inner_join(
    Municipios_da_Amazonia_Legal_2022 %>%
      mutate(code_muni = as.numeric(cd_mun))
  ) %>%
  inner_join(
    ibge2022 %>%
      mutate(code_muni = as.numeric(municipio_codigo))
  ) 



#mapa com área
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



#Tentativa com viridis
ggplot() +
  geom_sf(data = southamerica, fill="#DCDCDC")+
  geom_sf(data= brasil, fill= "#808080")+
  geom_sf(data=amazonia_legal_sf, aes(fill=populacao_residente/10^6), color = "black") +
  geom_sf(data=estados, fill=NA)+
  #geom_sf(data= sf_populacao_milhao, color="black", size=1)+
  geom_sf_text(data = sf_populacao_milhao,
            aes(x=lon, y=lat, 
                label= str_wrap(paste0(name_muni,":"," ", round(populacao_residente/10^6,1)),20)), 
            color = "white", 
            fontface = "bold", 
            size = 2.9,
            show.legend = FALSE
  )+
  geom_text(data = southamerica_names,
            aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
            color = "black", 
            fontface = "bold", 
            size = 2.9
  )+
  
  coord_sf(xlim = c(xmin,xmax), ylim=c(ymin,ymax))+
  scale_fill_viridis() +
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 30)
  )


##Tentativa de Gráfico com pontos (rejeitada)

amazonia_legal_seats<-  
  sedes_municipios %>%
  inner_join(
    Municipios_da_Amazonia_Legal_2022 %>%
      mutate(code_muni = as.numeric(cd_mun))
  ) %>%
  inner_join(
    ibge2022 %>%
      mutate(code_muni = as.numeric(municipio_codigo))
  ) 


ggplot() +
  geom_sf(data = southamerica, fill="#DCDCDC")+
  geom_sf(data= brasil, fill= "#808080")+
  geom_sf(data=estados, fill="black")+
  geom_sf(data=amazonia_legal_seats, 
          aes(fill=populacao_residente/10^6),
          pch=21, 
          color = "#444444",
          size=1) +
  geom_sf(data=sf_populacao_milhao, 
          aes(fill=populacao_residente/10^6),
          pch=21, 
          color = "#444444",
          size=2) +
  geom_sf_text(data = sf_populacao_milhao,
               aes(label= str_wrap(paste0(name_muni,":"," ", round(populacao_residente/10^6,1)),20)), 
               color = "white", 
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



    

mapa_municipios %>%
  inner_join(
    Municipios_da_Amazonia_Legal_2022 %>%
      mutate(code_muni = as.numeric(cd_mun))
  ) %>%
  inner_join(
    ibge2022 %>%
      mutate(code_muni = as.numeric(municipio_codigo))
  ) %>%
  ggplot() +
  geom_sf(aes(fill=densidade_demografica), color=NA) +
  scale_fill_viridis()




