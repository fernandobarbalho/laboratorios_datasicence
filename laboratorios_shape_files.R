
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


sf_top_10_populacao<-
  sedes_amazonia %>%
  inner_join(
    top_10_populacao %>%
      mutate(code_muni = as.numeric(cd_mun))
  )


data("world")

southamerica<-
  world %>%
  filter(continent=="South America" )
  
  
  ggplot() +
  geom_sf()


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


ggplot() +
  geom_sf(data = southamerica, fill="#DCDCDC")+
  geom_sf(data= brasil, fill= "#808080")+
  geom_sf(data=amazonia_legal_sf, aes(fill=populacao_residente/1000), color = "lightgray") +
  geom_sf(data=estados, fill=NA)+
  geom_sf(data= sf_top_10_populacao, color="black", size=1)+
  geom_text_repel(data = sf_top_10_populacao,
                  aes(x=lon, y=lat, label= str_wrap(paste0(name_muni,":"," ", round(populacao_residente/1000,0)),20)), 
                  color = "black", 
                  #fontface = "bold", 
                  size = 2.9
  )+
  coord_sf(xlim = c(xmin,xmax), ylim=c(ymin,ymax))+
  scale_fill_continuous_sequential(palette= "Heat" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= "População em milhares"
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




