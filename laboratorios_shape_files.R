
url<-"https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/amazonia_legal/2022/Municipios_da_Amazonia_Legal_2022.xlsx"

download.file(url, destfile = "Municipios_da_Amazonia_Legal_2022.xlsx", mode = "wb")

library(readxl)
library(geobr)
library(tidyverse)
library(viridis)


Municipios_da_Amazonia_Legal_2022 <- read_excel("Municipios_da_Amazonia_Legal_2022.xlsx")

Municipios_da_Amazonia_Legal_2022 <- janitor::clean_names(Municipios_da_Amazonia_Legal_2022)

mapa_municipios <- readRDS("~/Github/laboratorios_datasicence/mapa_municipios.RDS")

gera_tabela_ibge_municipios<- function(){
  # Load required libraries
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)
  
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
  geom_sf(aes(fill=populacao_residente), color=NA) +
    scale_fill_viridis()
    

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

glimpse(maps::world.cities)

