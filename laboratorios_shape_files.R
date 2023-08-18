
url<-"https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/amazonia_legal/2022/Municipios_da_Amazonia_Legal_2022.xlsx"

download.file(url, destfile = "Municipios_da_Amazonia_Legal_2022.xlsx", mode = "wb")

library(readxl)
library(geobr)
library(tidyverse)

Municipios_da_Amazonia_Legal_2022 <- read_excel("Municipios_da_Amazonia_Legal_2022.xlsx")

Municipios_da_Amazonia_Legal_2022 <- janitor::clean_names(Municipios_da_Amazonia_Legal_2022)

mapa_municipios <- readRDS("~/Github/laboratorios_datasicence/mapa_municipios.RDS")

mapa_municipios %>%
  inner_join(
    Municipios_da_Amazonia_Legal_2022 %>%
      mutate(code_muni = as.numeric(cd_mun))
  ) %>%
  ggplot() +
  geom_sf(aes(fill=sigla_uf))
