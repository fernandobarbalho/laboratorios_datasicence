###Datasets do censo para análises de chatgpt


censo_municipios_amazonia<-
  ibge2022 %>%
  rename(cd_mun = municipio_codigo) %>%
  inner_join(Municipios_da_Amazonia_Legal_2022)


censo_municipios_amazonia %>%
  readr::write_csv("censo_municipios_amazonia.csv")


censo_municipios_amazonia %>%
  summarise(
    populacao_maxima = max(populacao_residente),
    media_populacao = mean(populacao_residente),
    mediana_populacao = median(populacao_residente),
    desvio_padrao_populacao = sd(populacao_residente),
    area_maxima = max(area_total),
    media_area = mean(area_total),
    mediana_area = median(area_total),
    desvio_padrao_area = sd(area_total),
    densidade_maxima = max(densidade_demografica),
    media_densidade = mean(densidade_demografica),
    mediana_densidade = median(densidade_demografica),
    desvio_padrao_densidadea = sd(densidade_demografica),
    .by = uf)%>%
  readr::write_csv("sumario_municipios_amazonia.csv")


###Datasets enriquecidos com dados do capag

url<- "https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/86636c19-b38a-4b9e-8fff-30fc4208dd04/download/CAPAG-Municipios---Ano-Base-2022.xlsx"

download.file(url, destfile = "dados_capag_2022.xlsx", mode = "wb")

library(readxl)
dados_capag_2022 <- read_excel("dados_capag_2022.xlsx")

dados_capag_2022 <- janitor::clean_names(dados_capag_2022)

capag_amazonia<-
censo_municipios_amazonia %>%
  left_join(
    dados_capag_2022 %>%
      mutate(cd_mun = as.character(cod_ibge))
  )

capag_amazonia %>%
  readr::write_csv("capag_amazonia.csv")

capag_amazonia_legal_sf<-  
  mapa_municipios %>%
  inner_join(
    capag_amazonia %>%
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
  geom_sf(data=capag_amazonia_legal_sf, aes(fill=capag_2022), color = "lightblue") +
  geom_sf(data=estados, fill=NA)+
  #geom_sf(data= sf_populacao_milhao, color="black", size=1)+
  # geom_sf_text(data = sf_populacao_milhao,
  #              aes(label= str_wrap(paste0(name_muni,":"," ", round(populacao_residente/10^6,1)),20)), 
  #              color = "black", 
  #              fontface = "bold", 
  #              size = 2.9,
  #              nudge_y = -0.2,
  #              
  #              show.legend = FALSE
  #)+
  geom_text(data = southamerica_names,
            aes(x=lon, y=lat, label= str_wrap(name_long,20)), 
            color = "black", 
            fontface = "bold", 
            size = 2.9
  )+
  
  coord_sf(xlim = c(xmin,xmax), ylim=c(ymin,ymax))+
  scale_fill_discrete_qualitative(palette= "Dark 2")+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("Nota Capag", 30)
  )
