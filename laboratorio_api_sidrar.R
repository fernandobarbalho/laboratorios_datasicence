library(jsonlite)

library(sidrar)



sidra_ipca<-
  search_sidra("IPCA")

lista<- info_sidra(1737)


url<- "https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/63,2266/p/all/d/v63%202,v2266%2013"

mes_ano_ref<- 
  
  ipca <- get_sidra(1737,
                    variable = 2266,
                    period = c("199201-202306"))

fab<- get_sidra(1737,
                variable = 2266)

fab<-
  sidrar::get_sidra(api = "/t/4714/n6/all/v/all/p/all/d/v614%202")


fab <- sidrar::get_sidra(api = "/t/4714/n6/all/v/all/p/all/d/v614%202") |> 
  janitor::clean_names() |> 
  dplyr::select(municipio_codigo, municipio, variavel, valor)

fab <- sidrar::get_sidra(api = "/t/4714/n6/all/v/all/p/all/d/v614%202") |> 
  janitor::clean_names() |> 
  dplyr::select(municipio_codigo, municipio, variavel, valor)

d <- sidrar::get_sidra(api = "/t/1737/n1/all/v/63,2266/p/all/d/v63%202,v2266%2013")



ipca_historico<-
  jsonlite::fromJSON(url)



names(ipca_historico)<- ipca_historico[1,]

ipca_historico<- ipca_historico[-1,]

ipca_historico<-
  ipca_historico %>%
  clean_names() %>%
  filter(variavel=="IPCA - Número-índice (base: dezembro de 1993 = 100)") %>%
  mutate(data_referencia = paste(str_sub(mes_codigo,1,4), str_sub(mes_codigo,5,6), "01", sep = "-" ),
         valor = as.numeric(valor)) %>%
  select(data_referencia, valor)

unique(ipca_historico$Variável)
