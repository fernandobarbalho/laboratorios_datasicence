library(jsonlite)

# load the httr package
library(httr)


my_key<- "<entre_a_chave"  

# Define the API endpoint and headers
url <- "https://dados.gov.br/dados/api/publico/conjuntos-dados"
headers <- c(
  "accept" = "application/json",
  "chave-api-dados-abertos" = my_key
)




# Define the query parameters to retrieve data about "Impostos"
params <- list(
  isPrivado = "false",
  nomeConjuntoDados = "Imposto",
  pagina = "1"
)

# Make the GET request
response <- GET(url, add_headers(headers), query=params)

# Print the response content
conteudo<- content(response, "text")


df_impostos<- jsonlite::fromJSON(conteudo)


#Url para dados sobre grandes números de impostos
url_grandes_numeros <- str_c(url,"/", df_impostos$id[2] )



# Make the GET request
response <- GET(url_grandes_numeros, add_headers(headers))


conteudo_grandes_numeros<- content(response, "text")


list_impostos_grandes_numeros<- jsonlite::fromJSON(conteudo_grandes_numeros)


#download data

recursos<- list_impostos_grandes_numeros[["recursos"]]

#link de bens e direitos

link_arquivo<- recursos$link[2]

download.file(link_arquivo, destfile = "bens_direitos.csv", mode = "wb")

library(tidyverse)
bens_direitos <- read_delim("bens_direitos.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)



bens_direitos_tidy<-
  bens_direitos %>%
  pivot_longer(cols = -1, names_to = "bens_direitos", values_to = "valor") %>%
  rename(ano = `Ano Calendário`)



bens_direitos_tidy %>%
  filter(ano == 2020) %>%
  slice_max(order_by = valor, n=10) %>%
  mutate(bens_direitos = reorder(str_wrap(bens_direitos,20), valor)) %>%
  ggplot()+
  geom_col(aes(x= valor/10^9, y=bens_direitos)) +
  theme_light() +
  labs(x= "Valor em R$ bi",
       y="",
       title= "Grandes números IRPF: total bens e consumo para 2020")
