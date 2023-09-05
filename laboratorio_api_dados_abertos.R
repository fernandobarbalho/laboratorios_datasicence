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


ano_foco<- 2008

bens_direitos_tidy %>%
  mutate(destaque = ifelse( str_detect(bens_direitos,"criptoativos"),1,0)) %>%
  filter(ano == ano_foco) %>%
  slice_max(order_by = valor, n=10) %>%
  mutate(bens_direitos = reorder(str_wrap(bens_direitos,20), valor)) %>%
  ggplot()+
  geom_col(aes(x= valor/10^9, y=bens_direitos), show.legend = FALSE) +
  theme_light() +
  labs(x= "Valor em R$ bi",
       y="",
       title= paste("Grandes números IRPF: total bens e consumo"))+
  facet_wrap(ano~.)


##Animação


animation <- bens_direitos_tidy %>%
  mutate(destaque = ifelse(str_detect(bens_direitos, "criptoativos"), 1, 0)) %>%
  slice_max(order_by = valor, n = 10) %>%
  mutate(bens_direitos = reorder(str_wrap(bens_direitos, 20), valor)) %>%
  ggplot(aes(x = valor/10^9, y = bens_direitos, fill = destaque)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  labs(x = "Valor em R$ bi",
       y = "",
       title = paste("Grandes números IRPF: total bens e consumo")) +
  transition_states(ano, transition_length = 2, state_length = 1) +  # This line creates the animation
  enter_fade() +  # Controls how new bars enter the scene
  exit_fade()    # Controls how old bars exit the scene

anim_save("bar_race_animation.gif", animation)  # Saves the animation as a GIF



animation <- bens_direitos_tidy %>%
  mutate(destaque = ifelse(str_detect(bens_direitos, "criptoativos"), 1, 0)) %>%
  group_by(ano) %>%
  top_n(10, valor) %>%   # Filtering top 10 entries based on valor for each year
  ungroup() %>%
  mutate(bens_direitos = reorder(str_wrap(bens_direitos, 20), valor)) %>%
  ggplot(aes(x = valor/10^9, y = bens_direitos, fill = destaque)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  labs(x = "Valor em R$ bi",
       y = "",
       title = "Grandes números IRPF: total bens e consumo") +
  transition_states(ano, transition_length = 2, state_length = 1) +
  enter_fade() + 
  exit_fade()

anim_save("bar_race_animation.gif", animation)  # Saves the animation as a GIF



bens_direitos_tidy %>%
  mutate(destaque = ifelse( str_detect(bens_direitos,"criptoativos"),1,0)) %>%
  slice_max(order_by = valor, n=10) %>%
  mutate(bens_direitos = reorder(str_wrap(bens_direitos,20), valor)) %>%
  ggplot()+
  geom_col(aes(x= valor/10^9, y=bens_direitos, fill= destaque), show.legend = FALSE) +
  theme_light() +
  labs(title = 'ano: {frame_time}', x = 'Valor em R$ bi', y = '') +
  transition_time(ano) +
  ease_aes('linear')

ano_exercicio<- unique(bens_direitos_tidy$ano)

p<-
bens_direitos_tidy %>%
  mutate(destaque = ifelse( str_detect(bens_direitos,"criptoativos"),1,0)) %>%
  #filter(ano == ano_exercicio) %>%
  slice_max(order_by = valor, n=10) %>%
  mutate(bens_direitos = reorder(str_wrap(bens_direitos,20), valor)) %>%
  ggplot()+
  geom_col(aes(x= valor/10^9, y=bens_direitos, fill= destaque), show.legend = FALSE) +
  theme_light() 




library(gganimate)
library(gifski)

anim<- 
  p + 
  transition_states(ano,transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE) 


anim<-
  anim+
  labs(x= "Valor em R$ bi",
       y="",
       title= paste("Grandes números IRPF: total bens e consumo para","{format(closest_state)}"))

animate(anim, 200, fps = 1.5,  width = 1200, height = 1000,
        renderer = gifski_renderer())


anim_gif<- animate(anim, 
                   renderer = gifski_renderer(), 
                   nim, 200, 
                   fps = 20,  
                   width = 1200, 
                   height = 1000)
  


bens_set<-
bens_direitos_tidy %>%
  mutate(destaque = ifelse( str_detect(bens_direitos,"criptoativos"),"1","0")) %>%
  group_by(ano) %>%
  mutate(rank = rank(-valor),
         valor_lbl = paste0(" ",round(valor/1e9))) %>%
  group_by(bens_direitos) %>% 
  filter(rank <=30) %>%
  ungroup()


static_plot<-
  bens_set %>%
  ggplot(aes(rank, group= bens_direitos)) +
  geom_tile(aes(y = (valor/10^9)/2,
                height = valor/10^9,
                width = 0.9, fill=destaque), alpha = 0.8, color = NA, show.legend = FALSE)+
  geom_text(aes(y = 0, label = paste(bens_direitos, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=valor/10^9,label = valor_lbl, hjust=0)) +
  geom_text(aes(x=30, y=max(valor/10^9) , label = as.factor(ano)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 20)  + 
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_fill_discrete(guide = guide_legend(title.theme = element_text(
    size = 20), label.theme = element_text(size = 15))) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=1, face="italic", color="grey"),
        plot.caption =element_text(size=14, hjust=1, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

animated <- 
  static_plot + 
  transition_states(ano,transition_length = , state_length = 2, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  ease_aes('linear')+
  enter_fade()+
  exit_fade() 


animate(animated, 150, fps = 5, end_pause = 30, width = 1500, height = 1000, 
        renderer = gifski_renderer("anim_bens.gif"))
