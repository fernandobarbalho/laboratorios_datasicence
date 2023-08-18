library(tidyverse)
library(patchwork)

# allocate RAM memory to Java
options(java.parameters = "-Xmx2G")

# 1) build transport network, pointing to the path where OSM and GTFS data are stored
library(r5r)
path <- system.file("extdata/poa", package = "r5r")
r5r_core <- setup_r5(data_path = path, verbose = FALSE)

# 2) load origin/destination points and set arguments
points <- read.csv(system.file("extdata/poa/poa_hexgrid.csv", package = "r5r"))
mode <- c("WALK", "TRANSIT")
max_walk_time <- 30   # minutes
max_trip_duration <- 60 # minutes
departure_datetime <- as.POSIXct("13-05-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# 3.1) calculate a travel time matrix
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = points,
                          destinations = points,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_time = max_walk_time,
                          max_trip_duration = max_trip_duration)

# 3.2) or get detailed info on multiple alternative routes
det <- detailed_itineraries(r5r_core = r5r_core,
                            origins = points[370, ],
                            destinations = points[200, ],
                            mode = mode,
                            departure_datetime = departure_datetime,
                            max_walk_time = max_walk_time,
                            max_trip_duration = max_trip_duration,
                            shortest_path = FALSE,
                            drop_geometry = FALSE)

# 4) Calculate number of schools accessible within 20 minutes 
access <- accessibility(r5r_core = r5r_core,
                        origins = points,
                        destinations = points,
                        opportunities_colname = "schools",
                        decay_function = "step",
                        cutoffs = 21,
                        mode =  c("WALK", "TRANSIT"),
                        verbose = FALSE)

healthcare_points<-
  points %>%
  filter(healthcare == 1)


iso_healthcare<-
  isochrone(
    r5r_core,
    origins = healthcare_points,
    mode = "transit",
    mode_egress = "WALK",
    cutoffs = c(0, 15, 30,60),
    sample_size = 0.8,
    departure_datetime = Sys.time(),
    time_window = 10L,
    max_walk_time = Inf,
    max_bike_time = Inf,
    max_car_time = Inf,
    max_trip_duration = 120L,
    walk_speed = 3.6,
    bike_speed = 12,
    max_rides = 3,
    max_lts = 2,
    draws_per_minute = 5L,
    n_threads = Inf,
    verbose = FALSE,
    progress = TRUE
  )



iso_healthcare_walk<-
  isochrone(
    r5r_core,
    origins = healthcare_points,
    mode = "WALK",
    mode_egress = "WALK",
    cutoffs = c(0, 15, 30),
    sample_size = 0.8,
    departure_datetime = Sys.time(),
    time_window = 10L,
    max_walk_time = Inf,
    max_bike_time = Inf,
    max_car_time = Inf,
    max_trip_duration = 120L,
    walk_speed = 3.6,
    bike_speed = 12,
    max_rides = 3,
    max_lts = 2,
    draws_per_minute = 5L,
    n_threads = Inf,
    verbose = FALSE,
    progress = TRUE
  )




library(tidyverse)
library(geobr)


detach(package:geobr)

poa_sf<-
  read_municipality()


iso_healthcare %>%
  ggplot() +
  theme_light() +
  geom_sf(aes(fill=isochrone)) +
  labs(
    title= "Tempos de deslocamentos para estabelecimentos de saúde",
    subtitle = "Transporte público de Porto Alegre",
    fill= str_wrap("Tempo de deslocamento",20)
  )


iso_healthcare_walk %>%
  ggplot() +
  theme_light() +
  geom_sf(aes(fill=isochrone)) +
  labs(
    title= "Tempos de deslocamentos para estabelecimentos de saúde",
    subtitle = "Caminhando em Porto Alegre",
    fill= str_wrap("Tempo de deslocamento",20)
  )


iso_healthcare_walk %>%
  ggplot() +
  theme_light() +
  geom_sf() +
  labs(
    title= "Tempos de deslocamentos para estabelecimentos de saúde",
    subtitle = "Caminhando em Porto Alegre",
    fill= str_wrap("Tempo de deslocamento",20)
  )


library(geobr)


mapa_municipios<- geobr::read_municipality()


mapa_municipios <- readRDS("~/Github/laboratorios_datasicence/mapa_municipios.RDS")

mapa_poa<- 
  mapa_municipios %>%
  filter(code_muni == 4314902)


iso_healthcare %>%
  ggplot() +
  theme_light() +
  geom_sf() +
  geom_sf(data= mapa_poa, fill=NA) +
  labs(
    title= "Tempos de deslocamentos para estabelecimentos de saúde",
    subtitle = "Caminhando em Porto Alegre",
    fill= str_wrap("Tempo de deslocamento",20)
  )


library(aopdata)
library(ggplot2)

# download data
fortaleza <- read_access(city = 'for', 
                   mode = 'public_transport', 
                   year = 2019,
                   geometry = T)

# plot
ggplot(data = fortaleza) +
  geom_sf(aes(fill = ifelse(TMISA>60,60,TMISA)), color= NA) +
  scale_fill_viridis_c(direction = -1, breaks= seq(0,60,15),
                       labels = c('0', '15', '30', '45', '60+'),
                       name = "Minutos") +
  theme_void()


rec <- read_access(city = 'rec', 
                         mode = 'public_transport', 
                         year = 2019,
                         geometry = T)

# plot

x<-
ggplot(data = rec) +
  geom_sf(aes(fill = ifelse(TMISA>60,60,TMISA)), color= NA) +
  scale_fill_viridis_c(direction = -1, breaks= seq(0,60,15),
                       labels = c('0', '15', '30', '45', '60+'),
                       name = "Minutos") +
  theme_void()


rec_pop<-
  aopdata::read_population(city = "Recife", year=2010)

y<-
  ggplot(data = rec) +
  geom_sf(aes(fill = P001), color= NA) +
  scale_fill_viridis_c(direction = 1,
                       name = "Habitantes") +
  theme_void()


x+y



poa <- read_access(city = 'poa', 
                   mode = 'public_transport', 
                   year = 2019,
                   geometry = T)


# plot
ggplot(data = poa) +
  geom_sf(aes(fill = ifelse(TMISA>60,60,TMISA)), color= NA) +
  scale_fill_viridis_c(direction = -1, breaks= seq(0,60,15),
                       labels = c('0', '15', '30', '45', '60+'),
                       name = "Minutos") +
  theme_void()

