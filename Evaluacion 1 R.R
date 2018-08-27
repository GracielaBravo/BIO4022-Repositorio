library(tidyverse);library(readr);library(ggplot2);library(mapdata);library(maps)
library(dplyr);library(knitr); library(wesanderson)

Parques <- read_csv("biodiversidad/parks.csv")
Especies <- read_csv("biodiversidad/species.csv") %>% select(c(-X14))

#Para saber cuales son de Hawaii
Hawaii <- select(Parques, c("State","Park Name")) %>% filter(State == "HI") 

# Ver parques en mapa
Plotes <- select(Parques, c("Park Name", "Longitude", "Latitude"))
world_map <- map_data("world")
ggplot() + geom_map(data = world_map, map = world_map, aes(map_id=region), fill="grey", color="black") + expand_limits(x = world_map$long, y = world_map$lat) + geom_point(data = Plotes, aes(x = Longitude, y = Latitude), col = "red", cex = 3) + coord_fixed(1.3) +  xlim(-170, -50)+ ylim (10, 70) + theme(panel.background = element_blank()) + labs(x = "Longitude", y = "Latitude")

# Ver Parque vs Especies (Por Categoria)

#Tabla1. Categoria de especies presentes segun ubicacion
Categoria_parque <- select(Especies, c("Park Name", "Category"))
colnames(Categoria_parque) <- c("Park.Name", "Category") 
Tabla0 <- Categoria_parque %>% group_by(Park.Name, Category)%>% summarise(N=n())
Tabla1 <- Categoria_parque %>% group_by(Park.Name, Category)%>% summarise(N=n()) %>% spread(key = Park.Name, value = N) %>% select(c("Category","Haleakala National Park", "Hawaii Volcanoes National Park"))
Tabla2 <- filter(Tabla0, Park.Name == "Haleakala National Park")
Tabla3 <- filter(Tabla0, Park.Name == "Hawaii Volcanoes National Park")
Tabla4 <- rbind(Tabla2,Tabla3)

#Grafico Resumen Hawaii
transform(Tabla4,N = as.numeric(N))
Grafico1 <- ggplot(Tabla4, aes(x= Park.Name, y = N, fill = Category, color = Category)) + geom_col() + theme_grey() + coord_flip() 
Grafico1 + labs(x = "Parks", y = "Distribution") +  scale_x_discrete(labels=c("Haleakala National Park" = "Haleakala", "Hawaii Volcanoes National Park" = "Hawaii Volcanoes")) 
#Grafico1 + facet_wrap(vars(Category))

#Tabla de plantas vasculares segun Especies Nativa o No

Tabla5 <- select(Especies, c("Park Name", "Category", "Scientific Name", "Common Names", "Nativeness")) %>% filter(Category == "Vascular Plant") 
colnames(Tabla5) <- c("Park.Name", "Category", "Scientific.Name", "Common.Names", "Nativeness")
Filtrado <- c("Haleakala National Park", "Hawaii Volcanoes National Park")
Tabla5 <- filter(Tabla5, Park.Name %in% Filtrado)
Tabla6 <- Tabla5 %>% group_by(Park.Name, Nativeness)%>% summarise(N =n())%>% filter(Nativeness != "Unknown", Nativeness != "NA")

#Para los porcentajes
#"(%)" <- Table6[1,3]/(sum(Table6[1,3], Table6[2,3]))


#Grafico plantas Nativas y No nativas
#Grafico2 <- ggplot(Tabla6, aes(x= Park.Name, y = N, fill = Nativeness, color = Nativeness)) + geom_col() + theme_grey()
#Grafico2 + labs(x = "Parks", y = "Number of Species") +  scale_x_discrete(labels=c("Haleakala National Park" = "Haleakala", "Hawaii Volcanoes National Park" = "Hawaii Volcanoes")) 

#Ver nativos y no nativos vs Abundancia
Tabla7 <- select(Especies, c("Park Name", "Category", "Scientific Name", "Common Names", "Nativeness", "Abundance")) %>% filter(Category == "Vascular Plant") 
colnames(Tabla7) <- c("Park.Name", "Category", "Scientific.Name", "Common.Names", "Nativeness", "Abundance")
Filtrado <- c("Haleakala National Park", "Hawaii Volcanoes National Park")
Tabla7 <- filter(Tabla7, Park.Name %in% Filtrado) %>% group_by(Park.Name, Nativeness, Abundance)%>% summarise(N=n()) %>% filter(Abundance != "Unknown", Abundance != "NA")%>% filter(Nativeness != "Unknown", Nativeness != "NA")

Grafico2 <- ggplot(Tabla7, aes(x = Nativeness, y = N, color = Abundance, fill = Abundance)) + geom_bar(stat="summary", fun.y="sum") + theme_classic()
Grafico2 + facet_wrap(vars(Park.Name))

#------------------------------------------
#INTENTOS FALLIDOS XD

#MAPA
#canada <- map_data("worldHires", "canada")
#mexico <- map_data("worldHires", "mexico")
#usa <- map_data("usa")
#no esta alaska ni canada
#ggplot() + geom_polygon(data = canada, aes(x=long, y = lat, group = group), fill = "white", col = "black") + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill="white", col = "black") + geom_polygon(data = mexico, aes(x=long, y = lat, group = group), fill = "white", col = "black") + geom_point(data = Plotes, aes(x = Longitude, y = Latitude))
#ggplot()+ geom_map(world_map, aes(map_id = Country,fill = Country), col = "black") + expand_limits(x = world_map$long, y = world_map$lat)
#newmap <- getMap(resolution = "low")
#map <- plot(newmap,  xlim = c(-200, -60), ylim = c(40, 50), asp = 1)
#puntos <- points(Parques$Longitude, Parques$Latitude, col = "red", cex = .6)

# Ver especie especifica vs Latitud
#Lat_Fami <- select (Especies, c("Park Name", "Family"))
#Lat_Fami <-  group_by(Park.Name) %>% table(Lat_Fami$Family)
#colnames(Lat_Fami) <- c("Especie", "Frecuencia") 
#ggplot()+ geom_bar(Lat_Fami, x = )







