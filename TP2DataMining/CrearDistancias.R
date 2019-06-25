library(jsonlite)
library(sf)
library(repr)
library(scales)
library(geojsonio)
library(geosphere)

sucursales = stream_in(file("sucursales.json",open="r"))
barrios <- st_read("CABA_barrios.geojson")
nrow(sucursales)


coordenadas <- sucursales[,c("lng","lat")]
matriz_distancias <- distm(coordenadas, fun = distHaversine)
data.frame_distancias <- data.frame(matriz_distancias)

data.frame_distancias <- ifelse(data.frame_distancias > 300, 0, 1)
data.frame_distancias <- as.data.frame(data.frame_distancias)
data.frame_distancias[,"Menos300"] <- rowSums(data.frame_distancias[,-1])
data.frame_distancias[,"id"] <- sucursales[,"id"]
data.frame_distancias <- data.frame_distancias[,c("id","Menos300")]