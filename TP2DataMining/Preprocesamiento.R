library(jsonlite)
library(tidyverse)
library(imputeTS)

#Util si están en RStudio, y quieren que el path de data sea relativo a donde están con el script
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load.data.precios <- function() {
  precios = stream_in(file("data/precios.json",open="r"))  
}

to.wide <- function(precios){
  precios.wide <- precios %>%
    group_by(producto, sucursal) %>%
    filter(n() >= 7) %>%
    ungroup() %>%
    select(producto, sucursal, precio, medicion) %>%
    spread(medicion, precio) %>%
    setNames(c("producto", "sucursal", "m1","m2","m3","m4","m5","m6","m7","m8","m9","m10")) %>%
    as.data.frame()
  
  precios.wide <- na.interpolation(precios.wide)
}

preprocesar <- function(precios.wide){
  precios.wide.transformados <- precios.wide %>%
    #Se agrupan los precios en 4 grupos
    mutate(PrecioPeriodo1 = (m1 + m2 + m3) / 3) %>%
    mutate(PrecioPeriodo2 = (m4 + m5) / 2) %>%
    mutate(PrecioPeriodo3 = (m6 + m7) / 2) %>%
    mutate(PrecioPeriodo4 = (m8 + m9 + m10) / 3) %>%
    mutate(PrecioPromedio = (m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10) / 10) %>%
    #Se calculan las diferencias para poder discretizar después
    mutate(Variacion1 = (PrecioPeriodo2 - PrecioPeriodo1) / PrecioPeriodo1) %>%
    mutate(Variacion2 = (PrecioPeriodo3 - PrecioPeriodo2) / PrecioPeriodo2) %>%
    mutate(Variacion3 = (PrecioPeriodo4 - PrecioPeriodo3) / PrecioPeriodo3) %>%
    mutate(VariacionTotal = (PrecioPeriodo4 - PrecioPeriodo1) / PrecioPeriodo1) %>%
    #LABELS PARA LA VARIACION DE PRECIO
    mutate(VariacionDiscretizada1 = case_when(
      Variacion1 < -0.05 ~ "DISMINUCION FUERTE",
      Variacion1 >= -0.05 & Variacion1 < -0.02 ~ "DISMINUCION MEDIA",
      Variacion1 >= -0.02 & Variacion1 < -0.005 ~ "DISMINUCION LEVE",
      Variacion1 >= -0.005 & Variacion1 < 0.005 ~ "MANTIENE",
      Variacion1 >= 0.005 & Variacion1 < 0.05 ~ "AUMENTO LEVE",
      Variacion1 >= 0.05 & Variacion1 < 0.1 ~ "AUMENTO MEDIO",
      Variacion1 >= 0.1  ~ "AUMENTO FUERTE"
    )) %>%
    mutate(VariacionDiscretizada2 = case_when(
      Variacion2 < -0.05 ~ "DISMINUCION FUERTE",
      Variacion2 >= -0.05 & Variacion2 < -0.02 ~ "DISMINUCION MEDIA",
      Variacion2 >= -0.02 & Variacion2 < -0.005 ~ "DISMINUCION LEVE",
      Variacion2 >= -0.005 & Variacion2 < 0.005 ~ "MANTIENE",
      Variacion2 >= 0.005 & Variacion2 < 0.05 ~ "AUMENTO LEVE",
      Variacion2 >= 0.05 & Variacion2 < 0.1 ~ "AUMENTO MEDIO",
      Variacion2 >= 0.1  ~ "AUMENTO FUERTE"
    )) %>% 
    mutate(VariacionDiscretizada3 = case_when(
      Variacion3 < -0.05 ~ "DISMINUCION FUERTE",
      Variacion3 >= -0.05 & Variacion3 < -0.02 ~ "DISMINUCION MEDIA",
      Variacion3 >= -0.02 & Variacion3 < -0.005 ~ "DISMINUCION LEVE",
      Variacion3 >= -0.005 & Variacion3 < 0.005 ~ "MANTIENE",
      Variacion3 >= 0.005 & Variacion3 < 0.05 ~ "AUMENTO LEVE",
      Variacion3 >= 0.05 & Variacion3 < 0.1 ~ "AUMENTO MEDIO",
      Variacion3 >= 0.1  ~ "AUMENTO FUERTE"
    )) %>% 
    mutate(VariacionDiscretizadaTotal = case_when(
      VariacionTotal < -0.05 ~ "DISMINUCION FUERTE",
      VariacionTotal >= -0.05 & VariacionTotal < -0.02 ~ "DISMINUCION MEDIA",
      VariacionTotal >= -0.02 & VariacionTotal < -0.005 ~ "DISMINUCION LEVE",
      VariacionTotal >= -0.005 & VariacionTotal < 0.005 ~ "MANTIENE",
      VariacionTotal >= 0.005 & VariacionTotal < 0.05 ~ "AUMENTO LEVE",
      VariacionTotal >= 0.05 & VariacionTotal < 0.1 ~ "AUMENTO MEDIO",
      VariacionTotal >= 0.1  ~ "AUMENTO FUERTE"
    ))
  
  #Hay que calcular la media por producto para cada medición
  medias.producto <- precios.wide.transformados %>%
    group_by(producto) %>%
    summarize(PrecioMedioPeriodo1 = mean(PrecioPeriodo1), PrecioMedioPeriodo2 = mean(PrecioPeriodo2), PrecioMedioPeriodo3 = mean(PrecioPeriodo3), PrecioMedioPeriodo4 = mean(PrecioPeriodo4), PrecioPromedioTotal = mean(PrecioPromedio))
  
  #Calculamos los precios relativos
  precios.wide.transformados <- precios.wide.transformados %>%
    #Join para tener el valor medio de cada medición
    left_join(medias.producto, by = "producto") %>%
    #Se calculan los precios relativos de cada medición
    mutate(PrecioRelativo1 = (PrecioPeriodo1 - PrecioMedioPeriodo1) / PrecioMedioPeriodo1) %>%
    mutate(PrecioRelativo2 = (PrecioPeriodo2 - PrecioMedioPeriodo2) / PrecioMedioPeriodo2) %>%
    mutate(PrecioRelativo3 = (PrecioPeriodo3 - PrecioMedioPeriodo3) / PrecioMedioPeriodo3) %>%
    mutate(PrecioRelativo4 = (PrecioPeriodo4 - PrecioMedioPeriodo4) / PrecioMedioPeriodo4) %>%
    mutate(PrecioRelativoMedio = (PrecioPromedio - PrecioPromedioTotal) / PrecioPromedioTotal) %>%
    #Se los discretiza
    mutate(PrecioRelativoDiscretizado1 = case_when(
      PrecioRelativo1 < -0.1 ~ "MUY BARATO",
      PrecioRelativo1 >= -0.1 & PrecioRelativo1 < -0.05 ~ "MEDIANAMENTE BARATO",
      PrecioRelativo1 >= -0.05 & PrecioRelativo1 < -0.01 ~ "LEVEMENTE BARATO",
      PrecioRelativo1 >= -0.01 & PrecioRelativo1 < 0.01 ~ "MEDIO",
      PrecioRelativo1 >= 0.01 & PrecioRelativo1 < 0.05 ~ "LEVEMENTE CARO",
      PrecioRelativo1 >= 0.05 & PrecioRelativo1 < 0.1 ~ "MEDIO CARO",
      PrecioRelativo1 >= 0.1  ~ "MUY CARO"
    )) %>%
    mutate(PrecioRelativoDiscretizado2 = case_when(
      PrecioRelativo2 < -0.1 ~ "MUY BARATO",
      PrecioRelativo2 >= -0.1 & PrecioRelativo2 < -0.05 ~ "MEDIANAMENTE BARATO",
      PrecioRelativo2 >= -0.05 & PrecioRelativo2 < -0.01 ~ "LEVEMENTE BARATO",
      PrecioRelativo2 >= -0.01 & PrecioRelativo2 < 0.01 ~ "MEDIO",
      PrecioRelativo2 >= 0.01 & PrecioRelativo2 < 0.05 ~ "LEVEMENTE CARO",
      PrecioRelativo2 >= 0.05 & PrecioRelativo2 < 0.1 ~ "MEDIO CARO",
      PrecioRelativo2 >= 0.1  ~ "MUY CARO"
    )) %>%
    mutate(PrecioRelativoDiscretizado3 = case_when(
      PrecioRelativo3 < -0.1 ~ "MUY BARATO",
      PrecioRelativo3 >= -0.1 & PrecioRelativo3 < -0.05 ~ "MEDIANAMENTE BARATO",
      PrecioRelativo3 >= -0.05 & PrecioRelativo3 < -0.01 ~ "LEVEMENTE BARATO",
      PrecioRelativo3 >= -0.01 & PrecioRelativo3 < 0.01 ~ "MEDIO",
      PrecioRelativo3 >= 0.01 & PrecioRelativo3 < 0.05 ~ "LEVEMENTE CARO",
      PrecioRelativo3 >= 0.05 & PrecioRelativo3 < 0.1 ~ "MEDIO CARO",
      PrecioRelativo3 >= 0.1  ~ "MUY CARO"
    )) %>%
    mutate(PrecioRelativoDiscretizado4 = case_when(
      PrecioRelativo4 < -0.1 ~ "MUY BARATO",
      PrecioRelativo4 >= -0.1 & PrecioRelativo4 < -0.05 ~ "MEDIANAMENTE BARATO",
      PrecioRelativo4 >= -0.05 & PrecioRelativo4 < -0.01 ~ "LEVEMENTE BARATO",
      PrecioRelativo4 >= -0.01 & PrecioRelativo4 < 0.01 ~ "MEDIO",
      PrecioRelativo4 >= 0.01 & PrecioRelativo4 < 0.05 ~ "LEVEMENTE CARO",
      PrecioRelativo4 >= 0.05 & PrecioRelativo4 < 0.1 ~ "MEDIO CARO",
      PrecioRelativo4 >= 0.1  ~ "MUY CARO"
    )) %>%
    mutate(PrecioRelativoDiscretizadoMedio = case_when(
      PrecioRelativoMedio < -0.1 ~ "MUY BARATO",
      PrecioRelativoMedio >= -0.1 & PrecioRelativoMedio < -0.05 ~ "MEDIANAMENTE BARATO",
      PrecioRelativoMedio >= -0.05 & PrecioRelativoMedio < -0.01 ~ "LEVEMENTE BARATO",
      PrecioRelativoMedio >= -0.01 & PrecioRelativoMedio < 0.01 ~ "MEDIO",
      PrecioRelativoMedio >= 0.01 & PrecioRelativoMedio < 0.05 ~ "LEVEMENTE CARO",
      PrecioRelativoMedio >= 0.05 & PrecioRelativoMedio < 0.1 ~ "MEDIO CARO",
      PrecioRelativoMedio >= 0.1  ~ "MUY CARO"
    ))
}

datosSocioEconomicos <- function(precios){

	sucursales <- read.csv("data/Sucursales.csv",stringsAsFactors=FALSE)
	sucursales <- sucursales[,c("id","BARRIO","COMUNA","sucursalTipo","cadena","nombre")]
	Alquiler <- read.csv(file="data/AlquilerDepartamentos.csv",stringsAsFactors=FALSE, header=TRUE, sep=",", encoding = "UTF-8")
	colnames(Alquiler) <- c("Barrio","USDm2Alquiler")

	Venta <- read.csv(file="data/VentasDepartamentos.csv",stringsAsFactors=FALSE, header=TRUE, sep=",", encoding = "UTF-8")
	colnames(Venta) <- c("Barrio","USDm2Venta")

	Poblacion <- read.csv(file="data/PoblacionBarrio.csv",stringsAsFactors=FALSE, header=TRUE, sep=",", encoding = "UTF-8")

	Alquiler[,"Barrio"] <- toupper(Alquiler[,"Barrio"])
	Venta[,"Barrio"] <- toupper(Venta[,"Barrio"])
	Poblacion[,"Barrio"] <- toupper(Poblacion[,"Barrio"])

	mergeado <- merge(sucursales, Alquiler,by.x = "BARRIO", by.y = "Barrio")
	mergeado <- merge(mergeado, Venta,by.x = "BARRIO", by.y = "Barrio")
	mergeado <- merge(mergeado, Poblacion,by.x = "BARRIO", by.y = "Barrio")
	mergeado.final <- merge(mergeado, precios,by.x = "id", by.y = "sucursal")
	return(mergeado.final)
}


#Cargo los precios
precios <- load.data.precios()

#Los paso a wide
precios.wide <- to.wide(precios)

#Hago las transformaciones
precios.wide.transformados <- preprocesar(precios.wide)

#Elegimos los atributos
precios.wide.transformados.final <- select(precios.wide.transformados, producto, sucursal, PrecioRelativoDiscretizado1, PrecioRelativoDiscretizado2, PrecioRelativoDiscretizado3, PrecioRelativoDiscretizado4, PrecioRelativoDiscretizadoMedio, VariacionDiscretizada1, VariacionDiscretizada2, VariacionDiscretizada3, VariacionDiscretizadaTotal)

#Agregamos datos de socioeconomicos barrios 

precios.wide.transformados.barruis <- datosSocioEconomicos(precios.wide.transformados.final)

#Guardar a CSV
#write.csv(precios.wide, "PreciosWide.csv")
#write.csv(precios.wide.transformados, "PreciosWideTransformados.csv")
#write.csv(precios.wide.transformados.final, "PreciosWideTransformadosFinal.csv")