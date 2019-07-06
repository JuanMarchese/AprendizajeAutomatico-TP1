library(jsonlite)
library(tidyverse)
library(imputeTS)
library(readxl)
library(arules)
library(arulesViz)

#Util si están en RStudio, y quieren que el path de data sea relativo a donde están con el script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

dolar = c(35.5475,35.505,38.155,37.7017,38.3217,37.8083,37.705,37.24,38.32,38.96)
variacion_dolar = dolar / dolar[1]

calcularVariacion <- function(variacion,nombre){
  resultado = case_when(
    variacion < -0.05 ~ paste(nombre,": ---",sep=""),
    variacion >= -0.05 & variacion < -0.02 ~ paste(nombre,": --",sep=""),
    variacion >= -0.02 & variacion < -0.005 ~ paste(nombre,": -",sep=""),
    variacion >= -0.005 & variacion < 0.005 ~ paste(nombre,": =",sep=""),
    variacion >= 0.005 & variacion < 0.05 ~ paste(nombre,": +",sep=""),
    variacion >= 0.05 & variacion < 0.1 ~ paste(nombre,": ++",sep=""),
    variacion >= 0.1  ~ paste(nombre,": +++",sep="")
  )
  
  return(resultado)
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
    
    
    #variacion de precios en dolares
    mutate(PrecioDolarPeriodo1 = (m1 / dolar[1] + m2 / dolar[2] + m3 / dolar[3]) / 3) %>%
    mutate(PrecioDolarPeriodo2 = (m4 / dolar[4] + m5 / dolar[5]) / 2) %>%
    mutate(PrecioDolarPeriodo3 = (m6 / dolar[6] + m7 / dolar[7]) / 2) %>%
    mutate(PrecioDolarPeriodo4 = (m8 / dolar[8] + m9 / dolar[9] + m10 / dolar[10]) / 3) %>%
    mutate(PrecioDolarPromedio = (m1 / dolar[1] + m2 / dolar[2] + m3 / dolar[3] + 
                                    m4 / dolar[4] + m5 / dolar[5] + m6 / dolar[6] +
                                    m7 / dolar[7] + m8 / dolar[8] + m9 / dolar[9] + m10 / dolar[10]) / 10) %>%
    #Se calculan las diferencias para poder discretizar después
    mutate(VariacionPrecioDolar1 = (PrecioDolarPeriodo2 - PrecioDolarPeriodo1) / PrecioDolarPeriodo1) %>%
    mutate(VariacionPrecioDolar2 = (PrecioDolarPeriodo3 - PrecioDolarPeriodo2) / PrecioDolarPeriodo2) %>%
    mutate(VariacionPrecioDolar3 = (PrecioDolarPeriodo4 - PrecioDolarPeriodo3) / PrecioDolarPeriodo3) %>%
    mutate(VariacionPrecioDolarTotal = (PrecioDolarPeriodo4 - PrecioDolarPeriodo1) / PrecioDolarPeriodo1) %>%
    
    mutate(VDolar1 = calcularVariacion(VariacionPrecioDolar1,"VarDolar1")) %>%
    mutate(VDolar2 = calcularVariacion(VariacionPrecioDolar2,"VarDolar2")) %>%
    mutate(VDolar3 = calcularVariacion(VariacionPrecioDolar3,"VarDolar3")) %>%
    mutate(VDolarTotal = calcularVariacion(VariacionPrecioDolarTotal,"VarDolarTot")) %>%
    
    
    #LABELS PARA LA VARIACION DE PRECIO
    mutate(VariacionDiscretizada1 = case_when(
      Variacion1 < -0.05 ~ "Var1: ---",
      Variacion1 >= -0.05 & Variacion1 < -0.02 ~ "Var1: --",
      Variacion1 >= -0.02 & Variacion1 < -0.005 ~ "Var1: -",
      Variacion1 >= -0.005 & Variacion1 < 0.005 ~ "Var1: =",
      Variacion1 >= 0.005 & Variacion1 < 0.05 ~ "Var1: +",
      Variacion1 >= 0.05 & Variacion1 < 0.1 ~ "Var1: ++",
      Variacion1 >= 0.1  ~ "Var1: +++"
    )) %>%
    mutate(VariacionDiscretizada2 = case_when(
      Variacion2 < -0.05/2 ~ "Var2: ---",
      Variacion2 >= -0.05/2 & Variacion2 < -0.02/2 ~ "Var2: --",
      Variacion2 >= -0.02/2 & Variacion2 < -0.005/2 ~ "Var2: -",
      Variacion2 >= -0.005/2 & Variacion2 < 0.005/2 ~ "Var2: =",
      Variacion2 >= 0.005/2 & Variacion2 < 0.05/2 ~ "Var2: +",
      Variacion2 >= 0.05/2 & Variacion2 < 0.1/2 ~ "Var2: ++",
      Variacion2 >= 0.1/2  ~ "Var2: +++"
    )) %>% 
    mutate(VariacionDiscretizada3 = case_when(
      Variacion3 < -0.05 ~ "Var3: ---",
      Variacion3 >= -0.05 & Variacion3 < -0.02 ~ "Var3: --",
      Variacion3 >= -0.02 & Variacion3 < -0.005 ~ "Var3: -",
      Variacion3 >= -0.005 & Variacion3 < 0.005 ~ "Var3: =",
      Variacion3 >= 0.005 & Variacion3 < 0.05 ~ "Var3: +",
      Variacion3 >= 0.05 & Variacion3 < 0.1 ~ "Var3: ++",
      Variacion3 >= 0.1  ~ "Var3: +++"
    )) %>% 
    mutate(VariacionDiscretizadaTotal = case_when(
      VariacionTotal < -0.01 ~ "VarTotal: Deflacionaria",
      VariacionTotal >= -0.01 & VariacionTotal < 0.01 ~ "VarTotal: Neutral",
      VariacionTotal >= 0.01 & VariacionTotal < 0.04 ~ "VarTotal: Leve",
      VariacionTotal >= 0.04 & VariacionTotal < 0.07 ~ "VarTotal: Regular",
      VariacionTotal >= 0.07 & VariacionTotal < 0.14 ~ "VarTotal: Fuerte",
      VariacionTotal >= 0.14 & VariacionTotal < 0.35 ~ "VarTotal: Muy Fuerte",
      VariacionTotal >= 0.35  ~ "VarTotal: Hiperinflacionaria"
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
      PrecioRelativo1 < -0.1 ~ "PrecRel1: ---",
      PrecioRelativo1 >= -0.1 & PrecioRelativo1 < -0.05 ~ "PrecRel1: --",
      PrecioRelativo1 >= -0.05 & PrecioRelativo1 < -0.01 ~ "PrecRel1: -",
      PrecioRelativo1 >= -0.01 & PrecioRelativo1 < 0.01 ~ "PrecRel1: =",
      PrecioRelativo1 >= 0.01 & PrecioRelativo1 < 0.05 ~ "PrecRel1: +",
      PrecioRelativo1 >= 0.05 & PrecioRelativo1 < 0.1 ~ "PrecRel1: ++",
      PrecioRelativo1 >= 0.1  ~ "PrecRel1: +++"
    )) %>%
    mutate(PrecioRelativoDiscretizado2 = case_when(
      PrecioRelativo2 < -0.1 ~ "PrecRel2: ---",
      PrecioRelativo2 >= -0.1 & PrecioRelativo2 < -0.05 ~ "PrecRel2: --",
      PrecioRelativo2 >= -0.05 & PrecioRelativo2 < -0.01 ~ "PrecRel2: -",
      PrecioRelativo2 >= -0.01 & PrecioRelativo2 < 0.01 ~ "PrecRel2: =",
      PrecioRelativo2 >= 0.01 & PrecioRelativo2 < 0.05 ~ "PrecRel2: +",
      PrecioRelativo2 >= 0.05 & PrecioRelativo2 < 0.1 ~ "PrecRel2: ++",
      PrecioRelativo2 >= 0.1  ~ "PrecRel2: +++"
    )) %>%
    mutate(PrecioRelativoDiscretizado3 = case_when(
      PrecioRelativo3 < -0.1 ~ "PrecRel3: ---",
      PrecioRelativo3 >= -0.1 & PrecioRelativo3 < -0.05 ~ "PrecRel3: --",
      PrecioRelativo3 >= -0.05 & PrecioRelativo3 < -0.01 ~ "PrecRel3: -",
      PrecioRelativo3 >= -0.01 & PrecioRelativo3 < 0.01 ~ "PrecRel3: =",
      PrecioRelativo3 >= 0.01 & PrecioRelativo3 < 0.05 ~ "PrecRel3: +",
      PrecioRelativo3 >= 0.05 & PrecioRelativo3 < 0.1 ~ "PrecRel3: ++",
      PrecioRelativo3 >= 0.1  ~ "PrecRel3: +++"
    )) %>%
    mutate(PrecioRelativoDiscretizado4 = case_when(
      PrecioRelativo4 < -0.1 ~ "PrecRel4: ---",
      PrecioRelativo4 >= -0.1 & PrecioRelativo4 < -0.05 ~ "PrecRel4: --",
      PrecioRelativo4 >= -0.05 & PrecioRelativo4 < -0.01 ~ "PrecRel4: -",
      PrecioRelativo4 >= -0.01 & PrecioRelativo4 < 0.01 ~ "PrecRel4: =",
      PrecioRelativo4 >= 0.01 & PrecioRelativo4 < 0.05 ~ "PrecRel4: +",
      PrecioRelativo4 >= 0.05 & PrecioRelativo4 < 0.1 ~ "PrecRel4: ++",
      PrecioRelativo4 >= 0.1  ~ "PrecRel4: +++"
    )) %>%
    mutate(PrecioRelativoDiscretizadoMedio = case_when(
      PrecioRelativoMedio < -0.1 ~ "PrecRelMedio_label: ---",
      PrecioRelativoMedio >= -0.1 & PrecioRelativoMedio < -0.05 ~ "PrecRelMedio_label: --",
      PrecioRelativoMedio >= -0.05 & PrecioRelativoMedio < -0.01 ~ "PrecRelMedio_label: -",
      PrecioRelativoMedio >= -0.01 & PrecioRelativoMedio < 0.01 ~ "PrecRelMedio_label: =",
      PrecioRelativoMedio >= 0.01 & PrecioRelativoMedio < 0.05 ~ "PrecRelMedio_label: +",
      PrecioRelativoMedio >= 0.05 & PrecioRelativoMedio < 0.1 ~ "PrecRelMedio_label: ++",
      PrecioRelativoMedio >= 0.1  ~ "PrecRelMedio_label: +++"
    ))
}

# cargamos los datos socioecomomicos y datos de la sucursal
datosSocioEconomicos <- function(precios){
  
  sucursales <- read.csv("data/Sucursales.csv",stringsAsFactors=FALSE)
  sucursales <- sucursales[,c("id","BARRIO","COMUNA","sucursalTipo","cadena","nombre","Menos300")]
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

# cargamos los datos de los productos
mergeprod <- function(base){
  prod <- stream_in(file("data/productos.json",open="r"))
  prod <- as.data.frame(prod)
  clasif_productos <- read_excel("data/agrupamiento productos.xlsx")
  prod <- merge(prod, clasif_productos, by.x = "nombre",by.y = "Producto",all.x = T)
  
  prod <- prod %>% 
    mutate(nombre_1 = strsplit(nombre,' ')[[1]][1])
  
  base <- merge(base, prod,by.x = "producto", by.y='id',all.x = TRUE)
  
  base[,"termino_vino"] <- NA
  base[,"termino_galletitas"] <- NA
  base[,"termino_leche"] <- NA
  base[,"termino_agua"] <- NA
  base[,"termino_yogur"] <- NA
  base[,"termino_fideos"] <- NA
  base[,"termino_jabon"] <- NA
  base[,"termino_gaseosa"] <- NA
  base[,"termino_mate"] <- NA
  base[,"termino_cerveza"] <- NA
  
  base[grepl("vino",tolower(base[,"nombre.y"])) ,"termino_vino"] <- "S"
  base[grepl("galletitas",tolower(base[,"nombre.y"])) ,"termino_galletitas"] <- "S"
  base[grepl("leche",tolower(base[,"nombre.y"])) ,"termino_leche"] <- "S"
  base[grepl("agua",tolower(base[,"nombre.y"])) ,"termino_agua"] <- "S"
  base[grepl("yogur",tolower(base[,"nombre.y"])) ,"termino_yogur"] <- "S"
  base[grepl("fideos",tolower(base[,"nombre.y"])) ,"termino_fideos"] <- "S"
  base[grepl("jabon",tolower(base[,"nombre.y"])) ,"termino_jabon"] <- "S"
  base[grepl("gaseosa",tolower(base[,"nombre.y"])) ,"termino_gaseosa"] <- "S"
  base[grepl("mate",tolower(base[,"nombre.y"])) ,"termino_mate"] <- "S"
  base[grepl("cerveza",tolower(base[,"nombre.y"])) ,"termino_cerveza"] <- "S"
  
  return(base)
}

# labels para las variables demograficas
labeldemo <- function(final) {
  q_1 <- quantile(final$USDm2Alquiler,seq(0.1,0.9,.1))
  q_2 <- quantile(final$USDm2Venta,seq(0.1,0.9,.1))
  q_3 <- quantile(final$Densidad,seq(0.1,0.9,.1))
  q_4 <- quantile(final$Poblacion,seq(0.1,0.9,.1))
  q_5 <- quantile(final$Superficie,seq(0.1,0.9,.1))
  centro <- c(1,3,5,6,11,15)
  sur <- c(10,9,8,7,4)
  norte <- c(12,13,14,2)
  
  final %>%
    mutate(USDm2alq_label = case_when(
      USDm2Alquiler < q_1[1] ~ "USDm2Alq: ---",
      USDm2Alquiler >= q_1[1] & USDm2Alquiler < q_1[3] ~ "USDm2Alq: --",
      USDm2Alquiler >= q_1[3] & USDm2Alquiler < q_1[4] ~ "USDm2Alq: -",
      USDm2Alquiler >= q_1[4] & USDm2Alquiler < q_1[6] ~ "USDm2Alq: =",
      USDm2Alquiler >= q_1[6] & USDm2Alquiler < q_1[8] ~ "USDm2Alq: +",
      USDm2Alquiler >= q_1[8] & USDm2Alquiler < q_1[9] ~ "USDm2Alq: ++",
      USDm2Alquiler >= q_1[9]  ~ "USDm2Alq: +++"
    )) %>%
    mutate(USDm2venta_label = case_when(
      USDm2Venta < q_2[1] ~ "USDm2Venta: ---",
      USDm2Venta >= q_2[1] & USDm2Venta < q_2[3] ~ "USDm2Venta: --",
      USDm2Venta >= q_2[3] & USDm2Venta < q_2[4] ~ "USDm2Venta: -",
      USDm2Venta >= q_2[4] & USDm2Venta < q_2[6] ~ "USDm2Venta: =",
      USDm2Venta >= q_2[6] & USDm2Venta < q_2[8] ~ "USDm2Venta: +",
      USDm2Venta >= q_2[8] & USDm2Venta < q_2[9] ~ "USDm2Venta: ++",
      USDm2Venta >= q_2[9]  ~ "USDm2Venta: +++"
    )) %>%
    mutate(Densidad_label = case_when(
      Densidad < q_3[1] ~ "Densidad: ---",
      Densidad >= q_3[1] & Densidad < q_3[3] ~ "Densidad: --",
      Densidad >= q_3[3] & Densidad < q_3[4] ~ "Densidad: -",
      Densidad >= q_3[4] & Densidad < q_3[6] ~ "Densidad: =",
      Densidad >= q_3[6] & Densidad < q_3[8] ~ "Densidad: +",
      Densidad >= q_3[8] & Densidad < q_3[9] ~ "Densidad: ++",
      Densidad >= q_3[9]  ~ "Densidad: +++"
    )) %>%
    mutate(Poblacion_label = case_when(
      Poblacion < q_4[1] ~ "Poblacion: ---",
      Poblacion >= q_4[1] & Poblacion < q_4[3] ~ "Poblacion: --",
      Poblacion >= q_4[3] & Poblacion < q_4[4] ~ "Poblacion: -",
      Poblacion >= q_4[4] & Poblacion < q_4[6] ~ "Poblacion: =",
      Poblacion >= q_4[6] & Poblacion < q_4[8] ~ "Poblacion: +",
      Poblacion >= q_4[8] & Poblacion < q_4[9] ~ "Poblacion: ++",
      Poblacion >= q_4[9]  ~ "Poblacion: +++"
    )) %>%
    mutate(Superficie_label = case_when(
      Superficie < q_5[1] ~ "Superficie: ---",
      Superficie >= q_5[1] & Superficie < q_5[3] ~ "Superficie: --",
      Superficie >= q_5[3] & Superficie < q_5[4] ~ "Superficie: -",
      Superficie >= q_5[4] & Superficie < q_5[6] ~ "Superficie: =",
      Superficie >= q_5[6] & Superficie < q_5[8] ~ "Superficie: +",
      Superficie >= q_5[8] & Superficie < q_5[9] ~ "Superficie: ++",
      Superficie >= q_5[9]  ~ "Superficie: +++"
    )) %>% 
    mutate(Region = case_when(
      COMUNA %in% centro ~ "Centro",
      COMUNA %in% sur ~ "Sur",
      COMUNA %in% norte ~ "Norte"
    )) %>% 
    mutate(Menos300 = case_when(
      Menos300 < 4 ~ "Competencia: Baja",
      Menos300 >= 4 & Menos300 < 7 ~ "Competencia: Regular",
      Menos300 >= 7  ~ "Competencia: Alta"
    ))
}


#agregamos etiquetas para definir si un precio es atipico o no

TipoPrecio <- function(final){
  #Hay que calcular los cuartiles
  cuartiles.producto.comuna <- final %>%
    group_by(producto, COMUNA) %>%
    summarize(Q1.Periodo1 = quantile(log(PrecioPeriodo1),.25),Q3.Periodo1 = quantile(log(PrecioPeriodo1),.75),
              Q1.Periodo2 = quantile(log(PrecioPeriodo2),.25),Q3.Periodo2 = quantile(log(PrecioPeriodo2),.75),
              Q1.Periodo3 = quantile(log(PrecioPeriodo3),.25),Q3.Periodo3 = quantile(log(PrecioPeriodo3),.75),
              Q1.Periodo4 = quantile(log(PrecioPeriodo4),.25),Q3.Periodo4 = quantile(log(PrecioPeriodo4),.75))
  
  #Join para tener los cuartiles de cada medicion de cada medición
  final <- final %>%  
    left_join(cuartiles.producto.comuna, by = c("producto",'COMUNA')) %>%
    #Etiquetamos las mediciones de acuerdo a los precios registrados en cada medicion
    mutate(TipoDato.1 = case_when(
      log(PrecioPeriodo1) < Q1.Periodo1-1.5*(Q3.Periodo1-Q1.Periodo1) ~ "TipoDato1: Atípico(-)",
      log(PrecioPeriodo1) >= Q1.Periodo2-1.5*(Q3.Periodo1-Q1.Periodo1) & log(PrecioPeriodo1) < Q1.Periodo1 ~ "TipoDato1: Primer Cuartil",
      log(PrecioPeriodo1) >= Q1.Periodo1 & log(PrecioPeriodo1) < Q3.Periodo1 ~ "TipoDato1: Regular",
      log(PrecioPeriodo1) >= Q3.Periodo1 & log(PrecioPeriodo1) < Q3.Periodo1+1.5*(Q3.Periodo1-Q1.Periodo1) ~ "TipoDato1: Último Cuartil",
      log(PrecioPeriodo1) <= Q3.Periodo1+1.5*(Q3.Periodo1-Q1.Periodo1) ~ "TipoDato1: Atípico(+)")) %>%
    
    mutate(TipoDato.2 = case_when(
      log(PrecioPeriodo2) < Q1.Periodo2-1.5*(Q3.Periodo2-Q1.Periodo2) ~ "TipoDato2: Atípico(-)",
      log(PrecioPeriodo2) >= Q1.Periodo2-1.5*(Q3.Periodo2-Q1.Periodo2) & log(PrecioPeriodo2) < Q1.Periodo2 ~ "TipoDato2: Primer Cuartil",
      log(PrecioPeriodo2) >= Q1.Periodo2 & log(PrecioPeriodo2) < Q3.Periodo2 ~ "TipoDato2: Regular",
      log(PrecioPeriodo2) >= Q3.Periodo2 & log(PrecioPeriodo2) < Q3.Periodo2+1.5*(Q3.Periodo2-Q1.Periodo2) ~ "TipoDato2: Último Cuartil",
      log(PrecioPeriodo2) <= Q3.Periodo2+1.5*(Q3.Periodo2-Q1.Periodo2) ~ "TipoDato2: Atípico(+)")) %>%
    
    mutate(TipoDato.3 = case_when(
      log(PrecioPeriodo3) < Q1.Periodo3-1.5*(Q3.Periodo3-Q1.Periodo3) ~ "TipoDato3: Atípico(-)",
      log(PrecioPeriodo3) >= Q1.Periodo3-1.5*(Q3.Periodo3-Q1.Periodo3) & log(PrecioPeriodo3) < Q1.Periodo3 ~ "TipoDato3: Primer Cuartil",
      log(PrecioPeriodo3) >= Q1.Periodo3 & log(PrecioPeriodo3) < Q3.Periodo3 ~ "TipoDato3: Regular",
      log(PrecioPeriodo3) >= Q3.Periodo3 & log(PrecioPeriodo3) < Q3.Periodo3+1.5*(Q3.Periodo3-Q1.Periodo3) ~ "TipoDato3: Último Cuartil",
      log(PrecioPeriodo3) <= Q3.Periodo3+1.5*(Q3.Periodo3-Q1.Periodo3) ~ "TipoDato3: Atípico(+)")) %>%
    
    mutate(TipoDato.4 = case_when(
      log(PrecioPeriodo4) < Q1.Periodo4-1.5*(Q3.Periodo4-Q1.Periodo4) ~ "TipoDato4: Atípico(-)",
      log(PrecioPeriodo4) >= Q1.Periodo4-1.5*(Q3.Periodo4-Q1.Periodo4) & log(PrecioPeriodo4) < Q1.Periodo4 ~ "TipoDato4: Primer Cuartil",
      log(PrecioPeriodo4) >= Q1.Periodo4 & log(PrecioPeriodo4) < Q3.Periodo4 ~ "TipoDato4: Regular",
      log(PrecioPeriodo4) >= Q3.Periodo4 & log(PrecioPeriodo4) < Q3.Periodo4+1.5*(Q3.Periodo4-Q1.Periodo4) ~ "TipoDato4: Último Cuartil",
      log(PrecioPeriodo4) <= Q3.Periodo4+1.5*(Q3.Periodo4-Q1.Periodo4) ~ "TipoDato4: Atípico(+)"))
}

#Cargo los precios
precios <- load.data.precios() %>%
  to.wide() %>%
  preprocesar() %>%
  datosSocioEconomicos() %>%
  mergeprod() %>%
  labeldemo() %>%
  TipoPrecio()

#Elegimos los atributos
prueba <- precios %>%
  #filter(termino_cerveza == 'S') %>%
  select(producto,        
    #BARRIO,                         
    #COMUNA,
    Menos300,
    #sucursalTipo,                    
    #cadena,                         
    #VariacionDiscretizada1,          
    #VariacionDiscretizada2,         
    #VariacionDiscretizada3,          
    VariacionDiscretizadaTotal,      
    #PrecioRelativoDiscretizado1,
    #PrecioRelativoDiscretizado2,
    #PrecioRelativoDiscretizado3,    
    #PrecioRelativoDiscretizado4,
    PrecioRelativoDiscretizadoMedio,
    #VDolar1,
    #VDolar2,
    #VDolar3,
    #VDolarTotal,
    termino_vino,
    termino_galletitas,
    termino_leche,
    termino_agua,
    termino_yogur,
    termino_fideos,
    termino_jabon,
    termino_gaseosa,
    termino_mate,
    termino_cerveza,
    #marca,                   
    #`Clasf. Descripción`,
    #USDm2alq_label,                  
    #USDm2venta_label,                
    #Densidad_label,                 
    #Poblacion_label,                 
    #Superficie_label,
    Region,
    #TipoDato.1,
    #TipoDato.2,
    #TipoDato.3,                     
    #TipoDato.4
  )    

#reglas <- apriori(prueba, parameter = list(support=0.01, confidence=0.25, target = "rules"), appearance = list(lhs="VariacionDiscretizadaTotal=VarTotal: Muy Fuerte"))
reglas <- apriori(prueba, parameter = list(support=0.1, confidence=0.25, target = "rules"))
inspect(head(sort(reglas, by="lift", decreasing = TRUE),25))

#write.csv(arrange(as(reglas, "data.frame"), desc(lift)), "reglasZonaTest.csv")
plot(reglas)

#Guardar a CSV
#write.csv(precios.wide, "PreciosWide.csv")
#write.csv(precios.wide.transformados, "PreciosWideTransformados.csv")
#write.csv(precios.wide.transformados.final, "PreciosWideTransformadosFinal.csv")