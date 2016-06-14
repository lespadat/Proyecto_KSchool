########################################################################
## Analisis Surtido - global.R
########################################################################
library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)


####################################################################### 
# maestro tiendas
####################################################################### 
list_tienda <- fread("p1_tienda.csv")


####################################################################### 
# regiones
####################################################################### 
list_region <- unique(list_tienda$region)
list_region <- as.data.table(list_region)
setnames(list_region, "descripm")
list_region <- list_region[order(list_region$descripm), ]


####################################################################### 
# roles tienda
####################################################################### 
list_rol <- unique(list_tienda$rol)
list_rol <- as.data.table(list_rol)
setnames(list_rol, "descripm")
list_rol <- list_rol[order(list_rol$descripm), ]


####################################################################### 
# seccion
####################################################################### 
list_seccion <- fread("p1_seccion.csv")
list_seccion <- list_seccion[sector==1,]
list_seccion[,auxSec:=paste(seccion,descripm, sep = " ")]
list_seccion <- list_seccion[,c(4), with=FALSE]
setnames(list_seccion, "seccion")


####################################################################### 
# grupo 
####################################################################### 
list_grupo <- fread("p1_grupo.csv")
list_grupo <- list_grupo[seccion < 16,]
list_grupo[,auxGr:=paste(auxgrp,descripm, sep = " ")]
list_grupo <- list_grupo[,c(5), with=FALSE]
setnames(list_grupo, "grupo")


####################################################################### 
# familia 
####################################################################### 
list_familia <- fread("p1_familia.csv")
list_familia <- list_familia[seccion < 16,]
list_familia[,auxFm:=paste(auxfam,descripm, sep = " ")]
list_familia <- list_familia[,c(6), with=FALSE]
setnames(list_familia, "familia")


#######################################################################
# venta articulo - tienda
####################################################################### 
file_venta <- fread("p1_ventames.csv")


####################################################################### 
# maestro articulos
####################################################################### 
file_artic <- fread("p1_articulo.csv")
auxVtaMd <- max(file_artic$vtamedia)


## proveedores
list_prov <- unique(file_artic$ncodprove)
list_prov <- as.data.table(list_prov)
setnames(list_prov,'ncodprove')
setorder(list_prov,'ncodprove')





####################################################################### 
setkey(file_venta, ncodartic)
setkey(file_artic, ncodartic)
file_venta <- merge(file_venta, file_artic, by = 'ncodartic')



####################################################################### 
# venta tienda
####################################################################### 
vtaStore <- file_venta %>%
  group_by(tienda) %>%
  summarise(totventa=sum(impventa))

vtaStore <- merge(vtaStore, list_tienda, by = 'tienda')


####################################################################### 
list_tienda[,auxTienda:=paste(tienda,nombre,sep=" ")]
list_tienda <- list_tienda[order(auxTienda),]

list_grupo [, auxSec:=substr(grupo,1,2)]

list_familia [, auxGrp:=substr(familia,1,3)]


####################################################################### 
list_atributo <- fread("p1_Express_Atrib.csv")
list_atributo <- list_atributo[,aux:=1]
list_atributo <- list_atributo[order(list_atributo$atributo), ]

maestro_atributo <- list_atributo %>%
  group_by(atrib, atributo) %>%
  summarise(totItem=sum(aux))


####################################################################### 
list_nivsurt <-  c("N8","N7","N6","N5","N4","N3","NM")

####################################################################### 



