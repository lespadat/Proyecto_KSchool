########################################################################
## Analisis Tickets - global.R
########################################################################
library(data.table)
library(ggplot2)
library(dplyr)


####################################################################### 
# Subfamilia 
####################################################################### 
file_subfam <- fread("p2_subfamilia.csv")


####################################################################### 
# Familia 
####################################################################### 
file_familia <- fread("p2_familia.csv")

file_familia$seccion <- as.numeric(file_familia$seccion)
file_familia$grupo <- as.numeric(file_familia$grupo)
file_familia [,auxGrp:=(seccion*10)+grupo]
file_familia [,desc:=paste(auxfam,descripm,sep = " ")]

file_familia2 <- file_familia

####################################################################### 
# Grupo de Familia 
####################################################################### 
file_grupo <- fread("p2_grupo.csv")
file_grupo [,desc:=paste(auxgrp,descripm,sep = " ")]


####################################################################### 
# Seccion
####################################################################### 
file_seccion <- fread("p2_seccion.csv")
file_seccion [,desc:=paste(seccion,descripm,sep = " ")]


####################################################################### 
# Tickets
####################################################################### 
file_ticket <- fread("p2_ticket.csv")

file_ticket[,familia:=substr(categoria,1,4)]
file_ticket[,grupo:=substr(familia,1,3)]
file_ticket[,seccion:=substr(familia,1,2)]

# elimino categorias venta seccion
file_ticket <- file_ticket[substr(categoria,3,5)!='999',]


# elimino categorias Electro, Cultura, Bolsas, ...
file_ticket$categoria <- as.numeric(file_ticket$categoria)
file_ticket$familia <- as.numeric(file_ticket$familia)
file_ticket$seccion <- as.numeric(file_ticket$seccion)

file_ticket <- file_ticket[categoria<36000,]
file_ticket <- file_ticket[seccion!=32,]
file_ticket <- file_ticket[familia!=3151,]

totTicket <- nrow(file_ticket)


####################################################################### 
# frecuencia con la que aparece una categoria en los tickets
####################################################################### 
file_freq <- file_ticket %>%
  group_by(categoria) %>%
  summarise(totticketcat = n())

file_freq[,frequence:=round((100*(totticketcat/totTicket)),4)]
file_freq <- setorder(file_freq,-totticketcat)

file_freq[,freqcum:=cumsum(frequence)]

file_freq <- file_freq %>%
  select(categoria,frequence,freqcum)

setorder(file_freq, freqcum)


####################################################################### 
# Para analisis clientes: 
# genero grupo clientes con diversidad de categorias en sus compras
# (hay 5 o mas categorias (no evidentes) que han comprado en 2 o mas ocasiones)
#######################################################################
setkey(file_ticket,cliente,categoria)
file_cliente <- file_ticket %>%
  group_by(cliente,categoria) %>%
  summarise(aux = n())

catNoEvidentes <- file_freq %>%
  filter(freqcum>40)

setkey(file_cliente,categoria)
file_cliente <- file_cliente %>%
  filter((categoria %in% catNoEvidentes$categoria)&(aux>=2))

aux_cliente <- file_cliente %>%
  group_by(cliente) %>%
  summarise(aux = n())

aux_cliente <- aux_cliente %>%
  filter(aux>=5)

file_cliente <- file_cliente %>%
  filter(cliente %in% aux_cliente$cliente)


list_cliente <- as.character(unique(file_cliente$cliente))
list_cliente <- as.data.table(list_cliente)
setnames(list_cliente,'cliente')
setorder(list_cliente,cliente)
