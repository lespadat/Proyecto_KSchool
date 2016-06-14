########################################################################
## Analisis Surtido - server.R
########################################################################

shinyServer(function(input, output, session) {

## reactive para actualizar la lista de tiendas
  updListaTiendas <- reactive({
    if (input$varReg=="Todos") 
      { server_tienda <- list_tienda}
    else
      { server_tienda <- list_tienda %>%
        filter(region==input$varReg) }
    
    if (input$varRol!="Todos") 
      { server_tienda <- server_tienda %>%
        filter(rol==input$varRol) }

    res <- server_tienda$auxTienda
    return(res)
  })


## reactive para actualizar la lista de grupos de familia
  updListaGrupos <- reactive({
    server_grupo <- list_grupo %>%
      filter(auxSec==substr(input$varSec,1,2))
    res1 <- server_grupo$grupo
    return(res1)
  })  

    
## reactive para actualizar la lista de familias
  updListaFamilias <- reactive({
    server_familia <- list_familia %>%
      filter(auxGrp==substr(input$varGrp,1,3))
    res2 <- server_familia$familia
    return(res2)
  })  



###############################################################################################
## reactive para la query principal
###############################################################################################
  construyeQuery <- reactive({

    tiendaList <- list_tienda %>%
      select(tienda, region, rol, tarifa_pvp, auxTienda)
    
    if (input$varReg!="Todos") 
      { tiendaList <- tiendaList %>%
          filter(region==input$varReg) }
    
    if (input$varRol!="Todos") 
      { tiendaList <- tiendaList %>%
          filter(rol==input$varRol) }
    
    if (input$varSto!="Todos") 
      { tiendaList <- tiendaList %>%
        filter(auxTienda==input$varSto) }    

        
    soloCodTienda <- tiendaList$tienda
    
    ventaList <- file_venta %>%
      filter(tienda %in% soloCodTienda) %>%
      group_by(ncodartic, descripm, seccion, grupofam, familia, ambsurti, ncodprove, mgest ) %>%
      summarise(impventa=sum(impventa),numtda=n())    
    
    ventaList <- as.data.table(ventaList)
    ventaList[,auxGr:=(seccion*10)+grupofam]
    ventaList[,auxFam:=(auxGr*10)+familia]
    

## evaluamos si se ha seleccionado seccion, grupo, familia y NivSurt 

    if (input$varSec!="Todos") 
    { ventaList <- ventaList %>%
      filter(seccion==substr(input$varSec,1,2)) }    

    if (input$varGrp!="Todos") 
      { ventaList <- ventaList %>%
          filter(auxGr==substr(input$varGrp,1,3)) }    
    
    if (input$varFam!="Todos") 
      { ventaList <- ventaList %>%
        filter(auxFam==substr(input$varFam,1,4)) }    

    if (input$varNivs!="Todos") 
      { ventaList <- ventaList %>%
        filter(ambsurti==substr(input$varNivs,1,2)) }    
    
    
## filtramos articulos si se ha seleccionado AtributoMarketing    
    
    
    if (input$varAtr!="Todos") 
      { auxAtrib <- maestro_atributo[,maestro_atributo[atributo==input$varAtr]] 
        valueAtrib <- auxAtrib$atrib
        
        auxItem <- list_atributo[,list_atributo[atrib==valueAtrib]]
        listItem <- auxItem$sms
        
        ventaList <- ventaList[,ventaList[ncodartic %in% listItem]]
      }

    
## filtramos articulos si se ha seleccionado Proveedor 
    
    if (input$varProv!="Todos") 
      { ventaList <- ventaList %>%
        filter(ncodprove==input$varProv) }       
    

## si hay datos ordeno x venta descendente y calculo venta media, rentabilidad neta y peso acumulado
  
  if ( nrow(ventaList) > 0 ) {
    
    ventaList <- ventaList[order(-impventa),]
    ventaList[,vMedia:=round((impventa/numtda),2)]
    ventaList[,rent:=round(((impventa/100)*mgest),2)]
    
    pesoTot <- sum(ventaList$impventa) 
    ventaList[,pesoInd:=100*round((impventa/pesoTot),4)]
    ventaList[,pAcum:=cumsum(pesoInd)]
    ventaList[,pesoInd:=NULL]    
    
  }
  

    ventaList <- ventaList[, c(1,2,6,7,8,9,10,13,14,15), with=FALSE]
    
    setnames(ventaList, c("cod","articulo","nivel","prov","mg","vtaTot","tienda","vtaMed","rent","pesoAcum"))
    
    return(ventaList)
  })  
  
  
###############################################################################################
## reactive para dispersion articulo por venta Media y Margen
###############################################################################################
  dispersionItem <- reactive({
    
    listDispItem <- construyeQuery()
    listDispItem <- as.data.table(listDispItem)

    # valores extremos: desviacion Mg articulo mas de 3 veces superior a la desv tipica 
    mediaMg <- mean(listDispItem$mg)        
    desvTipicaMg <- sd(listDispItem$mg)

    limiteInf <- mediaMg - (3*desvTipicaMg)
    limiteSup <- mediaMg + (3*desvTipicaMg)    

    if (input$varExcOut) {
      listDispItem <- listDispItem[(mg>limiteInf)&(mg<limiteSup),]
    }
    
    return(listDispItem)
  }) 
  
###############################################################################################
## reactive para la query de presencia categ media tienda 
###############################################################################################

    construyeQueryCateg <- reactive({

    ventaItemSto <- file_venta 
    
    ## evaluamos si se ha seleccionado seccion
    
    if (input$varSec!="Todos") 
    { ventaItemSto <- ventaItemSto %>%
      filter(seccion==substr(input$varSec,1,2)) }    
    
    ## evaluamos si se ha seleccionado grupo
    
    if (input$varGrp!="Todos") 
    { ventaItemSto <- ventaItemSto %>%
      filter(grupofam==substr(input$varGrp,3,3)) }    
    
    ## evaluamos si se ha seleccionado familia 
    
    if (input$varFam!="Todos") 
      { ventaItemSto <- ventaItemSto %>%
        filter(familia==substr(input$varFam,4,4)) }          

    
    if (input$varProv!="Todos") 
      { ventaItemSto <- ventaItemSto %>%
        filter(ncodprove==input$varProv) }  
    
        
    ventaItemSto <- ventaItemSto[, c(1,2,3,4), with=FALSE]        
    setkey(ventaItemSto, tienda)

    ventaItemSto <- merge(ventaItemSto, list_tienda, by = 'tienda')


    ## filtramos articulos si se ha seleccionado AtributoMarketing    
    
    
    if (input$varAtr!="Todos") 
    { auxAtrib <- maestro_atributo[,maestro_atributo[atributo==input$varAtr]] 
    valueAtrib <- auxAtrib$atrib
    
    auxItem <- list_atributo[,list_atributo[atrib==valueAtrib]]
    listItem <- auxItem$sms
    
    ventaItemSto <- ventaItemSto[,ventaItemSto[ncodartic %in% listItem]]
    }
    

    # agrupamos venta y margen (ponderado) x Tienda
    ventaItemStoGroup <- ventaItemSto %>% 
      group_by(tienda, region, rol, tarifa_pvp) %>%
      summarise(totvta = sum(impventa), totmg = (sum(impventa*mgadap)/sum(impventa)))

    rm(ventaItemSto)

    ventaItemStoGroup$totmg <- round(ventaItemStoGroup$totmg, 2)
    

    setkey(ventaItemStoGroup,tienda)
    setkey(vtaStore,tienda)
    
    ventaItemStoGroup <- merge(ventaItemStoGroup, vtaStore, by = 'tienda')
    ventaItemStoGroup[,peso:=round((totvta/totventa),8)]
    
    ventaItemStoGroup <- ventaItemStoGroup %>% 
      select(tienda, region.x, rol.x, peso)
    
    setnames(ventaItemStoGroup,c('tienda', 'region', 'rol', 'peso'))
    
    return(ventaItemStoGroup)
  })  
  
  

  
  
  ###############################################################################################
  ## reactive para la query del grafica de dispersion de tiendas x PesoVenta y Margen
  ###############################################################################################
  
  construyeQueryDispSto <- reactive({
    
    tiendaList <- list_tienda %>%
      select(tienda, region, rol, tarifa_pvp, auxTienda)
    
    if (input$varReg!="Todos") 
    { tiendaList <- tiendaList %>%
      filter(region==input$varReg) }
    
    if (input$varRol!="Todos") 
    { tiendaList <- tiendaList %>%
      filter(rol==input$varRol) }
    
    soloCodTienda <- tiendaList$tienda
    
    
    ventaList <- file_venta %>%
      filter(tienda %in% soloCodTienda) %>%
      select(ncodartic, tienda, impventa, seccion, grupofam, familia, ambsurti, ncodprove, mgadap ) 
    
    ventaList <- as.data.table(ventaList)
    ventaList[,auxGr:=(seccion*10)+grupofam]
    ventaList[,auxFam:=(auxGr*10)+familia]

        
    ## evaluamos si se ha seleccionado seccion, grupo, familia 
    
    if (input$varSec!="Todos") 
    { ventaList <- ventaList %>%
      filter(seccion==substr(input$varSec,1,2)) }    
    
    if (input$varGrp!="Todos") 
    { ventaList <- ventaList %>%
      filter(auxGr==substr(input$varGrp,1,3)) }    
    
    if (input$varFam!="Todos") 
    { ventaList <- ventaList %>%
      filter(auxFam==substr(input$varFam,1,4)) }    
    
    if (input$varProv!="Todos") 
    { ventaList <- ventaList %>%
      filter(ncodprove==input$varProv) }    
    
    ## filtramos articulos si se ha seleccionado AtributoMarketing    
    
    if (input$varAtr!="Todos") 
      { auxAtrib <- maestro_atributo[,maestro_atributo[atributo==input$varAtr]] 
        valueAtrib <- auxAtrib$atrib
    
        auxItem <- list_atributo[,list_atributo[atrib==valueAtrib]]
        listItem <- auxItem$sms
    
        ventaList <- ventaList[,ventaList[ncodartic %in% listItem]]
      }
    

    # agrupamos venta y margen (ponderado) x Tienda
    ventaDispStoGroup <- ventaList %>% 
      group_by(tienda) %>%
      summarise(totvta = sum(impventa), totmg = (sum(impventa*mgadap)/sum(impventa)))

    ventaDispStoGroup <- as.data.table(ventaDispStoGroup)
        
    rm(ventaList)
    
    ventaDispStoGroup$totmg <- round(ventaDispStoGroup$totmg, 2)
    
    # join con total tienda para llevar importe vta a %

    setkey(ventaDispStoGroup,tienda)
    setkey(vtaStore,tienda)
    
    ventaDispStoGroup <- merge(ventaDispStoGroup, vtaStore, by = 'tienda')
    ventaDispStoGroup <- as.data.table(ventaDispStoGroup)
    
    ventaDispStoGroup[,peso:=round((totvta/totventa),8)]

 
    return(ventaDispStoGroup)
  })  
  
  
  
  
  
## observe que invoca la actualización de la lista de tiendas
  observe({
    newListaTienda <- updListaTiendas()
    updateSelectInput(session, 'varSto', 'Tienda', c("Todos",as.character(newListaTienda)),selected="Todos")
  })

  
## observe que invoca la actualización de la lista de grupos de familia
observe({
  newListaGrupo <- updListaGrupos()
  updateSelectInput(session, 'varGrp', 'Grupo', c("Todos",as.character(newListaGrupo)),selected="Todos")
})  


## observe que invoca la actualización de la lista de familias
  observe({
    newListaFamilia <- updListaFamilias()
    updateSelectInput(session, 'varFam', 'Familia', c("Todos",as.character(newListaFamilia)),selected="Todos")
  })  
  

## observe para finalizar la aplicacion
  observe({
    if(input$butSalir > 0){
      rm(list=ls())
      stopApp()
      print ("stop!!!")
    }
  })

    
#########################################################################################
## Pestaña_1, Indicadores Top
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    dataShowTable <- construyeQuery()
    dataShowTable
    
  },options = list(pageLength = 25, searching = TRUE)))


#########################################################################################
## Pestaña_3, Dispersion articulos x VtaMedia y Margen
  
  output$plot3 <- renderPlot({
    
    auxNube <- dispersionItem()
    
    auxNube <- auxNube[tienda>input$varMinSto,]

    medianMg2 <- median(auxNube$mg)        
    desvTipicaMg2 <- sd(auxNube$mg)
    limiteInf2 <- medianMg2 - (3*desvTipicaMg2)
    limiteSup2 <- medianMg2 + (3*desvTipicaMg2)    
    abline(h=limiteSup2)
    abline(h=limiteInf2)    

    titulo <- "Dispersion articulos por VtaMedia y MargenCaja"
            
    if (input$varExcOut) { 
      p <- ggplot(auxNube, aes(x=vtaMed, y=mg, colour = nivel))
      p + geom_point() + geom_text(aes(label = cod), hjust = -0.2, vjust = 0.5) + ggtitle(titulo) +
        theme_bw() + xlab("Venta Media") + ylab("Margen Caja") } else {
      p <- ggplot(auxNube, aes(x=vtaMed, y=mg, colour = nivel))
      p + geom_point() + geom_text(aes(label = cod), hjust = -0.2, vjust = 0.5) + ggtitle(titulo) +
        theme_bw() + xlab("Venta Media") + ylab("Margen Caja") + 
        geom_hline(aes(yintercept=limiteInf2)) + geom_hline(aes(yintercept=limiteSup2)) 
      }                

  })  


#########################################################################################
## Pestaña_4, presencia Cateoria-Tienda x Region/Rol 
  
  output$plot4 <- renderPlot({
    
    totalesCateg <- construyeQueryCateg()
    
    if (input$radioGraph==1) {
      graphCateg <- totalesCateg[, c(1,2,4), with=FALSE]
      titulo4 <- " Peso venta Categoria por Region"
      q4 <- ggplot(graphCateg, aes(region, peso)) 
      q4 + geom_boxplot() + ggtitle(titulo4) + ylab("Peso total PGC")
      } else {
        graphCateg <- totalesCateg[, c(1,3,4), with=FALSE]
        titulo4 <- " Peso venta Categoria por Rol"
        q4 <- ggplot(graphCateg, aes(rol, peso)) 
        q4 + geom_boxplot() + ggtitle(titulo4) + ylab("Peso total PGC")
      } 

  })  

#########################################################################################  
## Pestaña_5, Dispersion tiendas x PesoVenta y Margen
  
  output$plot5 <- renderPlot({
    
    nubeTienda <- construyeQueryDispSto()    
    
    titulo5 <- "Dispersion Tiendas por PesoVenta y MargenCaja"
    p <- ggplot(nubeTienda, aes(x=peso, y=totmg, colour = tarifa_pvp))
    p + geom_point() + geom_text(aes(label = tienda), hjust = -0.2, vjust = 0.5) + ggtitle(titulo5) +
      theme_bw() + xlab("Peso Categoria sobre total PGC") + ylab("Margen Caja")       
      

  })  

  
})
