########################################################################
## Analisis Tickets - server.R
########################################################################

shinyServer(function(input, output, session) {

## reactive para actualizar la lista de grupos de familia
  updListaGrupos <- reactive({
    server_grupo <- file_grupo %>%
      filter(seccion==substr(input$varSec,1,2))
    res1 <- server_grupo$desc
    return(res1)
  })  

    
## reactive para actualizar la lista de familias
  updListaFamilias <- reactive({
    server_familia <- file_familia %>%
      filter(auxGrp==substr(input$varGrp,1,3))
    res2 <- server_familia$desc
    return(res2)
  })  
  
  
  
  ####################################################################### 
  ## Grafico 1 
  ####################################################################### 
  
  output$plot1 <- renderPlot({


      if (substr(input$varFam,1,1) !="<") 
        
      {
          auxGraph1 <- construyeQuery1()
          p <- ggplot(auxGraph1, aes(sbfRelated, sbfAnalize)) + 
            geom_tile(aes(fill= porc), colour = "white") + 
            scale_fill_gradient(low = "white", high = "steelblue") + 
            theme_bw() + geom_text(aes(label=paste(porc)))
          p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=10)) +
            xlab("Ppales categorias complementarias") + ylab("Categorias de la Familia")
      }
  })

  
  
## reactive para la query del Informe1 
  construyeQuery1 <- reactive({
    
    totDf <- NULL
    
    ## seccion, grupo y familia deben estar informados (los tomamos de familia)
    ## si no lo están devuelve NULL

        
    if (substr(input$varFam,1,1) !="<") 
      
    {
    
      ## 1. identificar subfamilias de la familia seleccionada
      
      qrySec <- substr(input$varFam,1,2)
      qryGrp <- substr(input$varFam,3,3)
      qryFam <- substr(input$varFam,4,4)

      #qrySec <- 14
      #qryGrp <- 1
      #qryFam <- 0
      

      ## subfamilias de la Familia seleccionada
      list_subfamilia <- file_subfam %>%
        filter(seccion==qrySec & grupo==qryGrp & familia==qryFam)
      
      ## filtramos para eliminar las que no estan presentes en los tickets
      setkey(file_ticket,categoria)
      list_subfam_filt <- file_ticket %>%
        filter(categoria %in% list_subfamilia$auxsbf) %>%
        group_by(categoria) %>%
        summarise(count=n())
    
      setorder(list_subfam_filt,categoria)

      ## 2. por cada familia (ya filtrada) seleccionada 
      

      for (varCat in list_subfam_filt$categoria) {
        
        #varCat <- 14100
        #### 2.a numero de tickets en los que aparece
        
        sbfTratada<- file_ticket %>%
          filter(categoria==varCat) %>%
          group_by(categoria) %>%
          summarise(count=n())
        
        sbfTratada <- as.data.table(sbfTratada)
        setnames(sbfTratada,c("varCat","tickfam"))

        #### 2.b las otras 10 subfamilias más frecuentes en los tickets en los que aparece (y cuantos son)
        #### y que no sean del mismo grupo de Familia
        #### y que no sean categorías excluidas
    
        # todos los tickets en los q aparece la subfamilia tratada
        setkey(file_ticket,categoria)
        tmp_tickets <- file_ticket %>%
          filter(categoria==varCat) %>%
          select(seqticket)
          

        setkey(file_ticket,seqticket)
        setkey(tmp_tickets,seqticket)
        
        # detalle de todos esos tickets
        tmp_ticket_cat <- merge(tmp_tickets, file_ticket, by = 'seqticket')
        
        setkey(tmp_ticket_cat,categoria)
        setkey(file_freq,categoria)
        
        tmp_ticket_cat <- merge(tmp_ticket_cat, file_freq, by = 'categoria')
        

        # quitamos categorias del mismo grupo de familia que categoria tratada
        tmp_ticket_cat <- tmp_ticket_cat[grupo!=substr(varCat,1,3),]
        # quitamos categorias evidentes (% seleccionado por el usuario) 
        tmp_ticket_cat <- tmp_ticket_cat[freqcum>input$varSlider,]
        
        
        setkey(tmp_ticket_cat, categoria)
        comp_subfamilia <- tmp_ticket_cat %>%
          group_by(categoria) %>%
          summarise(numticketcomp=n())
        
        setorder(comp_subfamilia, -numticketcomp)
        comp_subfamilia <- comp_subfamilia[1:10,]

        if (nrow(comp_subfamilia)>0)
        {
          comp_subfamilia[,subfamilia:=sbfTratada$varCat]
          comp_subfamilia[,totTicket:=sbfTratada$tickfam]
          comp_subfamilia[,porc:=numticketcomp/totTicket]
          comp_subfamilia[,numticketcomp:=NULL]
          comp_subfamilia[,totTicket:=NULL]
          
          setcolorder(comp_subfamilia, c("subfamilia", "categoria", "porc"))
          
          if (is.null(totDf))
          {totDf <- comp_subfamilia}
          else
          {totDf <- do.call(rbind, list(totDf, comp_subfamilia))}
        }
      

        
      }
    
      ### acaba el bucle

      totDf <- merge(totDf,file_subfam,by.x="subfamilia",by.y="auxsbf")
      totDf[,sbfAnalize:=paste(subfamilia,descripm,sep = " ")]
      totDf[,seccion:=NULL]
      totDf[,grupo:=NULL]
      totDf[,familia:=NULL]
      totDf[,subfam:=NULL]
      totDf[,descripm:=NULL]
  
      totDf <- merge(totDf,file_subfam,by.x="categoria",by.y="auxsbf")
      totDf[,sbfRelated:=paste(categoria,descripm,sep = " ")]
      totDf[,categoria:=NULL]
      totDf[,seccion:=NULL]
      totDf[,grupo:=NULL]
      totDf[,familia:=NULL]
      totDf[,subfam:=NULL]
      totDf[,subfamilia:=NULL]
      totDf[,descripm:=NULL]
      totDf$porc <- round((totDf$porc*100),1)
  
    }          

    return(totDf)
        
})  

  
## observe que invoca la actualización de la lista de grupos de familia
observe({
  newListaGrupo <- updListaGrupos()
  updateSelectInput(session, 'varGrp', 'Grupo', c("<selecciona grupo>",as.character(newListaGrupo)),
                    selected="<selecciona grupo>")
})  
  
## observe que invoca la actualización de la lista de familias
  observe({
    newListaFamilia <- updListaFamilias()
    updateSelectInput(session, 'varFam', 'Familia', c("<selecciona familia>",as.character(newListaFamilia)),
                      selected="<selecciona familia>")
})  
  
  
## observe para finalizar la aplicacion
observe({
  if(input$butSalir > 0){
    rm(list=ls())
    stopApp()
    print ("stop!!!")
  }
})




####################################################################### 
## Grafico 2 
####################################################################### 

  output$plot2 <- renderPlot({
    

    if ( (substr(input$varFam,1,1) !="<") & (substr(input$varFam2,1,1) != '<') )
      
    {
      auxGraph2 <- construyeQuery2()
      p2 <- ggplot(auxGraph2, aes(sbfRelated, sbfAnalize)) + 
        geom_tile(aes(fill= porc), colour = "white") + 
        scale_fill_gradient(low = "white", high = "tomato") + 
        theme_bw() + geom_text(aes(label=paste(porc)))
      p2 + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=10)) +
        xlab("Ppales categorias complementarias") + ylab("Categorias de la Familia")
    }
  })
  
  
## reactive para la query del Informe2
  construyeQuery2 <- reactive({
    
    totDf <- NULL
    
    ## seccion, grupo y familia deben estar informados (los tomamos de familia)
    ## familia2 debe estar informada 
    ## si no lo están devuelve NULL
    
    
    if ( (substr(input$varFam,1,1) !="<") & (substr(input$varFam2,1,1) != '<') )
      
    {
      
      qrySec <- substr(input$varFam,1,2)
      qryGrp <- substr(input$varFam,3,3)
      qryFam <- substr(input$varFam,4,4)

      ##qrySec <- 10
      ##qryGrp <- 1
      ##qryFam <- 2

            
      list_subfamilia <- file_subfam %>%
        filter(seccion==qrySec & grupo==qryGrp & familia==qryFam)

                  
      ## 2. por cada familia seleccionada
      
      for (varCat in list_subfamilia$auxsbf) {
        
        ##varCat <- 10120
                
        #### 2.a numero de tickets en los que aparece
        
        sbfTratada<- file_ticket %>%
          filter(categoria==varCat) %>%
          group_by(categoria) %>%
          summarise(count=n())
        
          sbfTratada <- as.data.table(sbfTratada)
          setnames(sbfTratada,c("varCat","tickfam"))
        

          #### 2.b las subfamilias de la Segunda Familia seleccionada
        
          # todos los tickets en los q aparece la subfamilia tratada
          setkey(file_ticket,categoria)
          tmp_tickets <- file_ticket %>%
            filter(categoria==varCat) %>%
            select(seqticket)
        

          setkey(file_ticket,seqticket)
          setkey(tmp_tickets,seqticket)
        
          # detalle de todos esos tickets
          tmp_ticket_cat <- merge(tmp_tickets, file_ticket, by = 'seqticket')
        
          setkey(tmp_ticket_cat,categoria)
          setkey(file_freq,categoria)
        
          tmp_ticket_cat <- merge(tmp_ticket_cat, file_freq, by = 'categoria')
        

          # Solo categorias de la segunda familia seleccionada por el usuario
          tmp_ticket_cat <- tmp_ticket_cat[familia==substr(input$varFam2,1,4),]


          setkey(tmp_ticket_cat, categoria)
          comp_subfamilia <- tmp_ticket_cat %>%
            group_by(categoria) %>%
            summarise(numticketcomp=n())
        

          setorder(comp_subfamilia, -numticketcomp)

          if (nrow(comp_subfamilia)>0)
          {
            comp_subfamilia[,subfamilia:=sbfTratada$varCat]
            comp_subfamilia[,totTicket:=sbfTratada$tickfam]
            comp_subfamilia[,porc:=numticketcomp/totTicket]
            comp_subfamilia[,numticketcomp:=NULL]
            comp_subfamilia[,totTicket:=NULL]
            
            setcolorder(comp_subfamilia, c("subfamilia", "categoria", "porc"))
            
            if (is.null(totDf)) {
              totDf <- comp_subfamilia
              } else {
              totDf <- do.call(rbind, list(totDf, comp_subfamilia))
              }
            
          }
        

      }
      
      ### acaba el bucle
      

      totDf <- merge(totDf,file_subfam,by.x="subfamilia",by.y="auxsbf")
      totDf[,sbfAnalize:=paste(subfamilia,descripm,sep = " ")]
      totDf[,seccion:=NULL]
      totDf[,grupo:=NULL]
      totDf[,familia:=NULL]
      totDf[,subfam:=NULL]
      totDf[,descripm:=NULL]
      
      totDf <- merge(totDf,file_subfam,by.x="categoria",by.y="auxsbf")
      totDf[,sbfRelated:=paste(categoria,descripm,sep = " ")]
      totDf[,categoria:=NULL]
      totDf[,seccion:=NULL]
      totDf[,grupo:=NULL]
      totDf[,familia:=NULL]
      totDf[,subfam:=NULL]
      totDf[,subfamilia:=NULL]
      totDf[,descripm:=NULL]
      totDf$porc <- round((totDf$porc*100),1)
      
    }          
    
    return(totDf)
    
    
  })  

  
  
  ####################################################################### 
  ## Grafico 3
  ####################################################################### 
  
  output$plot3 <- renderPlot({
    
    
    if (substr(input$varCli,1,1) !="<") 
      
    {
      auxGraph3 <- construyeQuery3()
      
      auxGraph3[,vtaCliente:=ifelse(vtaCliente==0,"Proponer","Es consumidor")]	
      
      p <- ggplot(auxGraph3, aes(sbfRelated, sbfAnalize)) + 
        geom_tile(aes(fill= vtaCliente), colour = "white") + 
        scale_colour_manual(values=c('Proponer'='red','Es consumidor'='green4')) + theme_bw() +
        xlab("Ppales categorias complementarias") + ylab("Categorias Top del Cliente")
      
      p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=10))
    }
  })
  
  
  
  ## reactive para la query del Informe3
  construyeQuery3 <- reactive({
    
    totDf <- NULL
    
    ## seccion, grupo y familia deben estar informados (los tomamos de familia)
    ## si no lo están devuelve NULL
    
    
    ## 1. identificar Top25 subfamilias del cliente seleccionado
    
    #qrySec <- 24
    #qryGrp <- 1
    #qryFam <- 0
    
    setkey(file_cliente, cliente)
    catCliente <- file_cliente %>%
      filter(cliente==input$varCli)
    
    setorder(catCliente,-aux)
    
    
        
    ## Top5 subfamilias del Cliente seleccionado
    list_subfam <- catCliente[1:5,categoria]


    ## 2. por cada familia (ya filtrada) seleccionada 
    
    
    for (varCat in list_subfam) {
      

      #### 2.a numero de tickets en los que aparece
      
      sbfTratada<- file_ticket %>%
        filter(categoria==varCat) %>%
        group_by(categoria) %>%
        summarise(count=n())
      
      sbfTratada <- as.data.table(sbfTratada)
      setnames(sbfTratada,c("varCat","tickfam"))
      
      #### 2.b las otras 10 subfamilias más frecuentes en los tickets en los que aparece (y cuantos son)
      #### y que no sean del mismo grupo de Familia
      #### y que no sean categorías excluidas
      
      # todos los tickets en los q aparece la subfamilia tratada
      setkey(file_ticket,categoria)
      tmp_tickets <- file_ticket %>%
        filter(categoria==varCat) %>%
        select(seqticket)
      
      setkey(file_ticket,seqticket)
      setkey(tmp_tickets,seqticket)
      
      # detalle de todos esos tickets
      tmp_ticket_cat <- merge(tmp_tickets, file_ticket, by = 'seqticket')
      
      setkey(tmp_ticket_cat,categoria)
      setkey(file_freq,categoria)
      
      tmp_ticket_cat <- merge(tmp_ticket_cat, file_freq, by = 'categoria')
      

      # quitamos categorias de la misma familia que categoria tratada
      tmp_ticket_cat <- tmp_ticket_cat[grupo!=substr(varCat,1,3),]
      # quitamos categorias evidentes (>40 presencia tickets) 
      tmp_ticket_cat <- tmp_ticket_cat[freqcum>40,]
      
      
      setkey(tmp_ticket_cat, categoria)
      comp_subfamilia <- tmp_ticket_cat %>%
        group_by(categoria) %>%
        summarise(numticketcomp=n())
      
      setorder(comp_subfamilia, -numticketcomp)
      comp_subfamilia <- comp_subfamilia[1:5,]

      comp_subfamilia[,vtaCliente:=0]
            

      if (nrow(comp_subfamilia)>0)
      {
        
        for (i in (1:nrow(comp_subfamilia))) {
          if ( comp_subfamilia[i,categoria] %in% catCliente$categoria) { 
            comp_subfamilia[i,vtaCliente:=1]  }
        }

        comp_subfamilia[,subfamilia:=sbfTratada$varCat]
        comp_subfamilia[,totTicket:=sbfTratada$tickfam]
        comp_subfamilia[,porc:=numticketcomp/totTicket]
        comp_subfamilia[,numticketcomp:=NULL]
        comp_subfamilia[,totTicket:=NULL]
        

        setcolorder(comp_subfamilia, c("subfamilia", "categoria", "porc", "vtaCliente"))
        
        if (is.null(totDf))
        {totDf <- comp_subfamilia}
        else
        {totDf <- do.call(rbind, list(totDf, comp_subfamilia))}
      }
      
      
      
    }
    
      ### acaba el bucle
    
      totDf <- merge(totDf,file_subfam,by.x="subfamilia",by.y="auxsbf")
      totDf[,sbfAnalize:=paste(subfamilia,descripm,sep = " ")]
      totDf[,seccion:=NULL]
      totDf[,grupo:=NULL]
      totDf[,familia:=NULL]
      totDf[,subfam:=NULL]
      totDf[,descripm:=NULL]
      
      totDf <- merge(totDf,file_subfam,by.x="categoria",by.y="auxsbf")
      totDf[,sbfRelated:=paste(categoria,descripm,sep = " ")]
      totDf[,categoria:=NULL]
      totDf[,seccion:=NULL]
      totDf[,grupo:=NULL]
      totDf[,familia:=NULL]
      totDf[,subfam:=NULL]
      totDf[,subfamilia:=NULL]
      totDf[,descripm:=NULL]
      totDf$porc <- round((totDf$porc*100),1)
      

    return(totDf)
    
  })  
  
  


  ####################################################################### 
  ## Grafico 4 
  ####################################################################### 
  
  output$plot4 <- renderPlot({
    
    setorder(file_freq,freqcum)
    p4 <- plot(file_freq$freqcum)  
    p4 
    abline(h=input$varSlider,col="red")

  })
  
  

    
})  

