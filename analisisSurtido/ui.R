########################################################################
## Analisis Surtido - ui.R
########################################################################


shinyUI(fluidPage(
  
  # Titulo
  titlePanel("Análisis Venta y Rentabilidad Surtido PGC Express"),

  # cabecera
  fluidRow(
    column(3, h3("Concepto Comercial Proximidad")),
    column(3, selectInput('varReg', 'Region', 
                             c("Todos",list_region$descripm),selected="Todos")), 
    column(3, selectInput('varRol', 'Rol', 
                             c("Todos",list_rol$descripm),selected="Todos")), 
    column(3, selectInput('varSto', 'Tienda', c("Todos")))
    ), 
  sidebarLayout(
    sidebarPanel(width=3, 
      selectInput('varSec', 'Seccion', c("Todos",list_seccion),  multiple = FALSE),
      selectInput('varGrp', 'Grupo', c("Todos"),  multiple = FALSE),
      selectInput('varFam', 'Familia',  c("Todos")),
      selectInput('varNivs', 'Niv.Surtido', choices = c("Todos",list_nivsurt),  multiple = FALSE),
      selectInput('varProv', 'Proveedor', choices = c("Todos",list_prov),  multiple = FALSE),
      selectInput('varAtr', 'Atrib.Mkt', 
                     c("Todos",maestro_atributo$atributo),selected="Todos"),
      hr(),
      actionButton('butSalir', label = 'Finalizar')
         ), 
    mainPanel(width=9, 
       tabsetPanel(
         tabPanel("Indic.Articulo", DT::dataTableOutput("table1")), 
         tabPanel("Disp.Articulo x VtaMed/Margen", 
              fluidRow(numericInput('varMinSto', 'Artic en + de n tiendas', 0, min = 0)),
              fluidRow(checkboxInput('varExcOut', 'Excluir Margenes extremos', value = FALSE, width = NULL)),
              fluidRow(plotOutput("plot3"))
              ),
         tabPanel("Presencia Categ x Reg/Rol", 
                  radioButtons('radioGraph', 'Datos por ...', choices = list("Region" = 1, "Rol" = 2), 
                               selected = 1, inline = TRUE),
                  fluidRow(plotOutput("plot4"))), 
         tabPanel("Disp.Tiendas x PesoVta/Margen",plotOutput("plot5"))
            )
        )
    )
  )
)
