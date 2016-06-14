########################################################################
## Analisis Tickets - ui.R
########################################################################


shinyUI(fluidPage(
  titlePanel("Análisis Tickets"),
  sidebarLayout(
    sidebarPanel(width=3, 
      selectInput('varSec', 'Seccion', choices = file_seccion$desc, multiple = FALSE),
      selectInput('varGrp', 'Grupo', c('<selecciona grupo>'), multiple = FALSE),
      selectInput('varFam', 'Familia', c('<selecciona familia>'), multiple = FALSE),
      hr(),
      hr(),            
      numericInput('varSlider', 'Excluir categorias que suponen el % de la venta', min = 0, max = 100, value = 0),
      hr(),       
      actionButton('butSalir', label = 'Finalizar') ), 
    mainPanel(width=9, 
       tabsetPanel(
         tabPanel("Top10 total relaciones por Subfamilia", plotOutput("plot1")),
         tabPanel(("Comparativa vs Familia"), 
              fluidRow(selectInput('varFam2', 'Familia', c('<selecciona familia a comparar>',as.character(file_familia2$desc)), 
                                   multiple = FALSE)),
              fluidRow(plotOutput("plot2"))), 
         tabPanel(("Analisis Cliente"), 
                  fluidRow(selectInput('varCli', 'Cliente', c('<selecciona cliente a analizar>',list_cliente$cliente), 
                                       multiple = FALSE)),
                  fluidRow(plotOutput("plot3"))),          
         tabPanel("Categorias Evidentes", plotOutput("plot4"))
            )
        )
    )
  )
)

