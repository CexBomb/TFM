library(shiny)
library(leaflet)
library(data.table)
library(shinyjs)

shinyUI(navbarPage(
  useShinyjs(),
  "Locales en Madrid",id = "nav",
  tabPanel(
    "Mapa de locales",div(
      class = "outer",tags$head(includeCSS('styles.css')),
      leafletOutput("mapdist",width = "100%",height = "100%"),
      
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "100%",
        
        h2("Seleccione opcion"),
        
        selectizeInput("epig", "Epigrafes", choices=NULL),
        selectizeInput("dist", "Distrito", choices=NULL),
        conditionalPanel(
          condition = "input.dist != '' & input.epig != ''",
          checkboxInput("distptos", "Mostrar  puntos mas distantes", value = FALSE),
          uiOutput("controlptos")),
        plotOutput("histpoblacion", height = 200),
        conditionalPanel(
          condition = "input.epig != ''",
          tabsetPanel(
            tabPanel("Locales / distrito",DT::dataTableOutput("resultmaxlocales")),
            tabPanel("Habitantes / local", DT::dataTableOutput("resultppl"))
          )
        )
      )
    )
  ),
  tabPanel(
    "Mapa de ingresos",div(
      class = "outer",tags$head(includeCSS('styles.css')),
      leafletOutput("mapcp",width = "100%",height = "100%"),
      
      absolutePanel(
        id = "controlscp", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "100%",
        
        h2("Seleccione opcion"),
        
        #selectInput("opcion","Elija opcion", choices = NULL),
        selectizeInput("cat", "Categoria", choices=NULL),
        selectizeInput("subcat", "Subcategoria", choices=NULL),
        
        conditionalPanel(
          condition = "input.subcat != ''",
          tabsetPanel(
            tabPanel("Ventas / CP",DT::dataTableOutput("resultcp")),
            tabPanel("CP / desplazamiento", DT::dataTableOutput("resultdispl"))
          )
        )
      )
    )
  ),
  
  tabPanel("Datos de interes",
            tabsetPanel(
            tabPanel("Datos de poblacion",plotOutput("poblacion", width = 1200, height = 1200)),
            tabPanel("Locales por distrito",DT::dataTableOutput("localesdistrito")),
            tabPanel("Habitantes por local", DT::dataTableOutput("localeshabitante")),
            tabPanel("Gasto por codigo postal", DT::dataTableOutput("gastocp")),
            tabPanel("Desplazamiento por codigo postal", DT::dataTableOutput("desplazamientocp"))
           )
  )
))
