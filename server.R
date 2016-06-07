library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(rgdal)
library(deldir)
library(FNN)
library(httr)
library(jsonlite)
library(gdata)
library(cluster)

#setwd("/Users/Cesc/Downloads/Master Data Science KSchool/4 - R/Proyecto/")
#-- Cargamos las tablas de los locales, poblacion, renta y lista de Codigos Postales
dtlocales <- fread("Data/dtlocales.csv")
dtpoblacion <- fread("Data/poblacion.csv")
renta <- fread("Data/renta.csv")

#-- Creamos una tabla con el total de habitantes por distrito
habitantesdistrito <- dtpoblacion %>%
  group_by(desc_distrito) %>%
  summarise(total = sum(as.integer(espanoleshombres)) + sum(as.integer(extranjeroshombres)) + sum(as.integer(espanolesmujeres)) + sum(as.integer(extranjerosmujeres)))
#-- Creamos una tabla con el total de locales por distrito
localpordist <- dtlocales %>%
  group_by(nombre_distrito,desc_epigrafe) %>%
  summarise(total = length(id_local))

#-- Leemos las categorias cargadas de la API del BBVA
catdf <- fread("Data/categorias.csv")
#-- Leemos las estadísticas básicas de las subcategorias por CP de la API del BBVA, 
#-- eliminando las que esten vacías
# cpbasicstats <- fread("Data/cpbasicstats.csv")
# cpbasicstats <- cpbasicstats %>% filter(!is.na(amountavg) & amountavg!=0)
#-- Leemos los datos de los top CP leidos de la API del BBVA
top100stats <- fread("Data/top100stats.csv")
setnames(top100stats, "label", "cp_origen")
#-- Agrupamos los datos para obtener el total de ingresos por subcategoria y CP
subcattopCP <- top100stats %>% 
  group_by(code_subcategoria,subcategoria,cp) %>%
  dplyr::summarise(total = round(sum(as.numeric(incomes)) / 1000,2) )
#-- Agrupamos los datos por categoria y CP de origen y destino
all.stats.per.cat.and.origen  <-
  top100stats %>% group_by(code_subcategoria,subcategoria,cp_origen,cp) %>% 
  summarise(total = round(sum(as.numeric(incomes)) / 1000,2)  )
#-- Guardamos en una lista los valores de los CP de Madrid mas el de Tres Cantos
listacp <- c(28001:28055,28790)

#-- Leemos el shapefile con los distritos de Madrid
shape <-
  readOGR("Data/Distritos/","200001494", encoding = "ISO-8859-1", verbose = FALSE)
#-- Nos quedamos solo con Madrid capital
madshape <- shape[nchar(as.character(shape$DESBDT)) > 6,]
#-- Nos quedamos solo con los nombres de los distritos
madshape$DESBDT <-
  substr(as.character(unlist(madshape$DESBDT)),8,nchar(as.character(unlist(madshape$DESBDT))))
#-- Transformamos las coordenadas para pasralas a leaflet
madshape <-
  spTransform(madshape, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#-- Añadimos los datos de la renta por habitante
madshape@data$renta <- renta$y2012
madshape@data$DESBDT <- toupper(madshape$DESBDT)
madshape@data$DESBDT <- gsub("Í","I",madshape$DESBDT)
madshape@data$DESBDT <- gsub("Á","A",madshape$DESBDT)

#-- Leemos el shapefile de los codigos postales
shape <-
  readOGR("Data/Codigos_Postales","200001489", encoding = "ISO-8859-1", verbose = FALSE)
#-- Transformamos las coordenadas para pasralas a leaflet
cpshape <- subset(shape,DESBDT %in% listacp)
cpshape <-
  spTransform(cpshape, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#eleccion <- c("Todos los negocios","Solo locales comerciales")
epigrafes <- unique(dtlocales$desc_epigrafe)
distritos <- unique(dtlocales$nombre_distrito)
categorias <- unique(catdf$cat_description)

pal <- colorNumeric(palette = "Reds",
                    domain = madshape$renta)

shinyServer(function(input, output, session) {
  #-- Cargamos los epigrafes y los distritos desde el servidor.
  #-- De esta manera se ejecuta mas rapido
  updateSelectizeInput(session, "epig",choices = epigrafes, server = TRUE)
  updateSelectizeInput(session, "dist",choices = distritos, server = TRUE)
  updateSelectizeInput(session, "cat",choices = categorias, server = TRUE)
  
  #-- Cuando se selecciona un epigrafe devolvemos un DT con los datos de los locales de ese epigrafe por distrito
  localesdist <- eventReactive(input$epig, {
    locales.dist <- subset(localpordist,desc_epigrafe==input$epig)
    locales.dist <- locales.dist[,c(1,3), with=FALSE]
    setnames(locales.dist,"nombre_distrito","Distrito")
  })
  
  #-- Si se selecciona un epigrafe mostramos las tablas de locales x distrito y habitantes x local
  observeEvent(input$epig,{
    if(input$epig!=""){
      line1 <- as.data.table(localesdist())
      local.por.dist <- copy(subset(localpordist,desc_epigrafe==input$epig))
      colnames(local.por.dist) <- c("desc_distrito","desc_epigrafe","total_locales")
      habitantesporlocal <- left_join(habitantesdistrito,local.por.dist,by="desc_distrito")
      habdistlocal <- data.frame(Distrito=habitantesporlocal$desc_distrito,total=round(habitantesporlocal$total/habitantesporlocal$total_locales,digits = 2)) 
      line2 <- habdistlocal  
      output$resultmaxlocales <- DT::renderDataTable(
        line1, selection = 'single', server = FALSE,
        options = list(
          paging = FALSE,
          scrollY = "300px",
          searching = FALSE,
          pageLength = 5,
          order = list(2,'desc')
        )
      )
      output$resultppl <- DT::renderDataTable(
        line2, selection = 'single', server = FALSE,
        options = list(
          paging = FALSE,
          scrollY = "300px",
          searching = FALSE,
          pageLength = 5,
          order = list(2,'desc')
        )
      )
    }
    
  })
  
  #-- Al pinchar en una fila de la tabla aparece un popup con los dato de ese distrito
  observeEvent(input$resultmaxlocales_rows_selected,{
    leafletProxy("mapdist") %>% clearPopups()
    distrito <- localesdist()[input$resultmaxlocales_rows_selected]$Distrito
    distcoor <- coordinates(madshape[madshape$DESBDT==distrito,])
    showPopup("mapdist",distrito,distcoor[2],distcoor[1])
  })
  
  observeEvent(input$resultppl_rows_selected,{
    leafletProxy("mapdist") %>% clearPopups()
    distrito <- habitantesdistrito[input$resultppl_rows_selected]$desc_distrito
    distcoor <- coordinates(madshape[madshape$DESBDT==distrito,])
    showPopup("mapdist",distrito,distcoor[2],distcoor[1])
  })
  
  #-- Si se selecciona epigrafe y/o distrito devolvemos los datos de la seleccion
  seleccion <- reactive({
    if (input$epig != "") {
      if (input$dist != "") {
        subset(dtlocales,nombre_distrito == input$dist &
                 desc_epigrafe == input$epig)
      }else{
        subset(dtlocales,desc_epigrafe == input$epig)
      }
    }
  })
  
  #Calculamos los puntos mas distantes a los locales seleccionados mediante
  #diagramas voronoi. Los devolvemos ordenados del mas alejado al mas cercano
  ptosintvoronoi <- reactive({
    if (input$epig != "" && input$dist != "") {
      xmin <- min(seleccion()$lon)
      xmax <- max(seleccion()$lon)
      ymin <- min(seleccion()$lat)
      ymax <- max(seleccion()$lat)
      voronoi <-
        deldir(seleccion()$lon, seleccion()$lat,rw = c(xmin,xmax,ymin,ymax))
      dt <-
        data.frame(lat = voronoi$dirsgs$y1,lon = voronoi$dirsgs$x1)
      dt <- dt[!duplicated(dt),]
      #Desechamos los puntos que caen fuera del distrito seleccinado
      x <- seleccion()$lon
      y <- seleccion()$lat
      z <- chull(x,y)
      #         polidist <- madshape[madshape$DESBDT==input$dist]
      #         polidist <- as.data.frame(coordinates(polidist))
      dt <-
        dt[as.logical(point.in.polygon(dt$lon,dt$lat,x[z],y[z])),]
      #Guardamos en dist la distancia minima de cada punto calculado
      #respecto a los locales seleccionados. Para un calculo rapido usamos
      #k-nearest neighbor
      df <- as.data.frame(seleccion()[,c("lat","lon"),with = FALSE])
      dt$dist <- get.knnx(df,dt,k = 1)$nn.dist
      dt$rank <-
        frankv(dt,cols = "dist",ties.method = "random",order = -1L)
      output$controlptos <- renderUI({
        sliderInput(
          "nptos","Seleccione los puntos a mostrar",min = 1,max = min(nrow(dt),50),value = 1,step =
            1
        )
      })
      return(dt <- arrange(dt,desc(dist)))
    }
  })
  #---------------------------
  #-- Dibujamos por defecto el mapa de Madrid dibidido por distritos
  output$mapdist <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      #       addProviderTiles("Hydda.RoadsAndLabels", options = providerTileOptions(opacity = 0.35)) %>%
      #       addProviderTiles("Esri.WorldGrayCanvas") %>%
      setView(-3.692416,40.461621,zoom = 11) %>%
      addPolygons(data = madshape, fillColor = ~ pal(renta),fillOpacity = 0.4,weight = 0.5,smoothFactor = 0.2,layerId = ~DESBDT) %>%
      addLegend(pal = pal,values = madshape$renta,position = "bottomleft",title = "Renta bruta por habitante",labFormat = labelFormat(suffix = ".000"))
  })
  
  #-- Si el usuario elige un epigrafe y/o un distrito se añade una capa de puntos
  #-- con la localizacion de los locales
  observe({
    if (input$epig != "") {
      leafletProxy("mapdist",data = seleccion()) %>%
        clearMarkers() %>%
        clearPopups() %>%
        addCircleMarkers(lng = ~ lon,lat = ~ lat,color = "steelblue",popup = ~ as.character(rotulo),radius = 4,stroke = FALSE,fillOpacity = .7)
      if (nrow(seleccion()) > 1) {
        leafletProxy("mapdist", data = seleccion()) %>%
          #-- Con fitBounds ajustamos automáticamente el mapa para ajustarlo a los puntos
          #-- de la seleccion realizada
          fitBounds(
            lng1 = max(seleccion()$lon),lat1 = max(seleccion()$lat),
            lng2 = min(seleccion()$lon),lat2 = min(seleccion()$lat)
          )
      }
      else{
        leafletProxy("mapdist", data = seleccion()) %>%
          setView(seleccion()[1,]$lon, seleccion()[1,]$lat, zoom = 13)
      }
    }
    if (input$epig != "" &&
        input$dist != "" && nrow(seleccion()) > 1) {
      if (input$distptos == TRUE) {
        ifelse (is.null(input$nptos),ptos <- 1,ptos <- input$nptos)
        leafletProxy("mapdist",data = seleccion()) %>%
          clearPopups() %>%
          addCircleMarkers(data = ptosintvoronoi()[1:ptos,],lng = ~ lon,lat = ~ lat,color="green",radius = ~ (dist * 2000),stroke = FALSE,popup =  ~ paste0(lat," ",lon),fillOpacity = .5)
      }
    }
  })
  
  #-- Dibujamos el grafico de distribucion de la poblacion
  output$histpoblacion <- renderPlot({
    if (input$dist != "") {
      edadseleccion <- subset(dtpoblacion, desc_distrito == input$dist)
      edadgroup <- edadseleccion %>%
        group_by(desc_distrito,edad_dec) %>%
        dplyr::summarise(
          hombres =
            sum(as.integer(espanoleshombres)) + sum(as.integer(extranjeroshombres)),
          mujeres =
            sum(as.integer(espanolesmujeres)) + sum(as.integer(extranjerosmujeres))
        )
      
      edadgroup.long <-
        melt(
          edadgroup, id.vars = c("desc_distrito","edad_dec"),
          measure.vars = c("hombres","mujeres")
        )
      
      ggplot(edadgroup.long,aes(edad_dec,value,fill = variable)) +
        geom_bar(stat = "identity", position = "dodge") +
        ggtitle("Distribucion de la poblacion") +
        xlab("Edad (en decenas)") +
        ylab("Total habitantes") +
        theme(
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.ontop = TRUE
        )
    }
  })
  #--------------------------
  #-- Introducimos los elementos requeridos en los desplegables
  observeEvent(input$cat,{
    leafletProxy("mapcp") %>% clearPopups()
    enable("subcat")
    subcatoption <- subset(catdf,cat_description == input$cat)
    updateSelectizeInput(session, "subcat",choices = intersect(unique(subcatoption$subcat_description),unique(subcattopCP$subcategoria)) ,server = TRUE)
  })
  
  #-- Dibujamos por defecto el mapa de Madrid dividivo por CP
  output$mapcp <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-3.692416,40.461621,zoom = 11) %>%
      addPolygons(data = cpshape, fillOpacity = 0.1, weight = 0.3,smoothFactor = 0.2, layerId = ~DESBDT)
  })
  
  #-- Cuando se selecciona una subcategoria devolvemos los datos de los CP con mas ventas
  maxsubcatincomes <- eventReactive(input$subcat,{
    if(input$subcat!=""){
      max.subcat.incomes <- subset(subcattopCP,subcategoria==input$subcat)
      setnames(max.subcat.incomes,c("cp","total"),c("Codigo postal","Ingresos"))
      return(max.subcat.incomes)
    }
  })
  
  #-- Cuando se selecciona una subcategoria devolvemos los datos de los CP con mas gasto fuera 
  #-- de su CP
  maxdisplacementcp <- eventReactive(input$subcat,{
    if(input$subcat!=""){
      subcat.all.stats <- subset(all.stats.per.cat.and.origen,subcategoria==input$subcat)
      max.displacement.cp <- subset(subcat.all.stats,cp_origen!=cp)
      setnames(max.displacement.cp,c("cp_origen","cp","total"),c("Origen","Destino","Total"))
      return(max.displacement.cp)
    }
  })
  
  #-- Cuando se selecciona una subcategoria añadimos el total de ingresos por CP al shape
  #-- y añadimos el resultado a las tablas
  observeEvent(input$subcat,{
    if (input$subcat != "") {
      leafletProxy("mapcp") %>% clearPopups()
      seleccion.top100stats <- subset(subcattopCP,subcategoria == input$subcat)
      
      tmp <- vector()
      for (i in cpshape$DESBDT) {
        ifelse(
          i %in% seleccion.top100stats$cp,tmp <-
            append(tmp,seleccion.top100stats[seleccion.top100stats$cp == i]$total),tmp <-
            append(tmp,0)
        )
      }
      cpshape$total <- tmp
      palcp <- colorNumeric(palette = "YlGnBu", domain = cpshape$total)
      leafletProxy("mapcp",data = cpshape) %>%
        clearControls() %>%
        addPolygons(fillColor = ~ palcp(total),fillOpacity = 0.4,weight = 0.5,smoothFactor = 0.2, layerId = ~DESBDT) %>%
        addLegend(pal = palcp, values = cpshape$total, position = "bottomleft", title = "Ingresos anuales (miles de €)")
    }
    
    #-- Mostramos las tablas con informacion sobre los ingresos por CP
    if(input$subcat!=""){
      line1 <- as.data.table(maxsubcatincomes()[,3:4,with = FALSE])
      line2 <- as.data.table(maxdisplacementcp()[,3:5,with = FALSE]) 
      output$resultcp <- DT::renderDataTable(
        line1, selection = 'single', server = FALSE,
        options = list(
          paging = FALSE,
          scrollY = "300px",
          searching = FALSE,
          pageLength = 5,
          order = list(2,'desc')
        )
      )
      output$resultdispl <- DT::renderDataTable(
        line2, selection = 'single', server = FALSE,
        options = list(
          paging = FALSE,
          scrollY = "300px",
          searching = FALSE,
          pageLength = 5,
          order = list(3,'desc')
        )
      )
    }
  })
  
  #-- Al pinchar en una fila de la tabla aparece un popup con los dato de ese CP
  observeEvent(input$resultcp_rows_selected,{
    leafletProxy("mapcp") %>% clearPopups()
    zipcode <- maxsubcatincomes()[input$resultcp_rows_selected]$`Codigo postal`
    cpcoor <- coordinates(cpshape[cpshape$DESBDT==zipcode,])
    showPopup("mapcp",zipcode,cpcoor[2],cpcoor[1])
  })
  
  observeEvent(input$resultdispl_rows_selected,{
    leafletProxy("mapcp") %>% clearPopups()
    zipcode <- maxdisplacementcp()[input$resultdispl_rows_selected,]$Origen
    cpcoor <- coordinates(cpshape[cpshape$DESBDT==zipcode,])
    showPopup("mapcp",zipcode,cpcoor[2],cpcoor[1])
  })
  
  #-- Funcion que configura los popup
  showPopup <- function(map, id, lat, lng) {
    if (map=="mapcp"){
    selectedZip <- top100stats[top100stats$cp == id,]
    total <- ifelse(input$subcat != "",
                    round(sum(as.numeric(selectedZip[selectedZip$subcategoria == input$subcat,]$incomes) / 1000),digits = 2), 
                    "Sin datos")
    content <- as.character(tagList(
      tags$h5(sprintf("Codigo Postal: %s",id)),
      if (input$subcat != "")
        tags$h6(sprintf(input$subcat)),
      tags$b(class = "titpopup",sprintf("Total:")),
      sprintf(ifelse(
        total == 0,"Sin datos",as.character(total)
      ))
    ))
    
    }
    if(map=="mapdist"){
      renta <- subset(madshape,DESBDT==id)$renta
      content <- as.character(tagList(
        tags$h5(sprintf("Distrito: %s",id)),
        if(input$epig!=""){
          numlocales <- nrow(dtlocales %>% filter(desc_epigrafe==as.character(input$epig)) %>% filter(nombre_distrito==as.character(toupper(id))))
          habitantes <- subset(habitantesdistrito, desc_distrito == as.character(toupper(id)))$total
          habitantesporlocal <- round(habitantes/numlocales,digits = 2)
          renta <- subset(madshape,DESBDT==id)$renta
          HTML(
            paste(
            tags$h6(sprintf(input$epig)),
            tags$b(class = "titpopup", "Total en el distrito:"),
            tags$b(sprintf(as.character(numlocales))),
            tags$br(),
            tags$b(class = "titpopup", "Habitantes por local:"),
            tags$b(sprintf(as.character(habitantesporlocal) ))
            )
          )
        },
        tags$br(),
        tags$b(class = "titpopup", "Renta por habitante:"),
        tags$b(sprintf(as.character(renta) ))
        ))
    }
    leafletProxy(map) %>% clearPopups() %>% addPopups(lng, lat, content)
  }
  #-- Muestra un popup al pinchar en el mapa de CP
  observe({
    leafletProxy("mapcp") %>% clearPopups()
    event <- input$mapcp_shape_click
    if (is.null(event))
      return()
    isolate({
      showPopup("mapcp",event$id, event$lat, event$lng)
    })
  })
  #-- Muestra un popup al pinchar en el mapa de distritos
  observe({
    leafletProxy("mapdist") %>% clearPopups()
    event <- input$mapdist_shape_click
    if (is.null(event))
      return()
    isolate({
      showPopup("mapdist",event$id, event$lat, event$lng)
    })
  })
  
  #-- Generacion del informe
  #-- Grafico de poblacion
  pobgroup <- dtpoblacion %>%
    group_by(desc_distrito,edad_dec) %>%
    dplyr::summarise(
      hombres =
        sum(as.integer(espanoleshombres)) + sum(as.integer(extranjeroshombres)),
      mujeres =
        sum(as.integer(espanolesmujeres)) + sum(as.integer(extranjerosmujeres))
    )
  pobgroup.long <-
    melt(
      pobgroup, id.vars = c("desc_distrito","edad_dec"),
      measure.vars = c("hombres","mujeres")
    )
  p <- ggplot(pobgroup.long,aes(edad_dec,value,fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Edad (en decenas)") +
    ylab("Total habitantes") +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.ontop = TRUE
    ) +
    facet_wrap("desc_distrito")
  
  output$poblacion <- renderPlot(p)
  
#   #-- Tabla de Locales por distrito
  informelocalpordist <- copy(as.data.table(localpordist))
  colnames(informelocalpordist) <- c("Distrito","Epigrafe","Total locales en distrito")
  #informelocalpordist$Epigrafe <- factor(informelocalpordist$Epigrafe)
  output$localesdistrito <- DT::renderDataTable(
    informelocalpordist, filter = "bottom", options = list(
      scrollY = "300px",
      order = list(1,'asc')
    )
  )
  
  #-- Tabla de habitantes por local
  tmphabitantesdistrito <- copy(habitantesdistrito)
#    setnames(tmphabitantesdistrito,"desc_distrito","nombre_distrito")
#    setnames(tmphabitantesdistrito,"total","total_habitantes")
  colnames(tmphabitantesdistrito) <- c("nombre_distrito","total_habitantes")
  habitantesporlocal <- left_join(localpordist,tmphabitantesdistrito, by="nombre_distrito")
  informehabdistlocal <- data.frame(Distrito=habitantesporlocal$nombre_distrito,Epigrafe=habitantesporlocal$desc_epigrafe,'Habitantes por local'=round(habitantesporlocal$total_habitantes/habitantesporlocal$total,digits = 2))
  output$localeshabitante <- DT::renderDataTable(
    informehabdistlocal, filter = "bottom", options = list(
      scrollY = "300px",
      order = list(1,'asc')
    )
  )
  
  #-- Tabla de gastos por CP
  informetopCP <- copy(as.data.table(subcattopCP[,2:4,with = FALSE]))
  setnames(informetopCP,c("subcategoria","cp","total"),c("Subcategoria","Codigo postal","Ingresos"))
  output$gastocp <- DT::renderDataTable(
    informetopCP, filter = "bottom", options = list(
      scrollY = "300px",
      order = list(3,'desc')
    )
  )
  
  #-- Tabla de desplazamientos
  tmpdesplazamientos <- copy(as.data.frame(all.stats.per.cat.and.origen[,2:5,with = FALSE]))
  informedesplazamientos <- subset(tmpdesplazamientos,cp_origen!=cp)
  setnames(informedesplazamientos,c("subcategoria","cp_origen","cp","total"),c("Subcategoria","Origen","Destino","Total"))
  output$desplazamientocp <- DT::renderDataTable(
    informedesplazamientos, filter = "bottom", options = list(
      scrollY = "300px",
      order = list(4,'desc')
    )
  )
  
})
