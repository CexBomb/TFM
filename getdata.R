library(data.table)
library(plyr)
library(dplyr)
library(ggmap)
library(gdata)
library(httr)
library(jsonlite)

########################################################
#--------------------- FUNCIONES -----------------------
########################################################
#-- Funcion para pasar coordenadas de grados a decimales
#-- Se utiliza la formula dec = D + M/60 + S/3600
degreetodecimal <- function(coord){
  tmp1=strsplit(coord,"º")
  tmp2=strsplit(tmp1[[1]][2],"\\'")
  dec=c(as.numeric(tmp1[[1]][1]),as.numeric(tmp2[[1]][1]),as.numeric(tmp2[[1]][2]))
  return(dec[1]+dec[2]/60+dec[3]/3600)
}

#-- Funcion para obtener de la API de Google el codigo postal, la lon y lat.
#-- Se le pasa una direccion (tipo de via, nombre de via y numero) y devuelve su
#-- codigo postal, su latitud y su longitud
getlatlonAPI <- function(a,b,c){
  direccion <- paste(trimws(a),"+", gsub(" ","+",trimws(b)),"+", trimws(c), "+Madrid", sep = "")
  lon.lat <-  geocode(direccion)
  resp <- revgeocode(as.numeric(lon.lat), output = "more")
  zip.code <- as.numeric(as.character(resp$postal_code))
  return(c(zipcode=zip.code,lat=lon.lat[2],lon=lon.lat[1]))
}

########################################################
#------------- CARGA Y LIMPIEZA DE DATOS ---------------
########################################################
#-- Cargamos y preparamos los datos de los locales
dtlocales <- fread('Data/Locales201602.csv', encoding = 'Latin-1')
dtlocales <- dtlocales[,c(1,3,5,14,16,17,20,21,40,42,44,46), with = FALSE]
#--Seleccionamos los locales relacionados con negocios minoristas u hostelería
dtlocales <- subset(dtlocales,
                    desc_division == "COMERCIO AL POR MENOR, EXCEPTO DE VEHICULOS DE MOTOR Y MOTOCICLETAS"
                    | desc_division == "VENTA Y REPARACI0N DE VEHICULOS DE MOTOR Y MOTOCICLETAS" 
                    | desc_seccion == "HOSTELERIA" 
                    | desc_seccion == "ACTIVIDADES INMOBILIARIAS"
)
#-- Eliminamos los locales no abiertos
dtlocales <- subset(dtlocales,desc_situacion_local == "Abierto")
dtlocales$desc_situacion_local <- NULL
#-- Eliminamos los locales sin actividad
dtlocales <- subset(dtlocales,desc_epigrafe != "")
dtlocales <- subset(dtlocales,desc_epigrafe != "LOCAL SIN ACTIVIDAD")
#-- Eliminamos los espacios lead y trail 
dtlocales <- as.data.table(vapply(dtlocales,trimws,character(nrow(dtlocales))))

#-- Cargamos y preparammos los datos de los codigos postales
codigospostales <- fread('Data/CALLEJERO_VIGENTE_NUMERACIONES_201604.csv', encoding = 'Latin-1')
codigospostales <- codigospostales[,c(3,5,6,8,10,12,19,20),with = FALSE]
colnames(codigospostales) <- c("clase_via","nombre_via","numero","nombre_distrito","nombre_barrio","zipcode","x","y")
#-- Separamos la columna numero en nom_edificio y num_edificio
codigospostales$nom_edificio <- substr(codigospostales$numero,1,3)
codigospostales$num_edificio <- lapply(codigospostales$numero,function(x) unlist(strsplit(x,"[^0-9]+"))[2])
codigospostales$numero <- NULL
codigospostales$num_edificio <- as.character(as.numeric(codigospostales$num_edificio))
#-- Eliminamos los espacios lead y trail 
codigospostales <- as.data.table(vapply(codigospostales, trimws, character(nrow(codigospostales))))

#-- Rellenamos la tabla de locales con los datos de codigospostales.
dtlocales <- dtlocales[,c("id_local","desc_barrio_local", "desc_vial_edificio","nom_edificio",
                          "clase_vial_edificio","desc_distrito_local","num_edificio",
                          "rotulo","desc_seccion","desc_division","desc_epigrafe"),with=FALSE]
colnames(dtlocales) <- c("id_local","nombre_barrio","nombre_via","nom_edificio",
                         "clase_via","nombre_distrito","num_edificio",
                         "rotulo","desc_seccion","desc_division","desc_epigrafe")
codigospostales <- codigospostales[,c("nombre_barrio","nombre_via","clase_via","nombre_distrito",
                                      "num_edificio","nom_edificio","zipcode","x","y"),with=FALSE]
dtlocales <- join(dtlocales,codigospostales,by=c("clase_via","nombre_via","nom_edificio","num_edificio","nombre_barrio","nombre_distrito"),type="left",match="first")
rm("codigospostales")

#-- Pasamos las coordenadas de DMS a decimal
dtlocales$lat <- round(sapply(dtlocales$y,degreetodecimal), digits = 5)
dtlocales$lon <- round(sapply(dtlocales$x,degreetodecimal), digits = 5)*(-1)
dtlocales$x <- dtlocales$y <- NULL

#-- Rellenamos con la API de Google los campos vacios
dtnulos <- subset(dtlocales,is.na(zipcode))
for(i in 1:nrow(dtnulos)){
  resp <- with(dtnulos[i,],getlatlonAPI(clase_via,nombre_via,num_edificio))
  dtlocales[which(dtlocales$id_local==dtnulos[i,]$id_local),]$zipcode <- as.character(resp[1])
  dtlocales[which(dtlocales$id_local==dtnulos[i,]$id_local),]$lat <- resp[2]
  dtlocales[which(dtlocales$id_local==dtnulos[i,]$id_local),]$lon <- resp[3]
}

#-- Cargamos y preparamos los datos de poblacion
poblacion <- fread('Data/Rango_Edades_Seccion_201604.csv',encoding = 'Latin-1')
colnames(poblacion) <- tolower(colnames(poblacion))
#-- Eliminamos los espacios lead y trail 
poblacion <- as.data.table(vapply(poblacion,trimws,character(nrow(poblacion))))
poblacion[poblacion==""] <- 0
#-- Agrupamos las edades por decenas
poblacion$edad_dec <- as.numeric(poblacion$cod_edad_int)%/%10

#-- Cargamos y preparamos los datos de renta
renta <- read.xls("Data/D1121314_base2010.xls",sheet = 3,pattern = "2002", encoding = "Latin-1")
renta <- renta[,c("X","X2012..a.")]
renta <- renta[2:22,]
colnames(renta) <- c("distrito","y2012")
renta$distrito <- gsub("\xed","i",renta$distrito)
renta$distrito <- gsub("\xe1","a",renta$distrito)
renta$y2012 <- gsub(",",".",renta$y2012)
renta$y2012 <- as.numeric(renta$y2012)
renta$distrito <- substr(renta[,"distrito"],5,nchar(renta[,"distrito"]))

########################################################
# ------------------- API BBVA -------------------------
########################################################
#-- Obtenemos un token para hacer peticiones a la API
filename <- "./Data/tokenBBVA"
credentials <- paste("Basic",readChar(filename,file.info(filename)$size))
token <- POST("https://connect.bbva.com/token", query = list(grant_type = "client_credentials"), add_headers(Authorization = credentials))
token <- fromJSON(content(token,"text"))
accesskey <- paste(token$token_type,token$access_token)
#-- Hacemos una llamada a la API para obtener todas las categorias de comercios
r <- GET("https://apis.bbva.com/paystats_sbx/2/info/merchants_categories", add_headers(Authorization = accesskey))
resp <- fromJSON(content(r,"text"))
#-- Guardamos las categorias y subcategorias en un Data Frame
categorias <- data.frame(code = resp$data$stats$categories[[1]]$code,description = resp$data$stats$categories[[1]]$description)
subcategorias <- resp$data$stats$categories[[1]]$subcategories
catdf <- data.frame(
    cat_code = character(0),cat_description = character(0),
    subcat_code = character(0),subcat_description = character(0)
  )
for (i in 1:nrow(categorias)) {
  tmp <- data.frame(
      cat_code = categorias$code[i],cat_description = categorias$description[i],
      subcat_code = subcategorias[[i]]$code,subcat_description = subcategorias[[i]]$description
    )
  catdf <- rbind(catdf,tmp)
}
catdf$cat_description <- as.character(unlist(catdf$cat_description))
catdf$subcat_description <- as.character(unlist(catdf$subcat_description))
listacp <- c(28001:28055,28790)

#-- Recuperamos los top100 CPs para cada CP y categoría
top100stats <- data.table(code_subcategoria=character(0),subcategoria=character(0),label=character(0),cards=character(0),txs=character(0),incomes=character(0),cp=character(0),mes=character(0))
system.time(
  for (i in 1:nrow(catdf)){
    code_subcategoria <- catdf[i]$subcat_code
    subcategoria <- catdf[i]$subcat_description
    print(i)
    for (zipcode in listacp){
      peticion <- paste("https://apis.bbva.com/paystats_sbx/2/zipcodes/",zipcode,"/customer_zipcodes?by=incomes&category=",code_subcategoria,"&date_max=20141231&date_min=20140101&group_by=month",sep = "")
      top100cp <- GET(peticion, add_headers(Authorization = accesskey))
      if(top100cp$status_code==401){
        token <- POST("https://connect.bbva.com/token", query = list(grant_type = "client_credentials"), add_headers(Authorization = credentials))
        token <- fromJSON(content(token,"text"))
        accesskey <- paste(token$token_type,token$access_token)
        top100cp <- GET(peticion, add_headers(Authorization = accesskey))
      }
      top100json <- fromJSON(content(top100cp,"text"))
      for (j in 1:length(top100json$data$stats$zipcodes)){
        tmp <- as.data.table(top100json$data$stats$zipcodes[[j]])
        if (!is.null(top100json$data$stats$zipcodes[[j]])){
          tmp$code_subcategoria <- code_subcategoria
          tmp$subcategoria <- subcategoria
          tmp$cp <- zipcode
          tmp$mes <- j
          top100stats <- rbind(top100stats,tmp)
        }
      }
    }
    setnames(top100stats, "label", "cp_origen")
  })

#-- Guardamos las tablas en ficheros csv
write.csv(dtlocales,file = "Data/dtlocales.csv", row.names = FALSE)
write.csv(poblacion, "Data/poblacion.csv", row.names = FALSE)
write.csv(renta,"Data/renta.csv", row.names = FALSE)
write.csv(catdf,"Data/categorias.csv", row.names = FALSE)
write.csv(top100stats,"Data/top100stats.csv", row.names = FALSE)
