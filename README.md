---
title: "Memoria del Trabajo Fin de Master"
author: "Cesar Gonzalez"
date: "15 de junio de 2016"
output: html_document
---


##Introducción

La finalidad del Trabajo Fin de Máster realizado consiste en una aplicación que intenta responder a la pregunta: Si quisiera montar un negocio: ¿Dónde sería una buena zona para ubicarlo?

Por supuesto, el éxito de un negocio depende de factores internos, como una buena gestión, trato al cliente, etc. Pero en la mayoría de negocios existen factores externos que condicionan el éxito o fracaso del mismo. Estos factores pueden ser el número de habitantes de la zona, su renta media, la distribución de la edad de la población, la existencia o no de competencia, etc.

Lógicamente cada negocio es distinto y tiene sus propias características, afectando estos factores de manera distinta a cada uno de ellos. Por este motivo no se puede dar una respuesta única ni 100% fiable a la pregunta inicial, pero la importania de estos datos se demuestra en el hecho de que en cualquier plan de negocio se exige que se tengan en cuenta estos factores, el llamado *Entorno económico y sociocultural*

Con la elaboración de este Trabajo se intenta ofrecer una ayuda a la importante toma de decisión que supone la ubicación de un negocio y ofrecer datos fiables a la hora de realizar un plan de negocio o un estudio de mercado.

Se puede acceder a la aplicación desde la dirección <http://cescbox.ddns.net:24445/>. En la propia página existe una sección de ayuda con tutoriales y videos explicativos.

##Uso de la aplicación

El uso de la aplicación es muy sencillo. Tiene tres secciones fundamentales:

* Un mapa donde se dibujarán los locales de los epígrafes elegidos. Además muestra información de renta y de distribución de la población. Sólo se tiene que seleccionar el Epígrafe deseado para empezar a ver todos los locales correspondientes a ese Epígrafe en Madrid. Si se selecciona un distrito veremos sólo los locales de ese distrito y un gráfico con la distribución de la edad de ese distrito. Finalmente, pinchando en _Mostrar puntos mas distantes_ podremos ver los puntos más alejados de la competencia, pudiendo elegir el número de puntos mostrados. El tamaño de los puntos está relacionado directamente con la distancia a la competencia.
* Un mapa en el que se muestra información de ingresos económimcos de diversos tipos de negocio. En él podremos ver cuáles son los negocios con más ingresos y cuáles son las zonas desde las que se desplazan sus habitantes en busca de un negocio.
* Una sección con informes desde la que se pueden ver los datos en bruto para un análisis más global.


##Datos de entrada

Para la realización del trabajo se han utilizado diversas fuentes tanto públicas como privadas. La mayoría de los datos de población se han descargado desde la plataforma de datos abiertos de la Comunidad de Madrid <http://datos.madrid.es/portal/site/egob>

Otros datos han sido obtenidos de la API del BBVA <https://www.bbvaapimarket.com/web/api_market/>. Estos datos no pueden ser publicados por lo que sólo se hará una descripción de los mismos.

###Tablas públicas

Debido a que la mayoría de las tablas utilizadas son públicas y además son muy pesadas, se va a indicar la página desde las que se pueden descargar en lugar de publicarlas.


* Censo de locales y sus actividades. Descargada desde <http://datos.madrid.es/sites/v/index.jsp?vgnextoid=66665cde99be2410VgnVCM1000000b205a0aRCRD&vgnextchannel=20d612b9ace9f310VgnVCM100000171f5a0aRCRD>. Contiene los datos de todos los locales de Madrid y sus actividades por Epígrafe, así como su dirección y coordenadas en formato ED50.

* Callejero vigente. Descargado desde <http://datos.madrid.es/sites/v/index.jsp?vgnextoid=f1555cde99be2410VgnVCM1000000b205a0aRCRD&vgnextchannel=20d612b9ace9f310VgnVCM100000171f5a0aRCRD>. Este listado contiene datos geográficos y administrativos de todos los portales y edificios de Madrid. Esta información fué necesaria utilizarla debido a que la anterior tabla tenía coordenadas erróneas y en ocasiones inexistentes. De ella se extraen las coordenadas (en formato DMS) y los códigos postales.

* Padrón municipal. Descargado desde <http://datos.madrid.es/sites/v/index.jsp?vgnextoid=1d755cde99be2410VgnVCM1000000b205a0aRCRD&vgnextchannel=20d612b9ace9f310VgnVCM100000171f5a0aRCRD>. Tabla con un desglose por edades de todos los habitantes de Madrid por distrito, sección censal, etc.

* Datos de renta de los hogares. Descargada desde <http://www.madrid.es/portales/munimadrid/es/Inicio/El-Ayuntamiento/Estadistica/Areas-de-informacion-estadistica/Economia/Renta/Cuentas-de-renta-del-sector-Hogares?vgnextfmt=default&vgnextoid=e5561869a5cd0510VgnVCM2000000c205a0aRCRD&vgnextchannel=ef863636b44b4210VgnVCM2000000c205a0aRCRD>. Fichero de Excel desde el que obtenemos la información de la renta media de cada distrito.

###Shapefiles

Para la división de los mapas en Distritos y Códigos Postales se utilizaron los siguientes shapefiles:

* Shapefile de Distritos.
* Shapefile de Códigos Postáles

Ambos descargados desde la página <http://www.madrid.org/nomecalles/DescargaBDTCorte.icm>. Estos shapes tienen la información geográfica con el sistema de referencia ED50.

###Datos de la API del BBVA

El BBVA tiene una API que ofrece información relativa al pago realizado mediante tarjeta bancaria en los distintos negocios de la Comunidad de Madrid. Esta API está en fase Alpha y para acceder a sus servicios nos pusimos en contacto con el responsable de la misma para solicitar el acceso. Al no ser información pública no se subirán las tablas obtenidas, pero se pueden ver los datos en la propia aplicación.

La información de pago está agregada por Código Postal y por categorías en lugar de por Epígrafes (por ejemplo "Bares y Restaurantes""). 
Por este motivo no ha sido posible integrarlos en un sólo mapa con los datos anteriores como hubiera sido lo deseable. Sin embargo, esta información también es importante, por lo que se decidió incluirla en un mapa separado.

Estos datos no muestran el gasto total de cada negocio al ser sólo datos de pago mediante tarjeta bancaria. Sin embargo, por la ley de los grandes números, se puede concluir que los negocios con más ventas con tarjeta de crédito son los que más ventas tienen en general. Del mismo modo, si una zona tiene más ventas mediante tarjeta que otra, se puede concluir que tendrá también, en general más ingresos totales.

De la API del BBVA se obtuvieron las siguientes tablas:

* Una tabla con todas las categorías y subcategorías de negocios de las que se tienen datos.
* Otra tabla con el top 100 de localizaciones de residencia de clientes. Esta tabla se utiliza para ver cuales son los Códigos Postáles con mayores ingresos y también cuáles son los habitantes que más se desplazan a la hora de pagar (si de un determinado Código Postal se desplazan muchos habitantes es posible que falten servicios en esa zona.). La petición a este servicio se realiza por categoría o subcategoría de negocio, por lo que fué necesario hacer una llamada por cada subcategoría y guardar toda la información en una única tabla. La información sobre este y otros servicios puede encontrarse en <https://www.bbvaapimarket.com/web/api_market/bbva/paystats/documentation>

##Tratamiento de los datos.

La aplicación se ha implementado utilizando R, Shiny y Leaflet entre otras tecnologías. La idea es ofrecer un servicio web que muestre información en un tiempo razonable. Por este motivo se dividió el trabajo en dos archivos. El primero realizado en R, dedicado exclusivamente al tratamiento, limpieza y preparación de los datos. El segundo, una aplicación Shiny, obtiene los datos anteriores y los utiliza para una rápida visualización de los mismos. Sin embargo desde este archivo se realizan también algunos cálculos complejos como se detalla más adelante.

####Tabla de locales.

Esta tabla tiene muchas columnas innecesarias, por lo que lo primero es seleccionar sólo las que son de interés. 
Existen muchas actividades en esta tabla, algunas de ellas no tienen mucho sentido incluirlas en este trabajo, como grandes industrias, por ejemplo. En lugar de ellos nos hemos centrado en los negocios que requieren de un local comercial. Por este motivo se han filtrado las actividades dejando sólo las correspondientes a venta al por menor, hostelería e inmobiliarias. Por supuesto si se estuviese interesado en alguna actividad adicional sería tan sencillo como añadir dicha actividad al filtro.
Después eliminamos los locales cerrados o que no tiene actividad declarada.

####Tabla de Códigos Postáles.

Esta tabla se utlizará para enriquecer la tabla anterior con las coordenadas y Códigos Postáles de los locales filtrados. Esto se realizará mediante un Join, por lo que lo primero que se hace es cambiar el nombre de las columnas para que coincidcidan con los de la tabla anterior y se normalizan los campos de dirección que están también en un formato distinto. Una vez realizada la limpieza de ambas tablas se unen mediante un Join de manera que a cada local se le agregan su código postal y sus coordenadas. 

####Función degreetodecimal.

Debido a que las coordenadas de la tabla anterior se encuentran en formato DMS, se implementa una función para pasarlas a formato decimal.

####Función getlanlonAPI.

A pesar de especificar la codificación de las tablas con el formato "Latin-1", al realizar el Join de las dos primneras las direcciones correspondientes a calles con caracteres como la "ñ" aparecieron con campos nulos en las coordenadas y el código postal. Estas direcciones eran unas 100, por lo que se implementó una función para recopilar esta información de la API de Google. Ésta API permite hasta 2500 llamadas al día de manera gratuita y sólo es necesario llamarla una vez, por lo que se cumple con las Condiciones de Uso.

####Tabla de edad de población.

Esta es la tabla más limpia de todas. Para una correcta visualización de sus datos se decidió mostrar la distribución de la edad por decenas en vez de por años. Por este motivo sólo fué necesario incluir un campo en el que se añade la decena de la edad de los habitantes y hacer una agregación por este campo.

####Tabla con los datos de renta.

Al ser una tabla con formato Excel, se tuvo que eliminar las filas y columnas que no interesaban así como sustituir caracteres que no se mostraban correctamente como los acentos y dar un nombre adecuado a las columnas

####API del BBVA.

Se hace una llamada para recibir un token. A continuación se utiliza este token para hacer una petición de las categorías de negocio existentes. Una vez obtenidas todas las categorías se hace otra llamada para obtener el top 100 de gasto por Código Postal para cada categoría y se guarda en una tabla. Este proceso tarda unos 40 minutos en realizarse y es necesario tenerlo precalculado para tener los datos de los habitantes que más se desplazan por categoría.

####Cálculo de las zonas más alejadas de la competencia.

Una de las características más importantes de éste trabajo y el motivo inicial por el que se decidió realizarlo es la posibilidad de calcular la zona o zonas más alejada del resto de la competencia.
El cálculo de estas zonas se realiza en distintas fases:

1. Se calculan los puntos más alejados de los locales.Este cálculo se realiza mediante `Diagramas de Voronoi` <https://en.wikipedia.org/wiki/Voronoi_diagram>. Estos diagramas representan las mediatrices entre los puntos vecinos como puede comprobarse en la siguiente imágen. 
Las intersecciones de estas mediatrices son una buena aproximación a los puntos más alejado de sus vecinos. Esta es una manera computacionalmente rápida de calcularlos.

![Voronoi](http://www.ams.org/featurecolumn/images/august2006/diagramintro.1.jpg) 


2. Se calcula la distancia de los puntos calculados al local más cercano y se guarda en una tabla.Para calcular esta distancia se utiliza el algortimo `k-nearest neighbor`. Este algoritmo es más rápido que calcular la distancia de todos los puntos respecto a todos los locales.
3. Se ordena la tabla anterior descendentemente. De esta manera tenemos en primer lugar los puntos más alejados de todos y sólo queda ir dibujando los puntos en orden.


####Representación de la información, tablas y gráficos.

El tratamiento para la representación de la información para mostrar las diferentes tablas y gráficos que se pueden obtener en la aplicación se realiza mediante el filtrado y agrupamiento de las tablas obtenidas en los puntos anteriores, pasándolo a un formato que sea entendido tanto por Shiny como por Leaflet cuando ha sido necesario.




