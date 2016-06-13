---
title: "Memoria del Trabajo Fin de Master"
author: "Cesar Gonzalez"
date: "14 de junio de 2016"
output: html_document
---


##Introducci�n

La finalidad del Trabajo Fin de M�ster realizado consiste en una aplicaci�n que intenta responder a la pregunta: Si quisiera montar un negocio: �D�nde ser�a una buena zona para ubicarlo?

Por supuesto, el �xito de un negocio depende de factores internos, como una buena gesti�n, trato al cliente, etc. Pero en la mayor�a de negocios existen factores externos que condicionan el �xito o fracaso del mismo. Estos factores pueden ser el n�mero de habitantes de la zona, su renta media, la distribuci�n de la edad de la poblaci�n, la existencia o no de competencia, etc.

L�gicamente cada negocio es distinto y tiene sus propias caracter�sticas, afectando estos factores de manera distinta a cada uno de ellos. Por este motivo no se puede dar una respuesta �nica ni 100% fiable a la pregunta inicial, pero la importania de estos datos se demuestra en el hecho de que en cualquier plan de negocio se exige que se tengan en cuenta estos factores, el llamado *Entorno econ�mico y sociocultural*

Con la elaboraci�n de este Trabajo se intenta ofrecer una ayuda a la importante toma de decisi�n que supone la ubicaci�n de un negocio y ofrecer datos fiables a la hora de realizar un plan de negocio o un estudio de mercado.

Se puede acceder a la aplicaci�n desde la direcci�n <http://cescbox.ddns.net:24445/>. En la propia p�gina existe una secci�n de ayuda con tutoriales y videos explicativos.

##Uso de la aplicaci�n

El uso de la aplicaci�n es muy sencillo. Tiene tres secciones fundamentales:

* Un mapa donde se dibujar�n los locales de los ep�grafes elegidos. Adem�s muestra informaci�n de renta y de distribuci�n de la poblaci�n. S�lo se tiene que seleccionar el Ep�grafe deseado para empezar a ver todos los locales correspondientes a ese Ep�grafe en Madrid. Si se selecciona un distrito veremos s�lo los locales de ese distrito y un gr�fico con la distribuci�n de la edad de ese distrito. Finalmente, pinchando en _Mostrar puntos mas distantes_ podremos ver los puntos m�s alejados de la competencia, pudiendo elegir el n�mero de puntos mostrados. El tama�o de los puntos est� relacionado directamente con la distancia a la competencia.
* Un mapa en el que se muestra informaci�n de ingresos econ�mimcos de diversos tipos de negocio. En �l podremos ver cu�les son los negocios con m�s ingresos y cu�les son las zonas desde las que se desplazan sus habitantes en busca de un negocio.
* Una secci�n con informes desde la que se pueden ver los datos en bruto para un an�lisis m�s global.


##Datos de entrada

Para la realizaci�n del trabajo se han utilizado diversas fuentes tanto p�blicas como privadas. La mayor�a de los datos de poblaci�n se han descargado desde la plataforma de datos abiertos de la Comunidad de Madrid <http://datos.madrid.es/portal/site/egob>

Otros datos han sido obtenidos de la API del BBVA <https://www.bbvaapimarket.com/web/api_market/>. Estos datos no pueden ser publicados por lo que s�lo se har� una descripci�n de los mismos.

###Tablas p�blicas

Debido a que la mayor�a de las tablas utilizadas son p�blicas y adem�s son muy pesadas, se va a indicar la p�gina desde las que se pueden descargar en lugar de publicarlas.


* Censo de locales y sus actividades. Descargada desde <http://datos.madrid.es/sites/v/index.jsp?vgnextoid=66665cde99be2410VgnVCM1000000b205a0aRCRD&vgnextchannel=20d612b9ace9f310VgnVCM100000171f5a0aRCRD>. Contiene los datos de todos los locales de Madrid y sus actividades por Ep�grafe, as� como su direcci�n y coordenadas en formato ED50.

* Callejero vigente. Descargado desde <http://datos.madrid.es/sites/v/index.jsp?vgnextoid=f1555cde99be2410VgnVCM1000000b205a0aRCRD&vgnextchannel=20d612b9ace9f310VgnVCM100000171f5a0aRCRD>. Este listado contiene datos geogr�ficos y administrativos de todos los portales y edificios de Madrid. Esta informaci�n fu� necesaria utilizarla debido a que la anterior tabla ten�a coordenadas err�neas y en ocasiones inexistentes. De ella se extraen las coordenadas (en formato DMS) y los c�digos postales.

* Padr�n municipal. Descargado desde <http://datos.madrid.es/sites/v/index.jsp?vgnextoid=1d755cde99be2410VgnVCM1000000b205a0aRCRD&vgnextchannel=20d612b9ace9f310VgnVCM100000171f5a0aRCRD>. Tabla con un desglose por edades de todos los habitantes de Madrid por distrito, secci�n censal, etc.

* Datos de renta de los hogares. Descargada desde <http://www.madrid.es/portales/munimadrid/es/Inicio/El-Ayuntamiento/Estadistica/Areas-de-informacion-estadistica/Economia/Renta/Cuentas-de-renta-del-sector-Hogares?vgnextfmt=default&vgnextoid=e5561869a5cd0510VgnVCM2000000c205a0aRCRD&vgnextchannel=ef863636b44b4210VgnVCM2000000c205a0aRCRD>. Fichero de Excel desde el que obtenemos la informaci�n de la renta media de cada distrito.

###Shapefiles

Para la divisi�n de los mapas en Distritos y C�digos Postales se utilizaron los siguientes shapefiles:

* Shapefile de Distritos.
* Shapefile de C�digos Post�les

Ambos descargados desde la p�gina <http://www.madrid.org/nomecalles/DescargaBDTCorte.icm>. Estos shapes tienen la informaci�n geogr�fica con el sistema de referencia ED50.

###Datos de la API del BBVA

El BBVA tiene una API que ofrece informaci�n relativa al pago realizado mediante tarjeta bancaria en los distintos negocios de la Comunidad de Madrid. Esta API est� en fase Alpha y para acceder a sus servicios nos pusimos en contacto con el responsable de la misma para solicitar el acceso. Al no ser informaci�n p�blica no se subir�n las tablas obtenidas, pero se pueden ver los datos en la propia aplicaci�n.

La informaci�n de pago est� agregada por C�digo Postal y por categor�as en lugar de por Ep�grafes (por ejemplo "Bares y Restaurantes""). 
Por este motivo no ha sido posible integrarlos en un s�lo mapa con los datos anteriores como hubiera sido lo deseable. Sin embargo, esta informaci�n tambi�n es importante, por lo que se decidi� incluirla en un mapa separado.

Estos datos no muestran el gasto total de cada negocio al ser s�lo datos de pago mediante tarjeta bancaria. Sin embargo, por la ley de los grandes n�meros, se puede concluir que los negocios con m�s ventas con tarjeta de cr�dito son los que m�s ventas tienen en general. Del mismo modo, si una zona tiene m�s ventas mediante tarjeta que otra, se puede concluir que tendr� tambi�n m�s ingresos totales.

De la API del BBVA se obtuvieron las siguientes tablas:

* Una tabla con todas las categor�as y subcategor�as de negocios de las que se tienen datos.
* Otra tabla con el top 100 de localizaciones de residencia de clientes. Esta tabla se utiliza para ver cuales son los C�digos Post�les con mayores ingresos y tambi�n cu�les son los habitantes que m�s se desplazan a la hora de pagar (si de un determinado C�digo Postal se desplazan muchos habitantes es posible que falten servicios en esa zona.). La petici�n a este servicio se realiza por categor�a o subcategor�a de negocio, por lo que fu� necesario hacer una llamada por cada subcategor�a y guardar toda la informaci�n en una �nica tabla. La informaci�n sobre este y otros servicios puede encontrarse en <https://www.bbvaapimarket.com/web/api_market/bbva/paystats/documentation>

##Tratamiento de los datos.

La aplicaci�n se ha implementado utilizando R, Shiny y Leaflet entre otras tecnolog�as. La idea es ofrecer un servicio web que muestre informaci�n en un tiempo razonable. Por este motivo se dividi� el trabajo en dos archivos. El primero realizado en R, dedicado exclusivamente al tratamiento, limpieza y preparaci�n de los datos. El segundo, realizado en Shiny, obtiene los datos anteriores y los utiliza para una r�pida visualizaci�n de los mismos. Sin embargo desde este archivo se realizan tambi�n algunos c�lculos complejos como se detalla m�s adelante.

####Tabla de locales.

Esta tabla tiene muchas columnas innecesarias, por lo que lo primero es seleccionar s�lo las que son de inter�s. 
Existen muchas actividades en esta tabla, algunas de ellas no tienen mucho sentido incluirlas en este trabajo, como grandes industrias, por ejemplo. En lugar de ellos nos hemos centrado en los negocios que requieren de un local comercial. Por este motivo se han filtrado las actividades dejando s�lo las correspondientes a venta al por menor, hosteler�a e inmobiliarias. Por supuesto si se estuviese interesado en alguna actividad adicional ser�a tan sencillo como a�adir dicha actividad al filtro.
Despu�s eliminamos los locales cerrados o que no tiene actividad declarada.

####Tabla de C�digos Post�les.

Esta tabla se utlizar� para enriquecer la tabla anterior con las coordenadas y C�digos Post�les de los locales filtrados. Esto se realizar� mediante un Join, por lo que lo primero que se hace es cambiar el nombre de las columnas para que coincidcidan con los de la tabla anterior y se normalizan los campos de direcci�n que est�n tambi�n en un formato distinto. Una vez realizada la limpieza de ambas tablas se unen mediante un Join de manera que a cada local se le agregan su c�digo postal y sus coordenadas. 

####Funci�n degreetodecimal.

Debido a que las coordenadas de la tabla anterior se encuentran en formato DMS, se implementa una funci�n para pasarlas a formato decimal.

####Funci�n getlanlonAPI.

A pesar de especificar la codificaci�n de las tablas con el formato "Latin-1", al realizar el Join de las dos primneras las direcciones correspondientes a calles con caracteres como la "�" aparecieron con campos nulos en las coordenadas y el c�digo postal. Estas direcciones eran unas 100, por lo que se implement� una funci�n para recopilar esta informaci�n de la API de Google. �sta API permite hasta 2500 llamadas al d�a de manera gratuita y s�lo es necesario llamarla una vez, por lo que se cumple con las Condiciones de Uso.

####Tabla de edad de poblaci�n.

Esta es la tabla m�s limpia de todas. Para una correcta visualizaci�n de sus datos se decidi� mostrar la distribuci�n de la edad por decenas en vez de por a�os. Por este motivo s�lo fu� necesario incluir un campo en el que se a�ade la decena de la edad de los habitantes y hacer una agregaci�n por este campo.

####Tabla con los datos de renta.

Al ser una tabla con formato Excel, se tuvo que eliminar las filas y columnas que no interesaban as� como sustituir caracteres que no se mostraban correctamente como los acentos y dar un nombre adecuado a las columnas

####API del BBVA.

Se hace una llamada para recibir un token. A continuaci�n se utiliza este token para hacer una petici�n de las categor�as de negocio existentes. Una vez obtenidas todas las categor�as se hace otra llamada para obtener el top 100 de gasto por C�digo Postal para cada categor�a y se guarda en una tabla. Este proceso tarda unos 40 minutos en realizarse y es necesario tenerlo precalculado para tener los datos de los habitantes que m�s se desplazan por categor�a.

####C�lculo de las zonas m�s alejadas de la competencia.

Una de las caracter�sticas m�s importantes de �ste trabajo y el motivo inicial por el que se decidi� realizarlo es la posibilidad de calcular la zona o zonas m�s alejada del resto de la competencia.
El c�lculo de estas zonas se realiza en distintas fases:

1. Se calculan los puntos m�s alejados de los locales.Este c�lculo se realiza mediante `Diagramas de Voronoi` <https://en.wikipedia.org/wiki/Voronoi_diagram>. Estos diagramas representan las mediatrices entre los puntos vecinos como puede comprobarse en la siguiente im�gen. 
Las intersecciones de estas mediatrices son una buena aproximaci�n a los puntos m�s alejado de sus vecinos. Esta es una manera computacionalmente r�pida de calcularlos.

![Voronoi](http://www.ams.org/featurecolumn/images/august2006/diagramintro.1.jpg) 


2. Se calcula la distancia de los puntos calculados al local m�s cercano y se guarda en una tabla.Para calcular esta distancia se utiliza el algortimo `k-nearest neighbor`. Este algoritmo es m�s r�pido que calcular la distancia de todos los puntos respecto a todos los locales.
3. Se ordena la tabla anterior descendentemente. De esta manera tenemos en primer lugar los puntos m�s alejados de todos y s�lo  queda ir dibujando los puntos en orden.


####Representaci�n de la informaci�n, tablas y g�ficos.

El tratamiento para la representaci�n de la informaci�n para mostrar las diferentes tablas y gr�ficos que se pueden obtener en la aplicaci�n se realiza mediante el filtrado y agrupamiento de las tablas obtenidas en los puntos anteriores, pas�ndolo a un formato que sea entendido tanto por Shiny como por Leaflet cuando ha sido necesario.




