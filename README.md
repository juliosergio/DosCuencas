---
title: "README"
author: "Julio Sergio Santana"
date: "09/02/2017"
output: html_document
---
# INTRO
En este documento doy algunas pistas de cómo generar la información del  
proyecto.

# PRODUCCIÓN DE LA INFORMACIÓN
En seguida se muestra el diagrama de flujo de la información, hasta llegar 
a los _mapas_ con la relación de cambio del SPI entre dos períodos para una 
región dada.

**Figura 1.** Flujo de la información (como un mapa conceptual)

![Flujo de la info](MapaConceptual.jpg)

En esta figura se muestran los programas con sus respectivas entradas y salidas principales. En seguida 
se procede a explicar cada uno de los programas.

## 1 StationsView.R

### 1.1 Objetivo

Este programa tiene como objetivo digerir cualquiera de archivos NetCDF que contienen la información de la malla de CLICOM
producida para obtener recortes de la información en un formato más legible, CSV en este caso.

### 1.2 Entradas

Este programa tiene como entrada dos archivos con información, a saber:

1. **\<fuente-CLICOM-malla\>.nc**. Es un archivo en formato netCDF que contiene la descripción malla de alguna de las variables
climáticas: precipitación, temaperatura máxima, o temperatura mínima, en el territorio nacional. En este caso se trata del 
desarrollo que hiciera el CICESE con datos de la base de datos CLICOM, administrada por el Servicio Meteorológico Nacional.

2. **\<Frontera-Cuenca\>.csv**. Descripción de la frontera de la cuenca (o alguna región especifica), en formato CSV. La tabla 
debe contener por lo menos dos columnas: una etiquetada con el texto "Lon", y la otra, con el texto "Lat", que corresponden 
a las coordenadas geográficas, Longitud y Latitud, respectivamente.

### 1.3 Salidas

El programa entrega como salida un único archivo:

* **\<Var-Cuenca-Pts-Ts\>.csv**. Es un archivo, cuyas columnas son las series de tiempo de cada uno de los puntos (de malla), 
contenidos en la región descrita por la frontera de la cuenca (o área) provista, el primero y segundo renglón de cada una 
de la columnas contiene las coordenadas geográficas del punto correspondiente a la columna y a partir del tercer renglón 
se registran, en orden, cada uno de los valores de la variable hasta finalizar las series de tiempo.

### 1.4 Funcionamiento

Al inicio del programa pregunta interactivamente por el nombre del archivo netCDF que se desea procesar. 
Dicho archivo, contiene la variable en cuestión como un *hiper*cubo, a lo largo de tres dimensiones, en
este caso, la longitud, la latitud, y el tiempo. Así, la primer tarea de este programa es *desdoblar* esa información como una
tabla con n *columnas*, la longitud, la latitud, y cada uno de los valores de variable a lo largo del tiempo, esto es, para 
cada uno de los puntos de la malla, en el renglón se anotan la longitud, la latitud y toda la serie de tiempo de la variable
para ese punto. La segunda tarea es conservar de los puntos solamente los que se encuentran dentro del
área especificada como límite. El resultado de estas operaciones, que consiste en la transposición de la tabla descrita 
antes, se entrega como se ha mencionado en la salida \<Var-Cuenca-Pts-Ts\>.csv.

## 2 TandemSPI.R

### 2.1 Objetivo

A partir de una tabla de series de tiempo de precipitaciones mensuales, para un conjunto de puntos y que se apega al formato 
CSV descrito como la salida \<Var-Cuenca-Pts-Ts\>.csv, en la sección 1.3, produce, para una partición de la serie de tiempo 
original en dos series, dos tablas que contienen series de tiempo de los SPIs calculados para cada punto, así como los datos para la producción de histogramas de densidad de probabilidades para cada una de las series de tiempo de SPIs.

### 2.2 Entradas

El programa tiene como entrada un único archivo:

* **\<Var-Cuenca-Pts-Ts\>.csv**. Este archivo se apega al formato descrito en la sección 1.3; sin embargo, no se trata de 
cualquiera de los archivos que salen de esa estapa, sino espicíficamente de archivos de precipitación con datos mensuales durante 
algún período de varios años.

### 2.3 Salidas

El programa tiene como salida dos archivos, a saber:

1. **\<Prefix\>\_SPI0.csv**. Este archivo contine la primera mitad de la serie de tiempo de los SPIs calculados, así como los 
datos para la construcción del correspondiente histograma.

2. **\<Prefix\>\_SPI1.csv**. Este archivo contine la segunda mitad de la serie de tiempo de los SPIs calculados, así como los 
datos para la construcción del correspondiente histograma.

### 2.4 Funcionamiento

Al inicio, el programa pregunta interactivamente por el archivo que contiene los datos, esta información sirve no sólo para 
abrir el archivo en cuestión, sino para construír los nombres de los archivos de salida. Posteriormente, el programa pregunta 
también por el período en meses, en que se promedian los valores de precipitación previos cada mes. Típicamente, el archivo de 
entrada es alguno de los que se han producido como salida en la etapa descrita en la sección 1.3. El archivo contiene, 
como columnas, series mensuales de precipitación para un conjunto de puntos, cuyas coordenadas geográficas también se reportan como las dos primeras entradas de cada columna. Las series de tiempo son las mismas para todos los puntos considerados, así que de aquí 
en adelante se denominará únicamente como _la serie de tiempo_.   

La serie de tiempo, entonces, se particiona en dos subseries de tal manera que cada una de ellas consista en un número exacto e 
igual de años completos. Los meses sobrantes, que se considerarán al principio de la serie original, podrán ser utilizados de 
todas maneras para el cálculo de los promedios previos requeridos en la determinación de los SPIs.

Para el cálculo de los SPIs, se consideran las dos subseries consecutivas, como si fuera una sola serie y se calculan por medio 
de funciones modificadas para su cálculo a partir del código desarrollado por Joseph Wheatley (Biospherica, March 2010: 
http://joewheatley.net/2010/03/). El resultado es una tabla con las series de tiempo mensuales para cada uno de los puntos 
considerados. Esta tabla se particiona por la mitad, de acuerdo con las subseries de tiempo definidas previamente.

Finalmente, se hace una estadistica a las subseries de tiempo, para producir histogramas de densidad de probabilidades para 
los intervalos definidos por la _World Meteorological Organization_, para este índice, cuyas fronteras son: -3, -2, -1.5, 
-1, 1, 1.5, 2, 3.

Los archivos de salida contienen, para cada punto, la subserie de tiempo con los valores de los SPIs obtenidos, y al final, 
los valores para reproducir el histograma de densidad de probabilidades en cada caso.












