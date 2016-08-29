######## SCRIPT 3: CONTEO DE SUPERFICIES EN LAS IMÁGENES MODIS Y AVHRR

####### MODIS

### Comentarios Previos: Los códigos de las imágenes del producto MOD10A1:
# 0 = Data Missing           --> NA
# 1 = No Decision            --> NA
# 11 = Night                 --> NA
# 25 = No Snow (Land)        --> Land
# 37 = Lake                  --> Lake
# 39 = Ocean                 --> Ocean
# 50 = Cloud                 --> NA
# 100 = Lake Ice             --> Lake ice
# 200 = Snow                 --> Snow
# 254 = Detector Saturated   --> NA
# 255 = Fill                 --> NA

### MODIS (CIFRAS TOTALES)

## Paso 1. Directorio de Trabajo
setwd('c:/TRABAJO_SIG/TFM/')

## Paso 2. Bibliotecas
library(raster)
library(tiff)

## Paso 3.1. Listado de imágenes TIFF del Pirineo
tiflist=list.files('./Imagenes_MOD10A1/03_recorte/')

## Paso 4.1. Cálculo de la frecuencia (de celdillas) de cada una de las cubiertas
a_Pirineo = array(NA, dim=c(length(tiflist)*11,4))
contador = 0

for(i in 1:length(tiflist)){
  print(paste(i, 'de', length(tiflist)))
  resultado=freq(raster(paste('./Imagenes_MOD10A1/03_recorte/',tiflist[i],sep='')))
  for(j in resultado[,1]){ #J es cada uno de los códigos de MOD10A1
    print(paste('Cubierta con ID ',j,sep=''))
    contador = contador + 1
    a_Pirineo[contador,1]
    w=which(resultado==j)
    cobertura_i=cbind(tiflist[i], j, resultado[w[1],2], round(((resultado[w[1],2])/((741*241)))*100,4))
    a_Pirineo[contador,] = cobertura_i
  } # Nos exporta el número de celdas y el porcentaje con respecto del área de estudio.
}

write.table(a_Pirineo, './Series/01_MODIS_TODASCUBIERTAS_sinrellenar_Pirineo_2002_2015.txt', sep='\t', dec= ',')
write.csv(a_Pirineo, './Series/01_MODIS_TODASCUBIERTAS_sinrellenar_Pirineo_2002_2015.csv')

## Paso 3.2. Listado de imágenes TIFF de Sierra Nevada - Filabres
tiflist=list.files('./Imagenes_MOD10A1_G/03_recorte/')

## Paso 4.2. Cálculo de la frecuencia (de celdillas) de cada una de las cubiertas
a_Sierranevada = array(NA, dim=c(length(tiflist)*11,4))
contador = 0

for(i in 1:length(tiflist)){
  print(paste(i, 'de',length(tiflist)))
  resultado=freq(raster(paste('./Imagenes_MOD10A1_G/03_recorte/',tiflist[i],sep='')))
  for(j in resultado[,1]){ #J es cada uno de los códigos de MOD10A1
    print(paste('Cubierta con ID ',j,sep=''))
    contador=contador+1
    a_Sierranevada[contador,1]
    w = which(resultado==j)
    cobertura_i=cbind(tiflist[i], j, resultado[w[1],2], round(((resultado[w[1],2])/((251*141)))*100,4))
    a_Sierranevada[contador,] = cobertura_i
  } # Nos exporta el número de celdas y el porcentaje con respecto del área de estudio.
}

write.table(a_Sierranevada, './Series/01_MODIS_TODASCUBIERTAS_sinrellenar_Sierranevada.txt', sep='\t', dec= ',')
write.csv(a_Sierranevada, './Series/01_MODIS_TODASCUBIERTAS_sinrellenar_Sierranevada.csv')

### MODIS (CIFRAS DE NIEVE POR ALTITUDES)

## Paso 1. Directorio de trabajo
setwd('c:/TRABAJO_SIG/TFM')

## Paso 2. Bibliotecas
library(raster)

## Paso 3. Modelos Digitales de elevaciones reclasificados
# 1 = menos de 800 msnm
# 2 = entre 800 y 1500 msnm
# 3 = entre 1500 y 2500 msnm
# 4 = mas de 2500 msnm

mdtmodis_pi = raster('./MDT/04_rangos/mdtMODIS_pi_r.tif') #MDT PIRINEO
mdtmodis_sn = raster('./MDT/04_rangos/mdtMODIS_sn_r.tif') #MDT SIERRA NEVADA

## Paso 4.1 Listado imágenes MODIS del Pirineo
lista_modis = list.files("./Imagenes_MOD10A1/03_recorte/", pattern='.tif')
Tabla_comparativa = array(NA, dim=c(length(lista_modis),5)) #Crea una matriz de 11 columnas y tantas filas como imagenes 

contador = 0

for (i in 1:length(lista_modis)){
  print(paste(i,'de',length(lista_modis),"pares"))
  
  #Parte modis. El código = 200 es el de la nieve.
  modis = raster(paste('./Imagenes_MOD10A1/03_recorte/',lista_modis[i],sep=''))
  intervalo1_M = c(-1000, 199, NA)
  intervalo2_M = c(199, 201, 1)
  intervalo3_M = c(202, 2500, NA)
  m_M = c(intervalo1_M,  intervalo2_M,  intervalo3_M)
  rclmat_M = matrix(m_M, ncol=3, byrow=TRUE)
  modis_rec = reclassify(modis,rclmat_M,right=FALSE, overwrite=TRUE)
  zonal_modis = zonal(modis_rec, mdtmodis_pi,fun='count')
  
  #Añadir a un .csv los valores "zonal_modis"
  contador = contador + 1
  Tabla_comparativa[contador,1]
  #aqui se ponen los campos
  zonal_modis
  cobertura_i=cbind(lista_modis[i],zonal_modis[1,2], zonal_modis[2,2], zonal_modis[3,2], zonal_modis[4,2])
  Tabla_comparativa[contador,] = cobertura_i
}

write.table(Tabla_comparativa, './Series/07_MODIS_Pirineo_alturas.txt', sep='\t', dec= ',')
write.csv(Tabla_comparativa, './Series/07_MODIS_Pirineo_alturas.csv')

## Paso 4.2 Listado imágenes MODIS de Sierra Nevada - Filabres
lista_modis = list.files("./Imagenes_MOD10A1_G/03_recorte/", pattern='.tif')
Tabla_comparativa = array(NA, dim=c(length(lista_modis),5)) #Crea una matriz de 11 columnas y tantas filas como pares de imagenes

contador = 0

for (i in 1:length(lista_modis)){
  print(paste(i,'de',length(lista_modis),"pares"))
  
  #Parte modis. El código = 200 es el de la nieve.
  modis = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',lista_modis[i],sep=''))
  intervalo1_M = c(-1000, 199, NA)
  intervalo2_M = c(199, 201, 1)
  intervalo3_M = c(202, 2500, NA)
  m_M = c(intervalo1_M,  intervalo2_M,  intervalo3_M)
  rclmat_M = matrix(m_M, ncol=3, byrow=TRUE)
  modis_rec = reclassify(modis,rclmat_M,right=FALSE, overwrite=TRUE)
  zonal_modis = zonal(modis_rec, mdtmodis_sn,fun='count')
  
  #Añadir a un .csv los valores de "zonal_modis"
  contador = contador + 1
  Tabla_comparativa[contador,1]
  #aqui se ponen los campos
  zonal_modis
  cobertura_i=cbind(lista_modis[i],zonal_modis[1,2], zonal_modis[2,2], zonal_modis[3,2], zonal_modis[4,2])
  Tabla_comparativa[contador,] = cobertura_i
}

write.table(Tabla_comparativa, './Series/07_MODIS_Nevada_alturas.txt', sep='\t', dec= ',')
write.csv(Tabla_comparativa, './Series/07_MODIS_Nevada_alturas.csv')

####### AVHRR

#### Obtener número de ND total:

## Paso 1. Directorio de Trabajo
setwd('c:/TRABAJO_SIG/TFM/')

## Paso 2. Bibliotecas
library(raster)
library(tiff)

## Paso 3.1 Listado de Imágenes TIFF del Pirineo
tiflist=list.files('./AVHRR/06_Pirineo/')

## Paso 4.1 Cálculo de la frecuencia de ND
a_Pirineo = array(NA, dim=c(length(tiflist),2))

for(i in 1:length(tiflist)){
  print(paste(i, 'de',length(tiflist)))
  a_Pirineo[i,1]
  resultado=freq(raster(paste('./AVHRR/06_Pirineo/',tiflist[i],sep='')))
  #print(resultado) #Esto me da el numero de celdas de cada superficie.
  w = length(resultado)/2   #Encontrar la posición en la que se encuentra el "NA"
  cobertura_i=cbind(round(((resultado[w[1],2])/((36288)))*100,2), tiflist[i]) #36288 es el numero total de celdas por imagen pirenaica
  #print(cobertura_i)
  a_Pirineo[i,]<-cobertura_i
}

write.table(a_Pirineo, './Series/04_AVHRR_NODATA_Pirineo_1981_2015.txt', sep='\t', dec= ',')
write.csv(a_Pirineo, './Series/04_AVHRR_NODATA_Pirineo_1981_2015.csv')

## Paso 3.2 Listado de Imágenes TIFF de Sierra Nevada - Filabres
tiflist=list.files('./AVHRR/06_Nevada/')

## Paso 4.2 Cálculo de la frecuencia de ND
a_Sierranevada = array(NA, dim=c(length(tiflist),2))

for(i in 1:length(tiflist)){
  print(paste(i, 'de',length(tiflist)))
  a_Sierranevada[i,1]
  resultado=freq(raster(paste('./AVHRR/06_Nevada/',tiflist[i],sep='')))
  #print(resultado) #Esto me da el numero de celdas de cada superficie.
  w = length(resultado)/2   #Encontrar la posición en la que se encuentra el "NA"
  cobertura_i=cbind(round(((resultado[w[1],2])/((7119)))*100,2), tiflist[i])  #7119 es el numero total de celdas por imagen sierra nevada
  #print(cobertura_i)
  a_Sierranevada[i,]<-cobertura_i
}

write.table(a_Sierranevada, './Series/04_AVHRR_NODATA_Sierranevada_1981_2015.txt', sep='\t', dec= ',')
write.csv(a_Sierranevada, './Series/04_AVHRR_NODATA_Sierranevada_1981_2015.csv')


#### AVHRR (NIEVE POR ALTITUDES):

## Paso 1. Directorio de trabajo
setwd('c:/TRABAJO_SIG/TFM')

## Paso 2. Bibliotecas:
library(raster)

## Paso 3. Modelos Digitales de elevaciones reclasificados
# 1 = menos de 800 msnm
# 2 = entre 800 y 1500 msnm
# 3 = entre 1500 y 2500 msnm
# 4 = mas de 2500 msnm

mdtavhrr_pi = raster('./MDT/04_rangos/mdtAVHRR_pi_r.tif') #MDT PIRINEO
mdtavhrr_sn = raster('./MDT/04_rangos/mdtAVHRR_sn_r.tif') #MDT SIERRA NEVADA

## Paso 4.1 Nieve por altitudes en Pirineo (código = 1 es nieve)

lista_avhrr = list.files('./AVHRR/06_Pirineo/', pattern = '.tif')
tabla_avhrr = array(NA, dim=c(length(lista_avhrr),6)) #tabla con dos variables (el nombre de la imagen, y el numero de metros cuadrados de nieve)

contador = 0

for (i in 1:length(lista_avhrr)){
  print(paste(i,'de',length(lista_avhrr),"imagenes avhrr"))
  
  #Parte avhrr
  avhrr = raster(paste('./AVHRR/06_Pirineo/',lista_avhrr[i],sep=''))
  intervalo1_A = c(-1, 0, NA)
  intervalo2_A = c(0, 1, 1)
  intervalo3_A = c(1, 2, NA)
  m_A = c(intervalo1_A,  intervalo2_A,  intervalo3_A)
  rclmat_A = matrix(m_A, ncol=3, byrow=TRUE)
  avhrr_rec = reclassify(avhrr,rclmat_A,right=TRUE, overwrite=TRUE)
  zonal_avhrr = zonal(avhrr_rec, mdtavhrr_pi,fun='count')
  
  #Añadir a un .csv los valores de "zonal_avhrr"
  contador = contador + 1
  tabla_avhrr[contador,1]
  #aqui se ponen los campos
  cobertura_i=cbind(lista_avhrr[i],(zonal_avhrr[1,2])*(1098.681055*1098.681055),(zonal_avhrr[2,2])*(1098.681055*1098.681055),(zonal_avhrr[3,2])*(1098.681055*1098.681055),(zonal_avhrr[4,2])*(1098.681055*1098.681055),((zonal_avhrr[1,2])+(zonal_avhrr[2,2])+(zonal_avhrr[3,2])+(zonal_avhrr[4,2]))*(1098.681055*1098.681055))
  tabla_avhrr[contador,] = cobertura_i
}

write.table(tabla_avhrr, './Series/06_Pirineo_AVHRR_Seriecompleta.txt', sep='\t', dec= ',')
write.csv(tabla_avhrr, './Series/06_Pirineo_AVHRR_Seriecompleta.csv')

## Paso 4.2 Nieve por altitudes en Sierra Nevada - Filabres (código = 1 es nieve)

lista_avhrr = list.files('./AVHRR/06_Nevada/', pattern = '.tif')
tabla_avhrr = array(NA, dim=c(length(lista_avhrr),6)) #tabla con dos variables (el nombre de la imagen, y el numero de metros cuadrados de nieve)

contador = 0

for (i in 1:length(lista_avhrr)){
  print(paste(i,'de',length(lista_avhrr),"imagenes avhrr"))
  
  #Parte avhrr
  avhrr = raster(paste('./AVHRR/06_Nevada/',lista_avhrr[i],sep=''))
  intervalo1_A = c(-1, 0, NA)
  intervalo2_A = c(0, 1, 1)
  intervalo3_A = c(1, 2, NA)
  m_A = c(intervalo1_A,  intervalo2_A,  intervalo3_A)
  rclmat_A = matrix(m_A, ncol=3, byrow=TRUE)
  avhrr_rec = reclassify(avhrr,rclmat_A,right=TRUE, overwrite=TRUE)
  zonal_avhrr = zonal(avhrr_rec, mdtavhrr_sn,fun='count')
  
  #Añadir a un .csv los valores de "zonal_avhrr"
  contador = contador + 1
  tabla_avhrr[contador,1]
  #aqui se ponen los campos
  cobertura_i=cbind(lista_avhrr[i],(zonal_avhrr[1,2])*(1098.681055*1098.681055),(zonal_avhrr[2,2])*(1098.681055*1098.681055),(zonal_avhrr[3,2])*(1098.681055*1098.681055),(zonal_avhrr[4,2])(1098.681055*1098.681055),((zonal_avhrr[1,2])+(zonal_avhrr[2,2])+(zonal_avhrr[3,2])+(zonal_avhrr[4,2]))*(1098.681055*1098.681055))
  tabla_avhrr[contador,] = cobertura_i
}

write.table(tabla_avhrr, './Series/06_Nevada_AVHRR_Seriecompleta.txt', sep='\t', dec= ',')
write.csv(tabla_avhrr, './Series/06_Nevada_AVHRR_Seriecompleta.csv')

