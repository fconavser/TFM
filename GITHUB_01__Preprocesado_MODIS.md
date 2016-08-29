######## SCRIPT 1: PREPROCESADO IMÁGENES MODIS

### Comentarios Previos: Previamente a la ejecución de este script ya se han descargado todas
# las imágenes en formato HDF para las dos áreas de estudio en las carpetas "'C:/TRABAJO_SIG
# /TFM/Imagenes_MOD10A1" (Pirineo) y "C:/TRABAJO_SIG/TFM/Imagenes_MOD10A1_G" (Sierra Nevada - Filabres)
# En el Pirineo habrá que hacer mosaicado, reproyección y recorte. En Sierra Nevada - Filabres
# habrá que hacer reproyección y recorte únicamente.

### Área 1: Pirineo (Mosaicado - Reproyección - Recorte)

## Paso 1. Directorio de Trabajo
setwd('c:/TRABAJO_SIG/TFM/Imagenes_MOD10A1')

## Paso 2. Ubicación de las funciones de ModisDownload.R y Modis Reprojection Tool
source('c:/modis/ModisDownload.R')
MRTpath='c:/MRT/MRT/bin'

## Paso 3. Bibliotecas
library(maptools)

## Paso 4. Listado de archivos HDF
hdflist = list.files('Imagenes/', pattern='.hdf$')

## Paso 5. Ejecución de la función de Mosaicado de imágenes.
# Las imágenes de distintas escenas de un mismo día comparten la fecha, ubicada entre los carac-
# teres 13 y 28 del nombre del archivo. Los archivos se generarán en una nueva ubicación.
for(i in 2:length(hdflist)){
  print(paste(i,'de',length(hdflist)))
  nam = substr(hdflist[i],13,28)
  nam_ant = substr(hdflist[i-1],13,28)
  if(nam==nam_ant){
    mosaicHDF(paste('./Imagenes/',hdflist[c(i,(i-1))],sep=''),
              paste('/01_mosaicos/',nam,'.hdf',sep=''),MRTpath)
  } else{
    file.copy(paste('./Imagenes/',hdflist[i],sep=''),paste('/01_mosaicos/',nam,'.hdf',sep=''),
              overwrite=T)
  }
}

## Paso 6. Listado de archivos HDF mosaicados
hdflist = list.files('01_mosaicos/', pattern='.hdf$')

## Paso 7. Ejecución de la función de Reproyección de imágenes.
# Las imágenes reproyectadas en formato TIFF se generarán en una nueva ubicación.
for(i in 1:length(hdflist)){
  print(paste(i,'de',length(hdflist)))
  nam = substr(hdflist[i],1,28)
  reprojectHDF(paste('/01_mosaicos/',hdflist[i], sep=''),paste('./02_reproyecta/',nam,'_r.tif',sep=''),MRTpath,UL="",LR="", resample_type='NEAREST_NEIGHBOR',proj_type='UTM',bands_subset='',proj_params='0 0 0 0 0 0 0 0 0 0 0 0',datum='WGS84',utm_zone=30,pixel_size=500)
}

## Paso 8. Bibliotecas
library(raster)
library(tiff)

## Paso 9. Listado de archivos TIFF reproyectados
tiflist = list.files('02_reproyecta/', pattern='.tif$')

## Paso 10. Ejecución de la función de Recorte de imágenes.
# El recorte se hace sobre las filas 1840-2080 y las columnas 2010-2750 (Pirineo).
# Se generarán en una nueva ubicación.
for(i in 1:length(tiflist)){
  print(paste(i,'de',length(tiflist)))
  nam = substr(tiflist[i],13,28)
  crop(raster(paste('02_reproyecta/',tiflist[i],sep='')), extent(raster(paste('02_reproyecta/',tiflist[1],sep='')),1840,2080,2010,2750), paste('03_recorte/',nam,'_clip.tif',sep=''), snap='near')
}

# Con estos pasos nos quedamos con las imágenes MOD10A1
# mosaicadas, reproyectadas y recortadas al área de estudio 1. Pirineos


### Área 2: Sierra Nevada - Filabres (Reproyección - Recorte)

## Paso 1. Directorio de Trabajo
setwd('c:/TRABAJO_SIG/TFM/Imagenes_MOD10A1_G')

## Paso 2. Ubicación de las funciones de ModisDownload.R y Modis Reprojection Tool
source('c:/modis/ModisDownload.R')
MRTpath='c:/MRT/MRT/bin'

## Paso 3. Bibliotecas
library(maptools)

## Paso 4. Listado de archivos HDF
hdflist = list.files('Imagenes/', pattern='.hdf$')

## Paso 5. Ejecución de la función de Reproyección de imágenes.
# Las imágenes reproyectadas en formato TIFF se generarán en una nueva ubicación.
for(i in 1:length(hdflist)){
  print(paste(i,'de',length(hdflist)))
  nam = substr(hdflist[i],13,28)
  reprojectHDF(paste('/Imagenes/',hdflist[i], sep=''),paste('./02_reproyecta/',nam,'_r.tif',sep=''),MRTpath,UL="",LR="", resample_type='NEAREST_NEIGHBOR',proj_type='UTM',bands_subset='',proj_params='0 0 0 0 0 0 0 0 0 0 0 0',datum='WGS84',utm_zone=30,pixel_size=500)
}

## Paso 6. Bibliotecas
library(raster)
library(tiff)

## Paso 7. Listado de archivos TIFF reproyectados
tiflist = list.files('02_reproyecta/', pattern='.tif$')

## Paso 8. Ejecución de la función de Recorte de imágenes.
# El recorte se hace sobre las filas 650-790 y las columnas 1600-1850 (Sierra Nevada - Filabres).
# Se generarán en una nueva ubicación.
for(i in 1:length(tiflist)){
  print(paste(i,'de',length(tiflist)))
  nam = substr(tiflist[i],1,16)
  crop(raster(paste('02_reproyecta/',tiflist[i],sep='')), extent(raster(paste('02_reproyecta/',tiflist[1],sep='')),650,790,1600,1850), paste('03_recorte/',nam,'_g_clip.tif',sep=''), snap='near')
}

# Con estos pasos nos quedamos con las imágenes MOD10A1_G
# reproyectadas y recortadas al área de estudio 2. Sierra Nevada - Filabres

