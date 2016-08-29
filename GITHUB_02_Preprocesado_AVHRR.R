######## SCRIPT 2: PREPROCESADO IMÁGENES AVHRR

### Comentarios Previos: Previamente a la ejecución de este script ya se han descargado todos
# los archivos en formato .NC en la carpeta "'C:/TRABAJO_SIG/TFM/AVHRR/01_Brutas/"
# Las escenas AVHRR de partida cubrian la totalidad de la Península Ibérica. Habrá que adaptar
# el archivo NC, convertirlo a TIFF, reproyectar y recortar a cada área de estudio.

### Rotación Previa:

## Paso 1. Directorio de Trabajo
setwd("c:/TRABAJO_SIG/TFM/AVHRR")

## Paso 2. Bibliotecas
install.packages("ncdf4")
library(ncdf4)

## Paso 3. Listado de archivos .NC
ncs = list.files("./01_Brutas/", pattern='.nc')

## Paso 4. Rotación de imágenes. Las imágenes venían de origen con las coordenadas XY mal indicadas
for (n in 1:length(ncs)) {
  
  nieve_nc = nc_open(paste("./01_Brutas/",ncs[n],sep=''), write=TRUE)
  
  # dimensiones
  lons = nieve_nc$dim[[2]]
  lats = nieve_nc$dim[[1]]
  times = nieve_nc$dim[[3]]
  lons$name = 'lon'
  lats$name = 'lat'
  
  # creamos un nuevo netCDF con la estructura correcta
  dir.create('./02_Reproyectadas/',showWarnings = F)
  filename = paste('./02_Reproyectadas/',"r_",ncs[n],".nc",sep='')
  var = ncvar_def(
    nieve_nc$var[[1]]$name,
    nieve_nc$var[[1]]$name,
    list(lon=lons,lat=lats,time=times),
    nieve_nc$var[[1]]$missval,
    nieve_nc$var[[1]]$longname,
    nieve_nc$var[[1]]$prec,
    nieve_nc$var[[1]]$shuffle,
    #nieve_nc$var[[1]]$compression,
    compression=6,
    nieve_nc$var[[1]]$chunksizes
  )
  new = nc_create(filename, var)
  
  # y colocamos los datos rotados
  nieve = ncvar_get(nieve_nc)
  nieve2 = array(NA,dim(nieve)[c(2,1,3)])
  for (i in 1:dim(nieve)[3]) {
    #		nieve2[,,i] = t(nieve[nrow(nieve):1,,i])
    nieve2[,,i] = t(nieve[,,i])
    nieve2[,,i] = nieve2[,ncol(nieve2):1,i]
  }
  ncvar_put(new, nieve_nc$var[[1]]$name, nieve2)
  nc_close(new)
}

### Conversión de .NC (1 archivo por año) a .TIFF (1 archivo por dia)

## Paso 1. Bibliotecas
library(raster)
library(rgdal)

## Paso 2. Listado de archivos .NC
listado_nc = list.files("./02_Reproyectadas/", pattern='.nc')

## Paso 3. Creamos directorio de exportación
dir.create('./03_GTIFF/',showWarnings = F)

## Paso 3. Conversión a TIFF
for(i in 1:length(listado_nc)){
  print(paste(i, 'de',length(listado_nc),'nc'))
  nc_yyyy = nc_open(paste("./02_Reproyectadas/",listado_nc[i],sep=''))
  lon = ncvar_get(nc_yyyy,'lon')    #longitud
  lat = ncvar_get(nc_yyyy,'lat')    #latitud
  datos = ncvar_get(nc_yyyy, 'Nieve')
  dim = dim(datos)
  for(j in 1:dim[3]){
    print(paste(j, 'de',dim[3],'dias'))
    k = datos[,,j]
    kras = raster(k)
    rotate = function(x) t(apply(x, 2, rev))
    kras2 = rotate(rotate(rotate(rotate(k))))
    diaria = rotate(rotate(rotate(kras2)))
    
    #creamos el raster dandole la proyeccion adecuada, en este caso
    #European Datum 1950 30N, que es la de partida.
    diaria = raster(diaria, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                    crs=CRS("+init=epsg:23030"))
    
    writeRaster(x = diaria,filename = paste('./03_GTIFF/',listado_nc[i],'_',j,'.tif',sep=''), format = 'GTiff', overwrite = TRUE)
  }
}

### Reproyección de ED50 UTM30N a WGS84 UTM30N

# Este paso se ejecutó mediante Model Builder en ArcGIS, guardando las nuevas imágenes reproyectadas
# en la carpeta: "c:/TRABAJO_SIG/TFM/AVHRR/04_WGS84/"

### Recorte de imágenes AVHRR.

## Paso 1. Bibliotecas
library(raster)
library(maptools)
library(tiff)

## Paso 2. Lista de archivos TIFF
tiflist = list.files('./04_WGS84/', pattern='.tif$')

## Área De Estudio 1. PIRINEO

## Paso 3.1.1 Imagen de recorte: la primera de las imágenes MODIS del Pirineo
modispirineolist=list.files('c:/TRABAJO_SIG/TFM/Imagenes_MOD10A1/02_reproyecta/', pattern='.tif', full.names=TRUE)
modis=raster(modispirineolist[1])

## Paso 3.1.2 Recorte de imagen al extent del Pirineo
for(i in 1:length(tiflist)){
  print(paste(i,'de',length(tiflist)))
  crop(raster(paste('./04_WGS84/',tiflist[i],sep='')), extent(modis,1840,2080,2010,2750), paste('05_Pirineo/',tiflist[i],'_pi.tif',sep=''), snap='in', overwrite=TRUE)
}

## Area de Estudo 2. SIERRA NEVADA - FILABRES

## Paso 3.2.1 Imagen de recorte: la primera de las imágenes MODIS de Sierra Nevada - Filabres
modisnevadalist=list.files('c:/TRABAJO_SIG/TFM/Imagenes_MOD10A1_G/02_reproyecta', pattern='.tif', full.names=TRUE)
modis=raster(modisnevadalist[1])

## Paso 3.2.2 Recorte de imagen al extent de Sierra Nevada - Filabres
for(i in 1:length(tiflist)){
  print(paste(i,'de',length(tiflist)))
  crop(raster(paste('./04_WGS84/',tiflist[i],sep='')), extent(modis,650,790,1600,1850), paste('05_Nevada/',tiflist[i],'_sn.tif',sep=''), snap='in', overwrite=TRUE)
}


### Renombrado de archivos, pasar de "1" a "001", de "15" a "015", etc.

## Paso 1. Listado de archivos TIFF Pirineo
tiflist=list.files('./05_Pirineo/', '.tif$')

## Paso 2. Renombrado de archivos cuando el número de caracteres es X.
for(i in 1:length(tiflist)){
  print(paste(i,'de ', length(tiflist)))
  if(nchar(tiflist[i])==28){  #aqui pongo un numero que signifique que no le faltan 00 antes de los numeros
    nam_1 = substr(tiflist[i], 9,12) #cogemos de la letra 9 a la letra 12 (año)
    nam_2 = substr(tiflist[i], 17,17) #cogemos la cifra del dia
    file.copy(from=paste('./05_Pirineo/',tiflist[i],sep=''), to=paste('./06_Pirineo/',nam_1,'_00',nam_2,'_pi.tif',sep=''),overwrite=TRUE)
  } else{
    if(nchar(tiflist[i])==29){
      nam_1 = substr(tiflist[i], 9,12) #cogemos de la letra 9 a la letra 12 (año)
      nam_2 = substr(tiflist[i], 17,18) #cogemos de la letra 12 a la 13 (la cifra del dia)
      file.copy(from=paste('./05_Pirineo/',tiflist[i],sep=''), to=paste('./06_Pirineo/',nam_1,'_0',nam_2,'_pi.tif',sep=''),overwrite=TRUE) 
    } else{
      if(nchar(tiflist[i])==30){
        nam_1 = substr(tiflist[i], 9,12) #cogemos de la letra 9 a la letra 12 (año)
        nam_2 = substr(tiflist[i], 17,19) #cogemos de la letra 12 a la 13 (la cifra del dia)
        file.copy(from=paste('./05_Pirineo/',tiflist[i],sep=''), to=paste('./06_Pirineo/',nam_1,'_',nam_2,'_pi.tif',sep=''),overwrite=TRUE) 
      }
    }
  }
}

## Paso 1. Listado de archivos TIFF Sierra Nevada - Filabres
tiflist=list.files('./05_Nevada/', '.tif$')

## Paso 2. Renombrado de archivos cuando el número de caracteres es X.
for(i in 1:length(tiflist)){
  print(paste(i,'de ', length(tiflist)))
  if(nchar(tiflist[i])==28){  #aqui pongo un numero que signifique que no le faltan 00 antes de los numeros
    nam_1 = substr(tiflist[i], 9,12) #cogemos de la letra 9 a la letra 12 (aÃ±o)
    nam_2 = substr(tiflist[i], 17,17) #cogemos la cifra del dia
    file.copy(from=paste('./05_Nevada/',tiflist[i],sep=''), to=paste('./06_Nevada/',nam_1,'_00',nam_2,'_sn.tif',sep=''),overwrite=TRUE)
  } else{
    if(nchar(tiflist[i])==29){
      nam_1 = substr(tiflist[i], 9,12) #cogemos de la letra 9 a la letra 12 (aÃ±o)
      nam_2 = substr(tiflist[i], 17,18) #cogemos de la letra 17 a la 18 (la cifra del dia)
      file.copy(from=paste('./05_Nevada/',tiflist[i],sep=''), to=paste('./06_Nevada/',nam_1,'_0',nam_2,'_sn.tif',sep=''),overwrite=TRUE) 
    } else{
      if(nchar(tiflist[i])==30){
        nam_1 = substr(tiflist[i], 9,12) #cogemos de la letra 9 a la letra 12 (aÃ±o)
        nam_2 = substr(tiflist[i], 17,19) #cogemos la cifra del dia
        file.copy(from=paste('./05_Nevada/',tiflist[i],sep=''), to=paste('./06_Nevada/',nam_1,'_',nam_2,'_sn.tif',sep=''),overwrite=TRUE) 
      }
    }
  }
}


### Con esto ya tenemos las imágenes recortadas para cada área de estudo en formato TIFF,
# reproyectadas a WGS84 UTM30N y con nombres adecuados.
