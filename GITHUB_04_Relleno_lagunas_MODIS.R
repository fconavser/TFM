######## SCRIPT 4: RELLENO DE LAGUNAS ESPACIALES EN MOD10A1 SIGUIENDO LA METODOLOGIA DE GASCOIN ET AL. (2015)

### Comentarios Previos: Para la ejecución de este script es necesario ejecutar de nuevo
###  el script "GITHUB_01_Preprocesado_MODIS", pero aplicándolo a los productos MYD10A1 (SAT AQUA)
### Para evitar la saturación, las modificaciones para MYD10A1 no se han añadido, pero es idéntico 
###  pero aplicándolo a las imágenes de este otro producto

### Las imágenes MYD10A1 se encuentran en directorios idénticos, pero con "MYD10A1" en vez de "MOD10A1"

### Códigos:
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

## Paso 1. Directorio de Trabajo
setwd("c:/TRABAJO_SIG/TFM")

## Paso 2. Bibliotecas
library(raster)

#### PIRINEO

## Paso 3. Listado de imágenes MOD10A1 (SAT TERRA) a rellenar
listado_terra = list.files('./Imagenes_MOD10A1/03_recorte/',pattern='.tif$')

## Paso 4. Listado de imágenes MYD10A1 (SAT AQUA) como posible relleno
listado_aqua = list.files('./Imagenes_MYD10A1/03_recorte/', pattern='tif$')

## Paso 5. Aplicación del relleno de lagunas.
# En primer lugar se rellenaran los píxeles catalogados como ND con MYD10A1. En caso de que
# igualmente en MYD10A1 sean catalogados como ND, se aplicará un relleno temporal.

for(i in 5:(length(listado_terra)-4)){ #recorre 1 a 1 todo el listado de las imagenes TERRA desde la quinta hasta la ultima-4
  print(paste(i,'de', length(listado_terra)))
  fecha = substr(listado_terra[i],10,16) #la fecha que debe ser común está entre el carácter 10 y el 16 de los ficheros tif
  w = which(fecha==substr(listado_aqua,10,16)) #Crea un objeto W en el que diga en qué posición del listado AQUA está la imágen de TERRA.
  
  if(length(w)==0){
    #next #si no hay fecha común entre el listado de TERRA y de AQUA, sigue al siguiente (función next), pero si hay fecha común aplica todo lo que sigue
    com = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i],sep='')) #crea un objeto "com" de tipo raster con el fichero TERRA que no tiene par en AQUA aún así.
  } else{
    #Leemos TERRA y creamos un objeto TERRA con la imágen del día analizado en el momento (i)
    terra = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i],sep=''))
    #nos quedamos solo con los pixeles que NO queremos mantener (binario 1=categorias que no nos interesan: cumplen condición,
    #                                                  0=categorias que nos interesan: no cumplen condición)
    terra2 = (terra!=200 & terra!=25 & terra!=37 & terra!=100)
    #inversa de la anterior (lo que si que queremos mantener)
    terra3 = (terra==200 | terra==25 | terra==37 | terra==100)
    #convertimos el binario que queremos mantener en sus valores originales (y 0 en el resto)
    terra4 = terra*terra3
    
    #leemos aqua
    aqua = raster(paste('./Imagenes_MYD10A1/03_recorte/',listado_aqua[w],sep=''))
    #nos quedamos con los valores de aqua que queriamos rellenar en terra
    com = aqua*terra2
    #rellenamos los huecos de aqua con los valores originales de terra
    com = com+terra4
    aqrel = com
    #writeRaster(com, filename=paste('./rellenadas/',paste('comb_',fecha,".tif",sep=''), format="GTiff",overwrite='TRUE')
  }
  
  #dia1
  #imagen rellenada con aqua
  com.1 = (com!=200 & com!=25 & com!=37 & com!=100)
  com.2 = (com==200 | com==25 | com==37 | com==100)
  #leemos di anterior
  terra.menos1 = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i-1], sep=''))
  #relleno con el dia anterior
  ct1 =com.1*terra.menos1
  com.2=com.2*com
  ct1 = ct1 + com.2
  #leemos dia siguiente
  terra.mas1 = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i+1],sep=''))
  #solo la nieve dle dia siguiente
  terra.mas1.2 = (terra.mas1==200)
  #lo que queremos rellenar del dia anterior
  ct2 = (ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  #dia anterior rellenado con el dia siguiente
  ct3 =ct2*terra.mas1.2
  ct3[ct3==1] = 200
  #conversion de binarios a valores originales
  t = ct3*(ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  t[t==200] = 10000
  #t es la imagen rellenada
  t = ct1 + t
  t[t>9000] = 200
  t1 = t
  
  #dia2
  com = t1
  #imagen rellenada con aqua
  com.1 = (com!=200 & com!=25 & com!=37 & com!=100)
  com.2 = (com==200 | com==25 | com==37 | com==100)
  #leemos di anterior
  terra.menos1 = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i-2],sep=''))
  #relleno con el dia anterior
  ct1 =com.1*terra.menos1
  com.2=com.2*com
  ct1 = ct1 + com.2
  #leemos dia siguiente
  terra.mas1 = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i+2],sep=''))
  #solo la nieve dle dia siguiente
  terra.mas1.2 = (terra.mas1==200)
  #lo que queremos rellenar del dia anterior
  ct2 = (ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  #dia anterior rellenado con el dia siguiente
  ct3 =ct2*terra.mas1.2
  ct3[ct3==1] = 200
  #conversion de binarios a valores originales
  t = ct3*(ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  t[t==200] = 10000
  #t es la imagen rellenada
  t = ct1 + t
  t[t>9000] = 200
  t2 = t
  
  #dia3
  com = t2
  #imagen rellenada con aqua
  com.1 = (com!=200 & com!=25 & com!=37 & com!=100)
  com.2 = (com==200 | com==25 | com==37 | com==100)
  #leemos di anterior
  terra.menos1 = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i-3],sep=''))
  #relleno con el dia anterior
  ct1 =com.1*terra.menos1
  com.2=com.2*com
  ct1 = ct1 + com.2
  #leemos dia siguiente
  terra.mas1 = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i+3],sep=''))
  #solo la nieve dle dia siguiente
  terra.mas1.2 = (terra.mas1==200)
  #lo que queremos rellenar del dia anterior
  ct2 = (ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  #dia anterior rellenado con el dia siguiente
  ct3 =ct2*terra.mas1.2
  ct3[ct3==1] = 200
  #conversion de binarios a valores originales
  t = ct3*(ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  t[t==200] = 10000
  #t es la imagen rellenada
  t = ct1 + t
  t[t>9000] = 200
  t3 = t
  
  #dia4
  com = t3
  #imagen rellenada con aqua
  com.1 = (com!=200 & com!=25 & com!=37 & com!=100)
  com.2 = (com==200 | com==25 | com==37 | com==100)
  #leemos di anterior
  terra.menos1 = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i-4],sep=''))
  #relleno con el dia anterior
  ct1 =com.1*terra.menos1
  com.2=com.2*com
  ct1 = ct1 + com.2
  #leemos dia siguiente
  terra.mas1 = raster(paste('./Imagenes_MOD10A1/03_recorte/',listado_terra[i+4],sep=''))
  #nieve + resto categorias interesantes del dia siguiente
  terra.mas1.2 = (terra.mas1==200)
  #lo que queremos rellenar del dia anterior
  ct2 = (ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  #dia anterior rellenado con el dia siguiente
  ct3 =ct2*terra.mas1.2
  ct3[ct3==1] = 200
  #conversion de binarios a valores originales
  t = ct3*(ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  t[t==200] = 10000
  #t es la imagen rellenada
  t = ct1 + t
  t[t>9000] = 200
  t4 = t
  
  writeRaster(t4, filename=paste('./MODIS_Pirineo_rellenado/','comb_',fecha,".tif",sep=''), format="GTiff", overwrite=TRUE)
}


#### SIERRA NEVADA - FILABRES

## Paso 3. Listado de imágenes MOD10A1 (SAT TERRA) a rellenar
listado_terra = list.files('./Imagenes_MOD10A1_G/03_recorte/',pattern='.tif$')

## Paso 4. Listado de imágenes MYD10A1 (SAT AQUA) como posible relleno
listado_aqua = list.files('./Imagenes_MYD10A1_G/03_recorte/', pattern='tif$')

## Paso 5. Aplicación del relleno de lagunas.
# En primer lugar se rellenaran los píxeles catalogados como ND con MYD10A1. En caso de que
# igualmente en MYD10A1 sean catalogados como ND, se aplicará un relleno temporal.

for(i in 5:(length(listado_terra)-4)){ #recorre 1 a 1 todo el listado de las imagenes TERRA desde la quinta hasta la ultima-4
  print(paste(i,'de', length(listado_terra)))
  fecha = substr(listado_terra[i],10,16) #la fecha que debe ser común está entre el carácter 10 y el 16 de los ficheros tif
  w = which(fecha==substr(listado_aqua,10,16)) #Crea un objeto W en el que diga en qué posición del listado AQUA está la imágen de TERRA.
  
  if(length(w)==0){
    #next #si no hay fecha común entre el listado de TERRA y de AQUA, sigue al siguiente (función next), pero si hay fecha común aplica todo lo que sigue
    com = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i],sep='')) #crea un objeto "com" de tipo raster con el fichero TERRA que no tiene par en AQUA aún así.
  } else{
    #Leemos TERRA y creamos un objeto TERRA con la imágen del día analizado en el momento (i)
    terra = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i],sep=''))
    #nos quedamos solo con los pixeles que NO queremos mantener (binario 1=categorias que no nos interesan: cumplen condición,
    #                                                  0=categorias que nos interesan: no cumplen condición)
    terra2 = (terra!=200 & terra!=25 & terra!=37 & terra!=100)
    #inversa de la anterior (lo que si que queremos mantener)
    terra3 = (terra==200 | terra==25 | terra==37 | terra==100)
    #convertimos el binario que queremos mantener en sus valores originales (y 0 en el resto)
    terra4 = terra*terra3
    
    #leemos aqua
    aqua = raster(paste('./Imagenes_MYD10A1_G/03_recorte/',listado_aqua[w],sep=''))
    #nos quedamos con los valores de aqua que queriamos rellenar en terra
    com = aqua*terra2
    #rellenamos los huecos de aqua con los valores originales de terra
    com = com+terra4
    aqrel = com
    #writeRaster(com, filename=paste('./rellenadas/',paste('comb_',fecha,".tif",sep=''), format="GTiff",overwrite='TRUE')
  }
  
  #dia1
  #imagen rellenada con aqua
  com.1 = (com!=200 & com!=25 & com!=37 & com!=100)
  com.2 = (com==200 | com==25 | com==37 | com==100)
  #leemos di anterior
  terra.menos1 = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i-1], sep=''))
  #relleno con el dia anterior
  ct1 =com.1*terra.menos1
  com.2=com.2*com
  ct1 = ct1 + com.2
  #leemos dia siguiente
  terra.mas1 = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i+1],sep=''))
  #solo la nieve dle dia siguiente
  terra.mas1.2 = (terra.mas1==200)
  #lo que queremos rellenar del dia anterior
  ct2 = (ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  #dia anterior rellenado con el dia siguiente
  ct3 =ct2*terra.mas1.2
  ct3[ct3==1] = 200
  #conversion de binarios a valores originales
  t = ct3*(ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  t[t==200] = 10000
  #t es la imagen rellenada
  t = ct1 + t
  t[t>9000] = 200
  t1 = t
  
  #dia2
  com = t1
  #imagen rellenada con aqua
  com.1 = (com!=200 & com!=25 & com!=37 & com!=100)
  com.2 = (com==200 | com==25 | com==37 | com==100)
  #leemos di anterior
  terra.menos1 = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i-2],sep=''))
  #relleno con el dia anterior
  ct1 =com.1*terra.menos1
  com.2=com.2*com
  ct1 = ct1 + com.2
  #leemos dia siguiente
  terra.mas1 = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i+2],sep=''))
  #solo la nieve dle dia siguiente
  terra.mas1.2 = (terra.mas1==200)
  #lo que queremos rellenar del dia anterior
  ct2 = (ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  #dia anterior rellenado con el dia siguiente
  ct3 =ct2*terra.mas1.2
  ct3[ct3==1] = 200
  #conversion de binarios a valores originales
  t = ct3*(ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  t[t==200] = 10000
  #t es la imagen rellenada
  t = ct1 + t
  t[t>9000] = 200
  t2 = t
  
  #dia3
  com = t2
  #imagen rellenada con aqua
  com.1 = (com!=200 & com!=25 & com!=37 & com!=100)
  com.2 = (com==200 | com==25 | com==37 | com==100)
  #leemos di anterior
  terra.menos1 = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i-3],sep=''))
  #relleno con el dia anterior
  ct1 =com.1*terra.menos1
  com.2=com.2*com
  ct1 = ct1 + com.2
  #leemos dia siguiente
  terra.mas1 = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i+3],sep=''))
  #solo la nieve dle dia siguiente
  terra.mas1.2 = (terra.mas1==200)
  #lo que queremos rellenar del dia anterior
  ct2 = (ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  #dia anterior rellenado con el dia siguiente
  ct3 =ct2*terra.mas1.2
  ct3[ct3==1] = 200
  #conversion de binarios a valores originales
  t = ct3*(ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  t[t==200] = 10000
  #t es la imagen rellenada
  t = ct1 + t
  t[t>9000] = 200
  t3 = t
  
  #dia4
  com = t3
  #imagen rellenada con aqua
  com.1 = (com!=200 & com!=25 & com!=37 & com!=100)
  com.2 = (com==200 | com==25 | com==37 | com==100)
  #leemos di anterior
  terra.menos1 = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i-4],sep=''))
  #relleno con el dia anterior
  ct1 =com.1*terra.menos1
  com.2=com.2*com
  ct1 = ct1 + com.2
  #leemos dia siguiente
  terra.mas1 = raster(paste('./Imagenes_MOD10A1_G/03_recorte/',listado_terra[i+4],sep=''))
  #nieve + resto categorias interesantes del dia siguiente
  terra.mas1.2 = (terra.mas1==200)
  #lo que queremos rellenar del dia anterior
  ct2 = (ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  #dia anterior rellenado con el dia siguiente
  ct3 =ct2*terra.mas1.2
  ct3[ct3==1] = 200
  #conversion de binarios a valores originales
  t = ct3*(ct1!=200 & ct1!=25 & ct1!=37 & ct1!=100)
  t[t==200] = 10000
  #t es la imagen rellenada
  t = ct1 + t
  t[t>9000] = 200
  t4 = t
  
  writeRaster(t4, filename=paste('./MODIS_Sierranevada_rellenado/','comb_',fecha,".tif",sep=''), format="GTiff", overwrite=TRUE)
}