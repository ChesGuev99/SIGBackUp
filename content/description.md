---
date: "2016-05-05T21:48:51-07:00"
title: Descripción
---

# Tecnológico de Costa Rica Escuela de Computación Curso: Sistemas de Información Geográfica

## II Proyecto

La idea de este proyecto es crear mapas con información de interés y publicarlos en el Web. Para ello se debe realizar antes una investigación de información, obtener los datos necesarios y agregarlos al mapa de cantones o distritos de Costa Rica.

Cada persona realizará un mapa individual conforme a los temas que serán asignados. En los temas relacionados con distritos se cuenta con hojas de cálculo que deben ser procesadas para agregar la información a los mapas, en los temas relacionados con cantones existen documentos en PDF desde donde deben ser extraídos los datos.

La descripción para crear estos mapas aparece en el capítulo 8 del libro Geocomputation with R.

### Primera parte - Mapa estático
Cada estudiante debe descargar el archivo de datos que le fue asignado y extraer cuatro o cinco campos de datos que considere relevantes para el tema que está analizando. Además, debe identificar el campo que le servirá de "llave" para hacer el enlace con el archivo shapefile. Si el tema trata sobre distritos entonces debe ser el código (numérico) de dicho distrito (no utilice el nombre alfabético, pues diferentes distritos de diferentes cantones tienen el mismo nombre). Si el tema trata sobre cantones entonces debe ser el código (númerico) del cantón.

Una vez que se identifica dicha información, se debe tomar la capa de datos (shapefile) de cantones o distritos de Costa Rica y agregarles los datos obtenidos en la investigación. Para realizar este proceso se puede utilizar (a) Excel - que cuenta con una opción que permite leer y escribir archivos en formato DBF, (b) QGIS - que es un SIG que cuenta con opciones de edición de tablas de atributos, (c) R - que permite concatenar varios data fields con información relacionada.

Una vez que se ha incorporado la información al archivo shapefile se deben crear varios mapas estáticos donde mediante colores se clasifiquen los diferentes valores de los nuevos campos que se agregaron al mapa.

### Segunda parte - Mapa dinámico
Se debe crear un mapa dinámico que mediante el Web permite realizar acercamientos (zoom) y alejamientos (unzoom). Además, el mapa debe mostrar los cantones o distritos con colores que clasifican los diferentes valores de los campos que se agregaron al mapa. También debe haber algún tipo de selector que permita cambiar entre el campo del que se muestra la clasificación.

Por último, el mapa debe permitir que cuando se pase el cursor (o se haga click) sobre el distrito ó cantón se muestre todos los datos de dicho elemento geográficos. Entre estos datos estarían los campos agregados y otros campos generales como el nombre de la provincia, el cantón y el distrito (si fuere necesario).

El mapa deberá ser publicado en el Web mediante algún sitio de hosting gratuito. Pueden utilizar el sitio Netlify.com u otro que ustedes decidan.

# Tema asignado: Incidentes policiales x distrito - Ana Guevara

### Consideraciones generales
Deben utilizar la descripción para la creación de mapas estáticos y dinámicos que aparece en el capítulo 8 del libro Geocomputation With R (que se encuentra publicado en el Classroom).
Se debe incluir (en el repositorio de classroom) un archivo de descripción del proyecto con las direcciones en donde queda publicado el mapa, las imágenes de los mapas estáticos y los scripts utilizados en R para generar dichos mapas.
Este proyecto se debe realizar en forma individual.