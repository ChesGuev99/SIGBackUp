setwd("C:/Users/anagu/OneDrive/Documentos/TEC/SemestreII2020/SIG/sig-proyecto2-ChesGuev99")

library(sf)
library(dplyr)
library(leaflet)
library(leafpop)
library(RColorBrewer)
library(mapview)

puntosCR <- st_read("DistritosEdited.shp")
print(puntosCR)
#plot(puntosCR["prov_id"],col = "red")
JustMapa = tmap::tm_shape(puntosCR)+tmap::tm_fill(title = 'Tipo de Crimen')+tmap::tm_polygons(col='estadistica')
breaks = c(0,3,4,5)


MapaTipoCrimen = tmap::tm_shape(puntosCR)+
  tmap::tm_style("classic", sepia.intensity = 0.1)+
  tmap::tm_fill(title = 'Tipo de Crimen', col='estadstica', palette = 'RdYlBu') +
  tmap::tm_layout(bg.color = 'lightblue', main.title="Crimen En Costa Rica",
                  legend.position = c('left', 'center')) + 
  tmap::tm_compass(bg.alpha = 0,bg.color = 'lightblue' ,size = 4, 
                   show.labels = 2, type = 'rose', position = c('right','bottom')) 
  
MapaGenero = tmap::tm_shape(puntosCR)+
  tmap::tmap_style('watercolor')+
  tmap::tm_fill(title = 'Crimen y Género', col='estadsti_3', palette = c('grey70','royalblue1','orchid1')) +
  tmap::tm_layout(legend.title.color = 'royalblue1',bg.color = 'lightcyan',
                  main.title.color = 'midnightblue',main.title="Crimen En Costa Rica", 
                  legend.position = c('right', 'bottom'), frame = 'orchid1', frame.double.line = TRUE) + 
  tmap::tm_compass(bg.alpha = 0,bg.color = 'lightcyan' ,size = 6, 
                   show.labels = 2, type = '4star', position = c('left','bottom')) 


MapaNacionalidad = tmap::tm_shape(puntosCR)+
  tmap::tmap_style('beaver')+
  tmap::tm_fill(title = 'Crimen y Nacionalidad', col='estadsti_4', palette = 'BrBG') +
  tmap::tm_layout(legend.title.color = 'mistyrose4',bg.color = 'mintcream', 
                  main.title.color = 'mistyrose4',main.title="Crimen En Costa Rica", 
                  legend.position = c('left', 'bottom'), frame = 'mistyrose4', frame.double.line = TRUE) + 
  tmap::tm_compass(bg.alpha = 0,bg.color = 'lightcyan' ,size = 4, 
                   show.labels = 2, type = 'arrow', position = c('right','bottom')) 


MapaVictima = tmap::tm_shape(puntosCR)+
  tmap::tmap_style('beaver')+
  tmap::tm_fill(title = 'Crimen y Afectado', col='estadsti_1', palette = 'Set3') +
  tmap::tm_layout(legend.title.color = 'mistyrose4',bg.color = 'darkslategray4', 
                  main.title.color = 'mistyrose4',main.title="Crimen En Costa Rica", 
                  legend.position = c('left', 'bottom'), frame = 'mistyrose4', frame.double.line = TRUE) + 
  tmap::tm_compass(bg.alpha = 0,bg.color = 'lightcyan' ,size = 4, 
                   show.labels = 2, type = '8star', position = c('right','bottom')) 


MapaEdad = tmap::tm_shape(puntosCR)+
  tmap::tmap_style('beaver')+
  tmap::tm_fill(title = 'Crimen y Edad del Afectado', col='estadsti_2', palette = 'BrBG') +
  tmap::tm_layout(legend.title.color = 'navy',bg.color = 'navy', 
                  main.title.color = 'navy',main.title="Crimen En Costa Rica", legend.bg.color = 'paleturquoise', 
                  legend.position = c('left', 'bottom'), frame = 'paleturquoise', frame.double.line = TRUE) + 
  tmap::tm_compass(bg.alpha = 0,bg.color = 'lightcyan' ,size = 4, 
                   show.labels = 2, type = 'radar', position = c('right','bottom')) 


#tmap::tmap_arrange(MapaTipoCrimen,MapaVictima,MapaEdad,MapaGenero,MapaNacionalidad)



# TIPO DE CRIMEN
map <- mapview::mapview(puntosCR, alpha.regions = 10, col.regions = palette.colors(palette = 'Set3'), map.types = c('CartoDB.DarkMatter','CartoDB.Positron') , 
                 layer.name= 'Tipo De Crimen', zcol = "estadstica" ,
  popup = popupTable(puntosCR, zcol=c("NOM_PROV","NOM_CANT","NOM_DIST", 'COD_DIST',
  'estadstica', 'estadsti_1', 'estadsti_2', 'estadsti_3', 'estadsti_4'))) +

# Victima
  mapview::mapview(puntosCR, alpha.regions = 10, col.regions = palette.colors(palette = 'Accent'), map.types = c('CartoDB.DarkMatter','CartoDB.Positron') , 
                   layer.name= 'Victima', zcol = "estadsti_1" ,
                   popup = popupTable(puntosCR, zcol=c("NOM_PROV","NOM_CANT","NOM_DIST", 'COD_DIST',
                                                       'estadstica', 'estadsti_1', 'estadsti_2', 'estadsti_3', 'estadsti_4'))) +
# Edad
  mapview::mapview(puntosCR, alpha.regions = 10, map.types = c('CartoDB.DarkMatter','CartoDB.Positron') , 
                   layer.name= 'Edad', zcol = "estadsti_2" ,
                   popup = popupTable(puntosCR, zcol=c("NOM_PROV","NOM_CANT","NOM_DIST", 'COD_DIST',
                                                       'estadstica', 'estadsti_1', 'estadsti_2', 'estadsti_3', 'estadsti_4'))) +
# Genero
  mapview::mapview(puntosCR, alpha.regions = 10, col.regions = c('khaki','violetred', 'skyblue3'), map.types = c('CartoDB.DarkMatter','CartoDB.Positron') , 
                   layer.name= 'Género', zcol = "estadsti_3" ,
                   popup = popupTable(puntosCR, zcol=c("NOM_PROV","NOM_CANT","NOM_DIST", 'COD_DIST',
                                                       'estadstica', 'estadsti_1', 'estadsti_2', 'estadsti_3', 'estadsti_4'))) +
# Nacionalidad
  mapview::mapview(puntosCR, alpha.regions = 10, col.regions = brewer.pal(10, 'Dark2'), map.types = c('CartoDB.DarkMatter','CartoDB.Positron') , 
                   layer.name= 'Nacionalidad', zcol = "estadsti_4" ,
                   popup = popupTable(puntosCR, zcol=c("NOM_PROV","NOM_CANT","NOM_DIST", 'COD_DIST',
                                                       'estadstica', 'estadsti_1', 'estadsti_2', 'estadsti_3', 'estadsti_4'))) 
mapview::mapshot(map, url = paste0(getwd(), "/map.html"))


mapshot(map, url = paste0(getwd(), "/map.html"), selfcontained = TRUE)

#print(puntosCR)

