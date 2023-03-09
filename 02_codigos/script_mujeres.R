# Abrimos las paqueterías con un sólo comando:
## La herramienta pacman sirve para llamar más herramientas
library("pacman")

p_load(ineq, haven, readr, readxl, ggplot2, tidyverse, expss, 
       DescTools, lmtest, MASS, knitr, foreign, RColorBrewer)

##############
#P herramientas y paqueterías para mapear
## La herramienta sf sirve para utilizar vectores con informacion espacial
## La herramienta ggspatial sirve para mapear objetos espaciales

library("sf")
library("ggspatial")
library("colorspace")
library("readxl")
library("dplyr")
library("tmap")

##############
# Paso #1 Abrir capas para mapas, la cual proviene del archivo .shp
##############
#AGEBs de tamaulipas
municipios_sinaloa <- st_read("25mun.shp")

## Abrir los datos del censo censo
RESMUN_sinaloa <- read_excel("ITER_25XLSX20.xlsx")

RESMUN_sinaloa <- RESMUN_sinaloa %>% 
  filter(NOM_LOC == "Total del Municipio")

RESMUN_sinaloa <- rename(RESMUN_sinaloa, CVE_MUN = MUN)


# Ahora vamos a hacer Union de bases de datos (la capa del mapa con los datos del censo)

bd_final <- left_join(municipios_sinaloa, RESMUN_sinaloa)

## Generamos nuestro primer mapa y lo guardamos como un objeto
## Poblacion
options(scipen = 999)

ggplot () +
  theme_void()+
  geom_sf(data = bd_final, aes(fill = POBTOT), lwd = 0) +
  scale_fill_gradientn(colors = viridis::cividis(20)) +
  ggtitle("Población en Sinaloa en el año 2020: \n3,026,943", subtitle = "") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Número") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "#")

## Otra forma
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))      # Create reverse Spectral palette

plot_cities <- ggplot () +
  geom_sf(data = bd_final, aes(fill = POBTOT), lwd = 0) +
  scale_fill_gradientn(colors = myPalette(4)) +
  ggtitle("Población en Sinaloa en el año 2020: \n3,026,943", subtitle = "") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Número") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "#")

## plot_cities

## Cartogramas
library(cartogram)
carto.dorling <- cartogram_dorling(bd_final,"POBTOT")
class(carto.dorling)

tm_shape(carto.dorling) +
  tm_fill("POBTOT") +
  tm_borders()

carto.cont <- cartogram_cont(bd_final,"POBTOT")

tm_shape(carto.cont) +
  tm_fill("POBTOT") +
  tm_borders()

carto.ncont <- cartogram_ncont(bd_final,"POBTOT")
tm_shape(carto.ncont) +
  tm_fill("POBTOT") +
  tm_borders() +
  tm_layout(main.title = "Cartograma de la población en Sinaloa", title.size = 0.1, main.title.position="center")


## Guardar mapa

pob_fem <- ggplot(bd_final, aes(x=NOM_MUN, y=POBFEM))+
  geom_bar(stat='identity', fill="forest green")+
  ggtitle("Población femenina en el año 2020: \n1,532,128 Mujeres", subtitle = "") +
  ylab("Población femenina") 

pob_fem + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Poblacion femenina
ggplot () +
  theme_void()+
  geom_sf(data = bd_final, aes(fill = POBFEM), lwd = 0) +
  scale_fill_gradientn(colors = viridis::cividis(20)) +
  ggtitle("Población femenina en el año 2020", subtitle = "") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Número") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "#")

# Porcentaje
bd_final$porcen_noasisfem6a11 <- bd_final$P6A11_NOAF/5256*100

ggplot () +
  theme_void()+
  geom_sf(data = bd_final, aes(fill = porcen_noasisfem6a11), lwd = 0) +
  scale_fill_gradientn(colors = viridis::cividis(20)) +
  ggtitle("Porcentaje de niñas de 6 a 11 años de edad \nque no van a la escuela: 5,256 niñas", subtitle = "") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Número") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "%")

# Porcentaje
bd_final$porcen_nosabefem8a14 <- bd_final$P8A14AN_F/2845*100

ggplot () +
  theme_void()+
  geom_sf(data = bd_final, aes(fill = porcen_nosabefem8a14), lwd = 0) +
  scale_fill_gradientn(colors = viridis::cividis(20)) +
  ggtitle("Porcentaje de niñas de 8 a 14 años de edad \nque no sabe leer y escribir: 2,845 niñas", subtitle = "") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Número") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "%")

##
pob_nin <- ggplot(bd_final, aes(x=NOM_MUN, y=P8A14AN_F))+
  geom_bar(stat='identity', fill="forest green")+
  ggtitle("Cantidad de niñas de 8 a 14 años que no saben leer y escribir: \n2,845 niñas", subtitle = "") +
  ylab("Cantidad") 

pob_nin + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# Porcentaje
bd_final$porcen_femanalf <- bd_final$P15YM_SE_F/42875*100

ggplot () +
  theme_void()+
  geom_sf(data = bd_final, aes(fill = porcen_femanalf), lwd = 0) +
  scale_fill_gradientn(colors = viridis::cividis(20)) +
  ggtitle("Porcentaje de población femenina de 15 años \no más analfabeta: 42,875", subtitle = "") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Número") +
  labs ( caption = "Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "%")

##
pob_femanalf <- ggplot(bd_final, aes(x=NOM_MUN, y=P15YM_SE_F))+
  geom_bar(stat='identity', fill="forest green")+
  ggtitle("Población femenina de 15 años y más analfabeta.", subtitle = "") +
  ylab("Cantidad") 

pob_femanalf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Porcentaje
library(ggthemes)


bd_final$porcen_ocupadaf <- bd_final$POCUPADA_F/594142*100

grafica <- ggplot() +
  theme_void()+
  geom_sf(data = bd_final, aes(fill = porcen_ocupadaf), lwd = 0) +
  scale_fill_gradientn(colors = viridis::cividis(20)) +
  ggtitle("Porcentaje de población femenina ocupada: \n594,142", subtitle = "") +
  labs(caption = "INEGI, datos del Censo 2020. \nNota: el porcentaje es respecto al total de la entidad \nColaboracion de Socky Mojica",
         fill = "%") 

grafica + theme(
    plot.title = element_text(hjust = 0.5, lineheight=1.15, face="bold", size=16), 
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  )


##
pob_ocupadaf <- ggplot(bd_final, aes(x=NOM_MUN, y=POCUPADA_F))+
  geom_bar(stat='identity', fill="forest green")+
  ggtitle("Población femenina ocupada (personas con empleo).", subtitle = "") +
  ylab("Cantidad") 

pob_ocupadaf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





