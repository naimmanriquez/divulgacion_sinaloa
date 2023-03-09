library(pxR)
library(RColorBrewer)
library(rgeos)
#install.packages("rgdal", repos = "http://cran.us.r-project.org") reinstall cause gpclib dependencie https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true
library(rgdal)
library(rayshader)
library(knitr)
library(magrittr)
library(tidyverse)
library(magic)
library(av)
library(viridis)
library(sf)
library(ggspatial)
library(readxl)
library(dplyr)
library(tmap)
library(tidyverse)
library(remotes)
library(rgl)

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

plot <- ggplot () +
  geom_sf(data = bd_final, aes(fill = POBTOT), lwd = 0) +
  scale_fill_gradientn(colors = viridis::cividis(20)) +
  ggtitle("Población en Sinaloa en el año 2020: \n3,026,943", subtitle = "") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Número") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "#")

plot_gg(plot
         , width = 3.5
         , height = 3.5
         , sunangle = 225
)