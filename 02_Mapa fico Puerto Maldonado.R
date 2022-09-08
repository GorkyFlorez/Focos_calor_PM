require(pacman)
pacman::p_load(tidyverse,sf,ggplot2, ggspatial,sp,osmdata,leaflet, ggmap )
available_tags("highway")

mad_map <- get_map(getbb("Madre de Dios"), maptype = "toner-background") # Localizamos madre de Dios
Puer_Mal <- st_read ("SHP/Puerto.shp") # Caragmos un shp de puerto maldonado
Puer_Mal <- st_transform(Puer_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

leaflet(Puer_Mal) %>% addTiles() %>% addPolygons() # Leemos el mapa con leaflet

Puer_Mal <- st_read ("SHP/Puerto.shp") # Caragmos un shp de puerto maldonado
Puer_Mal <- st_transform(Puer_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

Via_Mal <- st_read ("SHP/Red_vial.geojson") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

Rio_Pol <- st_read ("SHP/Rio_Poli.geojson") # Caragmos un shp de puerto maldonado
Rio_Poli  <- st_transform(Rio_Pol ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion


library(openxlsx)
Foco <- read.xlsx("Excel/Focos_2022-09-07_2022-09-08.xlsx", sheet="Hoja1")


# Extrayendo información de OSM

Puer_Maldonado <- opq(Puer_Mal) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf() # Informacion de osm del area

Puer_Maldonado_rios <- opq(Puer_Mal)%>% add_osm_feature(key = "waterway", value = "river") %>%osmdata_sf()
Puer_Maldonado_secu <- opq(Puer_Mal)%>% add_osm_feature(key = "highway",
                                                        value = c("residential", "living_street",
                                                                  "unclassified",
                                                                  "service", "footway")) %>% osmdata_sf()
calles <- Puer_Maldonado$osm_lines
calles <- st_intersection(calles, Puer_Mal )
calles <- calles %>%
  mutate(maxspeed = as.numeric(maxspeed),
         lanes = ifelse(is.na(lanes), 1, as.numeric(lanes)))

W<-ggplot() +
  geom_sf(data = calles,
          inherit.aes = FALSE,
          aes(color = maxspeed), 
          size = 1,
          alpha = .4) +
            scale_color_viridis_c() +
            geom_sf(data = filter(calles, str_detect(name, "Avenida")), color = "salmon") +
            geom_sf(data = Puer_Maldonado_secu$osm_lines,
                    inherit.aes = FALSE,
                    color = "gray",
                    size = .5,
                    alpha = .6) +
            geom_sf(data = Puer_Maldonado_rios$osm_lines,
                    inherit.aes = FALSE,
                    color = "blue",
                    size = .5,
                    alpha = .5) +
  geom_point(data =Foco , aes(x = longitude, y = latitude),size=1.5, color="red", pch=21, fill="red")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.4)+
  geom_sf(data = Rio_Poli , color="#a2d2ff", size=0.3, fill="#a2d2ff")+
            coord_sf(xlim = c( -69.28133,-69.11575 ), ylim = c(-12.69011,-12.46308),expand = FALSE)+
  labs(color = '',  x = 'Longitud', y = 'Latitud',
       title = "Mapa de Situación actual de focos de calor en",
       subtitle = "la ciudad de Puerto Maldonado",
       caption = "Datos en https://queimadas.dgi.inpe.br/queimadas/portal \n@Ing. Gorky Florez")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.width = unit(2.5,"line"), #ancho de cuadrados de referencia 
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, family="serif", face = "italic"),
        
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
            scale_x_continuous(breaks = seq(-69.28, -69.12, by = 0.03), expand=c(0.02, 0.02)) +
            scale_y_continuous(breaks = seq(-12.69, -12.46, by = 0.05), expand=c(0.02, 0.02)) +
            annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
            ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")
          #------------------------------------------------------------------------
W


ggsave(plot = W ,"Mapa/Puerto Maldonado.png", units = "cm", width = 21,height = 29, dpi = 1200) 


