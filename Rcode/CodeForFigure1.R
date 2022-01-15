rm(list=ls())
options(stringsAsFactors=FALSE)

# Load libraries
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(cowplot)



# Load shapefiles
africa <- read_sf("data/gis/Africa_Continent.shp")
region_shp <- read_sf("data/gis/TZ_Region_2012_pop.shp")
district_shp <- read_sf("data/gis/TZ_District_2012_pop.shp")
tanzania <- read_sf("data/gis/gadm36_TZA_0.shp")
uganda <- read_sf("data/gis/gadm36_UGA_0.shp")
kenya <-  read_sf("data/gis/gadm36_KEN_0.shp")
burundi <-  read_sf("data/gis/gadm36_BDI_0.shp")
rwanda <-  read_sf("data/gis/gadm36_RWA_0.shp")
dem_congo <-  read_sf("data/gis/gadm36_COD_0.shp")
zambia <-  read_sf("data/gis/gadm36_ZMB_0.shp")
malawi <-  read_sf("data/gis/gadm36_MWI_0.shp")
mozam <-  read_sf("data/gis/gadm36_MOZ_0.shp")
# prot_areas <- read_sf("data/gis/Protected_areas.shp")
# clip_prot_areas <- read_sf("data/gis/Protected_areas_clipped.shp")

# Set study regions
study_regs = c("Lindi", "Pwani", "Morogoro", "Mtwara", "Dar es Salaam")

# Subset shapefiles
study_reg <- region_shp[which(region_shp$Region_Nam %in% study_regs),]
study_dis <- district_shp[which(district_shp$Region_Nam %in% study_regs),]

# Change column name to match rest of data
# names(clip_prot_areas)[names(clip_prot_areas) == 'Reg_ID'] <- 'Region_Nam'

# Set plot colours
dis_cols = c("Morogoro"="#64de43", "Pwani"="#de7043", "Dar es Salaam"="#4364de",
             "Lindi"="#be43de", "Mtwara"="#debd43")

# Get bb-box for Tanzania
tz_bb = st_as_sfc(st_bbox(tanzania))

# Produce inset map
inset_map = ggplot() +
  geom_sf(data=africa, fill="#f1f1f1", size=0.2) +
  geom_sf(data = tz_bb, fill = NA, color = "red", size = 1) +
  coord_sf() +
  theme_void()

# Produce main map
main_map = ggplot() +
  # Add country shapefiles
  geom_sf(data=uganda, fill="#f1f1f1", color="darkgrey", size=0.2) +
  geom_sf(data=kenya, fill="#f1f1f1", color="darkgrey", size=0.2) +
  geom_sf(data=burundi, fill="#f1f1f1", color="darkgrey", size=0.2) +
  geom_sf(data=rwanda, fill="#f1f1f1", color="darkgrey", size=0.2) +
  geom_sf(data=dem_congo, fill="#f1f1f1", color="darkgrey", size=0.2) +
  geom_sf(data=zambia, fill="#f1f1f1", color="darkgrey", size=0.2) +
  geom_sf(data=malawi, fill="#f1f1f1", color="darkgrey", size=0.2) +
  geom_sf(data=mozam, fill="#f1f1f1", color="darkgrey", size=0.2) +
  geom_sf(data=tanzania, fill="#d6d6d6", color="darkgrey", size=0.2) +
  # Add study districts
  geom_sf(data=study_dis, aes(fill=Region_Nam), color="#323232", size=0.2) +
  # Add country labels
  annotate(geom="text", x=38, y=-1.5, label="KENYA") +
  annotate(geom="text", x=31.5, y=0, label="UGANDA") +
  annotate(geom="text", x=30, y=-2, label="RWANDA") +
  annotate(geom="text", x=29.5, y=-3.5, label="BURUNDI") +
  annotate(geom="text", x=29.5, y=-7, label="DEMOCRATIC REPUBLIC \n OF THE CONGO") +
  annotate(geom="text", x=30.5, y=-10.8, label="ZAMBIA") +
  annotate(geom="text", x=34, y=-11, label="MALAWI") +
  annotate(geom="text", x=38, y=-12.5, label="MOZAMBIQUE") +
  annotate(geom="text", x=35, y=-6, label="TANZANIA") +
  # Finish formatting the map appearence
  scale_fill_manual(name="Region", values=dis_cols) +
  annotation_scale(location="bl", ) +
  annotation_north_arrow(location="bl", which_north = "true", pad_y = unit(0.3, "in")) +
  coord_sf(xlim=c(28,41), ylim=c(-13, 1)) +
  theme_void()


ggdraw() +
  draw_plot(main_map) +
  #draw_plot(inset_map, x = 0.55, y = 0.80, width = 0.2, height = 0.2)
  draw_plot(inset_map, x = 0.75, y = 0.65, width = 0.2, height = 0.2)
ggsave("Figures/study_map.tiff", height=7, width=10)
