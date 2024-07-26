library(raster)
library(tidyverse)
library(sf)
library(whitebox)
library(tmap)
library(stars)
library(rayshader)
library(rgl)
library(stars)
#librarary()
#install.packages('tm_shape')
#install.packages("whitebox", repos="http://R-Forge.R-project.org")
whitebox::wbt_init()
knitr::knit_hooks$set(webgl = hook_webgl)
theme_set(theme_classic())

Brad_DEM<-raster("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/GIS site selection/GIS Files/ModifiedDEM.TIFF")
plot(Brad_DEM)


wbt_fill_single_cell_pits(
                  dem = Brad_DEM,
                  output = "Brad_pits.tif")

wbt_breach_depressions_least_cost(
  dem = Brad_DEM,
  output = "BF_DEM_breached_SH.tif",
  dist = 5,
  fill = TRUE)

breached<-raster("Bradford_LiDAR_1M_June2023/Sam/BF_DEM_breached.tif")
plot(breached)


wbt_fill_depressions_wang_and_liu(
  dem = "Bradford_LiDAR_1M_June2023/BF_DEM.tif",
  output = "Bradford_LiDAR_1M_June2023/Sam/BF_DEM_filled_breached.tif")
filled_breached<-raster("Bradford_LiDAR_1M_June2023/Sam/BF_DEM_filled_breached.tif")
plot(filled_breached)


#flow accumulation####
wbt_d8_flow_accumulation(input = "Bradford_LiDAR_1M_June2023/Sam/BF_DEM_filled_breached.tif",
                         output = "Bradford_LiDAR_1M_June2023/Sam/BF_DEM_d8.tif")
flow_accum<-raster("Bradford_LiDAR_1M_June2023/Sam/BF_DEM_d8.tif")
plot(flow_accum)

wbt_d8_pointer(dem = "Bradford_LiDAR_1M_June2023/Sam/BF_DEM_filled_breached.tif",
               output = "Bradford_LiDAR_1M_June2023/Sam/BF_DEM_d8_pointer.tif")
pointer<-raster("Bradford_LiDAR_1M_June2023/Sam/BF_DEM_d8_pointer.tif")
plot(pointer)


ppoints <- tribble(
  ~Lon, ~Lat,
  -82.2837, 29.862,
  -82.1886, 29.9435,
  -82.2206454, 29.9045721)
ppointsSP <- SpatialPoints(ppoints, proj4string = CRS("+proj=longlat +datum=NAD83"))
shapefile(ppointsSP, filename = "Bradford_LiDAR_1M_June2023/Sam/pourpoints.shp", overwrite = TRUE)


wbt_extract_streams(flow_accum = "Bradford_LiDAR_1M_June2023/Sam/BF_DEM_d8.tif",
                    output = "Bradford_LiDAR_1M_June2023/Sam/BF_raster_streams.tif",
                    threshold = 500)
streams<-raster('Bradford_LiDAR_1M_June2023/Sam/BF_raster_streams.tif')
plot(streams)

wbt_jenson_snap_pour_points(pour_pts = "Bradford_LiDAR_1M_June2023/Sam/pourpoints.shp",
                            streams = "Bradford_LiDAR_1M_June2023/Sam/BF_raster_streams.tif",
                            output = "Bradford_LiDAR_1M_June2023/Sam/snappedpp.shp",
                            snap_dist = 0.0005) #careful with this! Know the units of your data
pp <- shapefile("Bradford_LiDAR_1M_June2023/Sam/snappedpp.shp")


wbt_watershed(d8_pntr = "Bradford_LiDAR_1M_June2023/Sam/BF_DEM_d8_pointer.tif",
              pour_pts = "Bradford_LiDAR_1M_June2023/Sam/snappedpp.shp",
              output = "Bradford_LiDAR_1M_June2023/Sam/BF_DEM_watersheds.tif")
ws <- raster("Bradford_LiDAR_1M_June2023/Sam/BF_DEM_watersheds.tif")
plot(ws)

wsshape <- st_as_stars(ws) %>% st_as_sf(merge = T)
ws1shp <- wsshape %>% filter(brush_watersheds == "1")
