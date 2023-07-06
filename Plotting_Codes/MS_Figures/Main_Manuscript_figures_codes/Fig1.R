library(loadeR)
library(visualizeR)
library(rgdal)
library(RColorBrewer)
library(ncdf4)
library("fields")
library("sp")
library("maptools")
library(maps)

setwd("/home/shikhani/Documents/cordex_domain_nc/org/")
# 
# nc1 <- loadGridData("tas_WAS-44_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v2_day_19560101-19601231.nc", var = "tas")
# spatialPlot(climatology(nc1), backdrop.theme ="coastline",  )

data(wrld_simpl)
#png(filename="all_doms.png",width=1200,height=900,bg="white")
pdf("Domain_world.pdf")

landFrac <-nc_open("tas_MNA-44_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19860101-19901231.nc")
lon <- ncvar_get(landFrac,"lon")
lat <- ncvar_get(landFrac,"lat")
t2m.mean <- ncvar_get(landFrac,"tas")
times <- ncvar_get(landFrac,"time")
t2m11 <- t2m.mean[,,1]
t2mNA <- t2m11
t2mNA <- t2m11 -t2m11
col<- colorRampPalette(c("snow1","snow2","snow3","seagreen","orange","firebrick"))
rgb.palette <-paste0(col(100), "20")
image.plot(lon,lat,t2mNA,col=rgb.palette,main=as.expression(paste("WAS-44_CNRM-CERFACS-CNRM-CM5_historical_SMHI-RCA4 01-01-1956",sep="")),axes=T,legend.lab="o C",ylim=c(-30,80),xlim=c(-30,130))

landFrac <-nc_open("tas_WAS-44_NOAA-GFDL-GFDL-ESM2M_rcp85_r1i1p1_IITM-RegCM4-4_v5_day_20710101-20751231.nc")
lon <- ncvar_get(landFrac,"lon")
lat <- ncvar_get(landFrac,"lat")
t2m.mean <- ncvar_get(landFrac,"tas")
times <- ncvar_get(landFrac,"time")
t2m11 <- t2m.mean[,,1]
t2mNA <- t2m11
t2mNA <- t2m11 -t2m11
col<- colorRampPalette(brewer.pal(8, "Oranges"))
rgb.palette <-paste0(col(100), "20")
par(new=TRUE)
image.plot(lon,lat,t2mNA,col=rgb.palette,axes=T,legend.lab="o C",ylim=c(-30,80),xlim=c(-30,130))

landFrac <-nc_open("tas_CAS-22_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_GERICS-REMO2015_v1_day_20910101-20951231.nc")
lon <- ncvar_get(landFrac,"lon")
lat <- ncvar_get(landFrac,"lat")
t2m.mean <- ncvar_get(landFrac,"tas")
times <- ncvar_get(landFrac,"time")
t2m11 <- t2m.mean[,,1]
t2mNA <- t2m11
t2mNA <- t2m11 -t2m11
col <- colorRampPalette(brewer.pal(8, "Blues"))
rgb.palette <-paste0(col(100), "20")
par(new=TRUE)
image.plot(lon,lat,t2mNA,col=rgb.palette,axes=T,legend.lab="o C",ylim=c(-30,80),xlim=c(-30,130))

map("world2", add = TRUE,wrap = c(-30,330))

dev.off()
######################

library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) 
library(RColorBrewer)

setwd("/home/shikhani/Documents/Sevan_plotting_draft/")
datafold <- "/home/shikhani/Documents/Sevan_plotting_draft/FIG1/Files_for_map/sevan.tif"
test <- raster(datafold) 
test
plot(test)
png("sevan_depth2.png", width=9, height=9, unit="cm", res=300)
my.palette <- brewer.pal(8, "RdYlBu") 
my.palette <- colorRampPalette(my.palette)(16)
my_map <- spplot(test,col="transparent", col.regions = my.palette,
                 colorkey = list(space = "left", height = 0.4, labels = list(cex=1.5)))
args <- my_map$legend$left$args$key
legendArgs <- list(fun = draw.colorkey,
                   args = list(key = args),
                   corner = c(0.05,0.3))
plot.margin = unit(rep(-1.25,4),"lines")
spplot(test, col.regions = my.palette, colorkey = FALSE,
       legend = list(inside = legendArgs))
dev.off()

test1 <- test
values(test1) <- 1900- values(test1) 
spplot(test1,col="transparent", col.regions = my.palette)


display.brewer.all()

#################
library(grid)
library(gridExtra)
library(png)
setwd("/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/fig1_map/")
# Load the world map image

world_map <- readPNG("Domain_world.png")
depth_map <- readPNG("sevan_depth2.png")
png("my_map2.png", width=20, height=20, unit="cm", res=300)
grid.raster(world_map ) # print homer in ll conrner

grid.raster(depth_map, x=.855, y=0.275, width=0.3)
dev.off()
