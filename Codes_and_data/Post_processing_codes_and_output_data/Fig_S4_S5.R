## HACKING LIMNOLOGY 2024
# Climate Change - ISIMIP
# author: Daniel Mercado-Bettín
# email:  daniel.mercado@ceab.csic.es
# theme:  GLOBAL LOCAL DATA
# date: 1 July 2024

# Load necessary libraries
#install.packages("terra")
#install.packages("ggplot2")
library(terra)
library(ggplot2)

setwd("/home/shikhani/Documents/Sevan_plotting_draft/revision2/")


##····································DOWNLOAD DATA data.isimip.org portal································
##········································································································
##········································································································
#Previouly you should have had donwloaded the files need as instructed in the workshop
#The data is download already as input in the repository

##····································GLOBAL SIMULATION, VISUALIZATION····································
##········································································································
##········································································································


# 1. Exploring NetCDF data - GLOBAL LAKES
#Example: Scenario: historical, time range: 1991-2000, region: Africa 
# Open the NetCDF file
nc_file <- rast("Lake_CCI_Ice.nc")
png(filename = "Sevan_ice_remote_sensing.png", height = 200, width = 200, units = "mm",res = 300 )
plot(nc_file)
dev.off()

# Explore the contents of the NetCDF file
print(nc_file)

#Get coordinates of the NetCDF file
#coordinates <- crds(nc_file)

# Get the time dimension 
time_info <- time(nc_file)

# Convert the time_info to Date format
time_info <- as.Date(time_info, format="%Y-%m-%d")

# Get the extent of the NetCDF file
ext <- ext(nc_file)
print(ext)

# 2. Plot a lake variable for 1 particular time for a whole region - GLOBAL LAKES

specific_time_index <- 150  # Change this to select a different time point
temp_at_time <- nc_file[[specific_time_index]]

# Convert the data to a data frame for plotting
temp_df <- as.data.frame(temp_at_time, xy = TRUE, na.rm = TRUE)


extract_time_data <- function(index) {
  temp_at_time <- nc_file[[index]]
  temp_df <- as.data.frame(temp_at_time, xy = TRUE, na.rm = TRUE)
  temp_df$time <- time_info[index]
  return(temp_df)
}

# Use lapply to apply the function to all time points
all_time_data <- lapply(1:nlyr(nc_file), extract_time_data)
fractions <- data.frame(Date=NA,water_fraction=NA ,ice_fraction=NA, cloud_fraction=NA, error_fraction=NA)
for(i in 1:length(time_info)){
  data_day <- all_time_data[[i]]
  day_fraction <- data.frame(Date= as.character(unique(data_day$time)) ,
                             water_fraction=length(data_day[which(data_day[,3]==1),3])/nrow(data_day), 
                             ice_fraction=length(data_day[which(data_day[,3]==2),3])/nrow(data_day),  
                             cloud_fraction=length(data_day[which(data_day[,3]==3),3])/nrow(data_day),  
                             error_fraction=length(data_day[which(data_day[,3]==4),3])/nrow(data_day))
  fractions  <- rbind(fractions, day_fraction)
}
fractions<- fractions[-1,]

plot(as.Date(fractions$Date), fractions$ice_fraction)

pd <-ggplot(data=fractions, aes(x=Date, y=ice_fraction))+
  geom_point(color="steelblue")+
  theme_bw()+
  labs(y="Fraction of ice covered area")

ggsave('ice_area_fraction.png', pd,  dpi = 300,width = 250,height = 200, units = 'mm')
write.csv(fractions, "ice_fractions_Sevan.csv", quote = F, row.names = F)
