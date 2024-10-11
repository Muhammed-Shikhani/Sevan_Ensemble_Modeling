
make_met_file <- function(list_name){
  
  make_data <<- function(list_var){
    my_vec <- rep(NA,length(my_dates))
    my_vec[which(as.Date(my_dates)%in% as.Date(list_var$Dates$start))] <- list_var$Data
    return(my_vec)
  }
  
  df_meteo <- data.frame('datetime' = paste0(my_dates, " 00:00:00"),
                         'Ten_Meter_Uwind_vector_meterPerSecond' = make_data(list_name$uas),
                         'Ten_Meter_Vwind_vector_meterPerSecond' = make_data(list_name$vas),
                         'Surface_Level_Barometric_Pressure_pascal' = make_data(list_name$ps),
                         'Air_Temperature_celsius' = make_data(list_name$tas),
                         'Relative_Humidity_percent' = make_data(list_name$hurs),
                         'Precipitation_millimeterPerDay' = make_data(list_name$pr),
                         'Shortwave_Radiation_Downwelling_wattPerMeterSquared' = make_data(list_name$rsds),
                         'Longwave_Radiation_Downwelling_wattPerMeterSquared' = make_data(list_name$rlds),
                         'Cloud_Cover_decimalFraction' = make_data(list_name$clt),
                         'Ten_Meter_Elevation_Wind_Speed_meterPerSecond' =make_data(list_name$sfcWind) 
  )
  df_app <- zoo::na.approx(as.matrix(df_meteo[,-1]))
  df_meteo[,-1] <- df_app
  
  
  write.csv(df_meteo, "LakeEnsemblR_meteo_standard.csv", row.names = F, quote = F)
}