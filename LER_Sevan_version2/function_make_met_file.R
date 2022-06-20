
make_met_file <- function(list_name){

  df_meteo <- data.frame('datetime' = paste0(c(my_dates,my_dates[length(my_dates)]+1), " 00:00:00"),
                         'Ten_Meter_Uwind_vector_meterPerSecond' = list_name$uas$Data,
                         'Ten_Meter_Vwind_vector_meterPerSecond' = list_name$vas$Data,
                         'Surface_Level_Barometric_Pressure_pascal' = list_name$ps$Data,
                         'Air_Temperature_celsius' = list_name$tas$Data,
                         'Relative_Humidity_percent' = list_name$hurs$Data,
                         'Precipitation_millimeterPerDay' = list_name$pr$Data,
                         'Shortwave_Radiation_Downwelling_wattPerMeterSquared' = list_name$rsds$Data,
                         'Longwave_Radiation_Downwelling_wattPerMeterSquared' = list_name$rlds$Data,
                         'Cloud_Cover_decimalFraction' = list_name$clt$Data,
                         'Ten_Meter_Elevation_Wind_Speed_meterPerSecond' = list_name$sfcWind$Data)
  df_app <- zoo::na.approx(as.matrix(df_meteo[,-1]))
  df_meteo[,-1] <- df_app
 write.csv(df_meteo, "LakeEnsemblR_meteo_standard.csv", row.names = F, quote = F)
}