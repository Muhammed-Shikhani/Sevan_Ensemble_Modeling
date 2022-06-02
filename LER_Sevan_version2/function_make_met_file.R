
make_met_file <- function(list_name){
  
  list_name <- get(load("/home/shikhani/Documents/CORDEX_2021/ERA5_daily_Lake_Sevan_1979_2020_interpolated_allvars_updatedps.rda"))

  df_meteo <- data.frame('datetime' = list_name$uas$Dates$start,
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
  
  write.csv(df_meteo, "LakeEnsemblR_meteo_standard.csv", row.names = F, quote = F)
}