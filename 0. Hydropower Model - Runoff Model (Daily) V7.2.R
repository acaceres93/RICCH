# Runoff Model

## Historical Model ---- 

### Historical Net Runoff

runoff_model_daily_historical <- function(LAI, rh, ra, avg_temp, precip_data, avg_altitude_basin, 
                                          avg_altitude_glacier, Sm, area_basin, area_glacier,
                                          degree_day_factor_glacier, degree_day_factor_snow, 
                                          start_date, frequency_date,  r_arc, r_aero, gs, 
                                          elevation_bands){
  
  # PET 
  PET_calculation_eto_daily_calibration(LAI = LAI, rh = rh, ra = ra, avg_temp = avg_temp, 
                                        avg_altitude_basin = avg_altitude_basin, 
                                        r_arc = r_arc, r_aero = r_aero, gs = gs) -> PET_model
  PET_model*1000*24*3600 -> PET_mm_model # daily
  
  # Soil Moisture
  soil_moisture_calculation_daily(precip_data = precip_data, 
                                  Eto = PET_mm_model, 
                                  Sm = Sm, start_date = start_date, 
                                  frequency_date = frequency_date) -> St_model
  aet_calculation_daily(precip_data, PET_mm_model, Sm, start_date, 
                        frequency_date) -> AET_model
  
  
  
  # Glacier and Snow melt
  
  rain_by_elevation_bands(elevation_bands = elevation_bands, 
                          avg_temp = avg_temp, 
                          avg_altitude_basin = avg_altitude_basin,
                          precip_daily = precip_data) -> rain_by_elevation_model
  
  rain_on_glacier_runoff_historical_daily(temp_avg_dataset = avg_temp, 
                                          avg_altitude_basin = avg_altitude_basin, 
                                          avg_altitude_glacier = avg_altitude_glacier,
                                          precipitation_daily = precip_data,
                                          glacier_area = area_glacier) -> rain_on_glacier_model ## Already in MM3
  
  glacier_runoff_historical_daily(temp_avg_dataset = avg_temp, 
                                  degree_day_factor = degree_day_factor_glacier, 
                                  avg_altitude_basin = avg_altitude_basin,
                                  avg_altitude_glacier = avg_altitude_glacier,
                                  glacier_area = area_glacier) -> glacier_melt_runoff_model
  
  adjusted_elevation_bands_glacier(elevation_bands, 
                                   area_glacier) -> adjusted_elevation_bands
  
  # Net Runoff
  
  snowmelt_calculations(elevation_bands = elevation_bands, 
                        avg_temp = avg_temp, 
                        degree_day_factor = degree_day_factor_snow, 
                        precip_data = precip_data,
                        basin_altitude = avg_altitude_basin,
                        glacier_area = area_glacier,
                        frequency_date = frequency_date, 
                        start_date = start_date) -> total_snowmelt_basin_ts
  
  mapply(`*`, rain_by_elevation_model, 
         adjusted_elevation_bands) -> rain_by_elevation
  rowSums(rain_by_elevation) -> total_rain_basin
  ts(total_rain_basin, frequency=frequency_date, start=start_date) -> total_rain_basin_ts
  
  St_model*(area_basin - area_glacier) -> total_St_model
  AET_model*(area_basin - area_glacier) -> total_AET_model
  
  stats::lag(total_St_model) + total_rain_basin_ts + total_snowmelt_basin_ts + glacier_melt_runoff_model + 
    rain_on_glacier_model- total_AET_model - 
    total_St_model -> net_runoff_model
  ifelse(net_runoff_model<0,0,net_runoff_model) -> net_runoff_model
  
  return(net_runoff_model)
}

### Historical Variables Separated

runoff_model_daily_historical_variables <- function(LAI, rh, ra, avg_temp, precip_data, avg_altitude_basin, 
                                          avg_altitude_glacier, Sm, area_basin, area_glacier,
                                          degree_day_factor_glacier, degree_day_factor_snow, 
                                          start_date, frequency_date, r_arc, r_aero, gs,
                                          elevation_bands){
  
  # PET 
  PET_calculation_eto_daily_calibration(LAI = LAI, rh = rh, ra = ra, avg_temp = avg_temp, 
                                        avg_altitude_basin = avg_altitude_basin, 
                                        r_arc = r_arc, r_aero = r_aero, gs = gs) -> PET_model
  PET_model*1000*24*3600 -> PET_mm_model # daily
  
  # Soil Moisture
  soil_moisture_calculation_daily(precip_data = precip_data, 
                                  Eto = PET_mm_model, 
                                  Sm = Sm, start_date = start_date, 
                                  frequency_date = frequency_date) -> St_model
  aet_calculation_daily(precip_data, PET_mm_model, Sm, start_date, 
                        frequency_date) -> AET_model
  
  
  
  # Glacier and Snow melt
  
  rain_by_elevation_bands(elevation_bands = elevation_bands, 
                          avg_temp = avg_temp, 
                          avg_altitude_basin = avg_altitude_basin,
                          precip_daily = precip_data) -> rain_by_elevation_model
  
  rain_on_glacier_runoff_historical_daily(temp_avg_dataset = avg_temp, 
                                          avg_altitude_basin = avg_altitude_basin, 
                                          avg_altitude_glacier = avg_altitude_glacier,
                                          precipitation_daily = precip_data,
                                          glacier_area = area_glacier) -> rain_on_glacier_model ## Already in MM3
  
  glacier_runoff_historical_daily(temp_avg_dataset = avg_temp, 
                                  degree_day_factor = degree_day_factor_glacier, 
                                  avg_altitude_basin = avg_altitude_basin,
                                  avg_altitude_glacier = avg_altitude_glacier,
                                  glacier_area = area_glacier) -> glacier_melt_runoff_model
  
  adjusted_elevation_bands_glacier(elevation_bands, 
                                   area_glacier) -> adjusted_elevation_bands
  
  # Net Runoff
  
  snowmelt_calculations(elevation_bands = elevation_bands, 
                        avg_temp = avg_temp, 
                        degree_day_factor = degree_day_factor_snow, 
                        precip_data = precip_data,
                        basin_altitude = avg_altitude_basin,
                        glacier_area = area_glacier,
                        frequency_date = frequency_date, 
                        start_date = start_date) -> total_snowmelt_basin_ts
  
  mapply(`*`, rain_by_elevation_model, 
         adjusted_elevation_bands) -> rain_by_elevation
  rowSums(rain_by_elevation) -> total_rain_basin
  ts(total_rain_basin, frequency=frequency_date, start=start_date) -> total_rain_basin_ts
  
  St_model*(area_basin - area_glacier) -> total_St_model
  AET_model*(area_basin - area_glacier) -> total_AET_model
  
  stats::lag(total_St_model) + total_rain_basin_ts + total_snowmelt_basin_ts + glacier_melt_runoff_model + 
    rain_on_glacier_model- total_AET_model - 
    total_St_model -> net_runoff_model
  ifelse(net_runoff_model<0,0,net_runoff_model) -> net_runoff_model
  stats::lag(total_St_model) - total_St_model -> Delta_Sm
  
  net_runoff_variables <- list(total_rain_basin_ts, total_snowmelt_basin_ts, 
                               glacier_melt_runoff_model, rain_on_glacier_model,
                               total_AET_model, Delta_Sm, net_runoff_model)
  
  return(net_runoff_variables)
}

## Projections Model ----

### Projections Net Runoff

runoff_model_daily_projections <- function(LAI, rh, ra, avg_temp, precip_data, avg_altitude_basin, 
                                           avg_altitude_glacier, Sm, area_basin, area_glacier,
                                           degree_day_factor_glacier, degree_day_factor_snow, 
                                           start_date, frequency_date, r_arc, r_aero, gs, elevation_bands,
                                           glacier_blocks){
  
  # PET 
  PET_calculation_eto_daily_calibration(LAI, rh, ra, avg_temp, avg_altitude_basin, r_arc, r_aero, gs) -> PET_model
  PET_model*1000*24*3600 -> PET_mm_model # daily
  
  # Soil Moisture
  soil_moisture_calculation_daily(precip_data = precip_data, 
                                  Eto = PET_mm_model, 
                                  Sm = Sm, 
                                  start_date = start_date, 
                                  frequency_date = frequency_date) -> St_model
  aet_calculation_daily(precip_data = precip_data, 
                        Eto = PET_mm_model, 
                        Sm = Sm,
                        start_date = start_date, 
                        frequency_date = frequency_date) -> AET_model
  
  # Glacier and Snow melt
  
  rain_by_elevation_bands(elevation_bands = elevation_bands, 
                          avg_temp = avg_temp, 
                          avg_altitude_basin = avg_altitude_basin,
                          precip_daily = precip_data) -> rain_by_elevation_model
  
  glacier_melt_area_daily(temp_avg_dataset = avg_temp, 
                          degree_day_factor = degree_day_factor_glacier,
                          avg_altitude_basin = avg_altitude_basin,
                          avg_altitude_glacier = avg_altitude_glacier,
                          glacier_area = area_glacier,
                          precipitation_daily = precip_data, start_date = start_date,
                          frequency_date = frequency_date,
                          glacier_blocks = glacier_blocks) -> glacier_area_ts
  
  glacier_runoff_daily(temp_avg_dataset = avg_temp, 
                       degree_day_factor = degree_day_factor_glacier, 
                       avg_altitude_basin = avg_altitude_basin,
                       avg_altitude_glacier = avg_altitude_glacier,
                       glacier_area = area_glacier, 
                       precipitation_daily = precip_data, start_date = start_date,
                       frequency_date = frequency_date,
                       glacier_blocks = glacier_blocks) -> glacier_melt_runoff_model ## Already in MM3
  
  rain_on_glacier_runoff_daily(temp_avg_dataset = avg_temp, 
                               degree_day_factor = degree_day_factor_glacier,
                               avg_altitude_basin = avg_altitude_basin, 
                               avg_altitude_glacier = avg_altitude_glacier, 
                               glacier_area = area_glacier, 
                               precipitation_daily = precip_data, 
                               start_date = start_date, 
                               frequency_date = frequency_date,
                               glacier_blocks = glacier_blocks) -> rain_on_glacier_model ## Already in MM3
  
  adjusted_elevation_bands_glacier(elevation_bands = elevation_bands, 
                                   glacier_area_ts = glacier_area_ts) -> adjusted_elevation_bands_area
  
  # Net Runoff
  
  snowmelt_calculations(elevation_bands = elevation_bands, 
                        avg_temp = avg_temp, 
                        degree_day_factor = degree_day_factor_snow, 
                        precip_data = precip_data,
                        basin_altitude = avg_altitude_basin,
                        glacier_area = glacier_area_ts,
                        frequency_date = frequency_date, 
                        start_date = start_date) -> total_snowmelt_basin_ts ## MODIFY THIS
  
  rain_by_elevation_model*adjusted_elevation_bands_area -> rain_by_elevation
  rowSums(rain_by_elevation) -> total_rain_basin
  ts(total_rain_basin, frequency=frequency_date, start=start_date) -> total_rain_basin_ts
  
  rowSums(adjusted_elevation_bands_area) -> basin_area_modified
  ts(basin_area_modified, frequency=frequency_date, start=start_date) -> basin_area_modified_ts
  
  St_model*(basin_area_modified_ts) -> total_St_model
  AET_model*(basin_area_modified_ts) -> total_AET_model
  
  stats::lag(total_St_model) + total_rain_basin_ts + total_snowmelt_basin_ts + 
    glacier_melt_runoff_model + rain_on_glacier_model - 
    total_AET_model - total_St_model -> net_runoff_model
  ifelse(net_runoff_model<0,0,net_runoff_model) -> net_runoff_model
  
  return(net_runoff_model)
}

### Projections Variables Separated Runoff - CODE

runoff_model_daily_projections_variables <- function(LAI, rh, ra, avg_temp, precip_data, avg_altitude_basin, 
                                                     avg_altitude_glacier, Sm, area_basin, area_glacier,
                                                     degree_day_factor_glacier, degree_day_factor_snow, 
                                                     start_date, frequency_date, r_arc, r_aero, gs, 
                                                     elevation_bands, glacier_blocks){
  
  # PET 
  PET_calculation_eto_daily_calibration(LAI, rh, ra, avg_temp, avg_altitude_basin, r_arc, r_aero, gs) -> PET_model
  PET_model*1000*24*3600 -> PET_mm_model # daily

  # Soil Moisture
  soil_moisture_calculation_daily(precip_data = precip_data, 
                                  Eto = PET_mm_model, 
                                  Sm = Sm, 
                                  start_date = start_date, 
                                  frequency_date = frequency_date) -> St_model
  aet_calculation_daily(precip_data = precip_data, 
                        Eto = PET_mm_model, 
                        Sm = Sm,
                        start_date = start_date, 
                        frequency_date = frequency_date) -> AET_model
  
  # Glacier and Snow melt
  
  rain_by_elevation_bands(elevation_bands = elevation_bands, 
                          avg_temp = avg_temp, 
                          avg_altitude_basin = avg_altitude_basin,
                          precip_daily = precip_data) -> rain_by_elevation_model
  
  glacier_melt_area_daily(temp_avg_dataset = avg_temp, 
                          degree_day_factor = degree_day_factor_glacier,
                          avg_altitude_basin = avg_altitude_basin,
                          avg_altitude_glacier = avg_altitude_glacier,
                          glacier_area = area_glacier,
                          precipitation_daily = precip_data, start_date = start_date,
                          frequency_date = frequency_date,
                          glacier_blocks = glacier_blocks) -> glacier_area_ts
  
  glacier_runoff_daily(temp_avg_dataset = avg_temp, 
                       degree_day_factor = degree_day_factor_glacier, 
                       avg_altitude_basin = avg_altitude_basin,
                       avg_altitude_glacier = avg_altitude_glacier,
                       glacier_area = area_glacier, 
                       precipitation_daily = precip_data, start_date = start_date,
                       frequency_date = frequency_date,
                       glacier_blocks = glacier_blocks) -> glacier_melt_runoff_model ## Already in MM3
  
  rain_on_glacier_runoff_daily(temp_avg_dataset = avg_temp, 
                               degree_day_factor = degree_day_factor_glacier,
                               avg_altitude_basin = avg_altitude_basin, 
                               avg_altitude_glacier = avg_altitude_glacier, 
                               glacier_area = area_glacier, 
                               precipitation_daily = precip_data, 
                               start_date = start_date, 
                               frequency_date = frequency_date,
                               glacier_blocks = glacier_blocks) -> rain_on_glacier_model ## Already in MM3
  
  adjusted_elevation_bands_glacier(elevation_bands = elevation_bands, 
                                   glacier_area_ts = glacier_area_ts) -> adjusted_elevation_bands_area
  
  # Net Runoff
  
  snowmelt_calculations(elevation_bands = elevation_bands, 
                        avg_temp = avg_temp, 
                        degree_day_factor = degree_day_factor_snow, 
                        precip_data = precip_data,
                        basin_altitude = avg_altitude_basin,
                        glacier_area = glacier_area_ts,
                        frequency_date = frequency_date, 
                        start_date = start_date) -> total_snowmelt_basin_ts ## MODIFY THIS
  
  rain_by_elevation_model*adjusted_elevation_bands_area -> rain_by_elevation
  rowSums(rain_by_elevation) -> total_rain_basin
  ts(total_rain_basin, frequency=frequency_date, start=start_date) -> total_rain_basin_ts
  
  rowSums(adjusted_elevation_bands_area) -> basin_area_modified
  ts(basin_area_modified, frequency=frequency_date, start=start_date) -> basin_area_modified_ts
  
  St_model*(basin_area_modified_ts) -> total_St_model
  AET_model*(basin_area_modified_ts) -> total_AET_model
  
  stats::lag(total_St_model) + total_rain_basin_ts + total_snowmelt_basin_ts + 
    glacier_melt_runoff_model + rain_on_glacier_model - 
    total_AET_model - total_St_model -> net_runoff_model
  ifelse(net_runoff_model<0,0,net_runoff_model) -> net_runoff_model
  stats::lag(total_St_model) - total_St_model -> Delta_Sm
  
  net_runoff_variables <- list(total_rain_basin_ts, total_snowmelt_basin_ts, 
                               glacier_melt_runoff_model, rain_on_glacier_model,
                               total_AET_model, Delta_Sm, net_runoff_model)
  
  return(net_runoff_variables)
}

