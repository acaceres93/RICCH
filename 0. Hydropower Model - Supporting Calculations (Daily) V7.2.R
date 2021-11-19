# Supporting Functions Runoff Model

### Precipitation - Snow + Rain ----

snow_melt_calculations_model_daily <- function(temp_avg_dataset, 
                                               degree_day_factor,
                                               avg_altitude_basin, 
                                               avg_altitude_elevation_band){
  (temp_avg_dataset - 0.0065*(avg_altitude_elevation_band - avg_altitude_basin)) -> temp_elevation_band
  ifelse(temp_elevation_band < 0, 0, temp_elevation_band) -> degree_days
  degree_days*degree_day_factor -> potential_daily_melt
  return(potential_daily_melt) # mm/day potential melt 
} # degree days

snow_accumulation_model_daily <- function(temp_avg_dataset, 
                                          degree_day_factor,
                                          avg_altitude_basin, 
                                          avg_altitude_elevation_band, 
                                          precipitation_daily){
  (temp_avg_dataset - 0.0065*(avg_altitude_elevation_band - avg_altitude_basin)) -> temp_elevation_band
  ifelse(temp_elevation_band < 0, 1, 0) -> accumulation_day
  accumulation_day*precipitation_daily -> potential_daily_accumulation
  return(potential_daily_accumulation) ## mm/day accumulated
} 

rain_model_elevation_band_daily <- function(temp_avg_dataset, 
                                            avg_altitude_basin, 
                                            avg_altitude_elevation_band,
                                            precipitation_daily){
  (temp_avg_dataset - 0.0065*(avg_altitude_elevation_band - avg_altitude_basin)) -> temp_elevation_band
  ifelse(temp_elevation_band < 0, 0, 1) -> rain_day
  rain_day*precipitation_daily -> daily_rain
  return(daily_rain)
} 

## Snowmelt

potential_snow_melt_bands <- function(elevation_bands, # mm/day melt
                                      avg_temp,
                                      degree_day_factor_snow,
                                      basin_altitude){
  
  potential_snowmelt <- matrix(ncol=nrow(elevation_bands), 
                               nrow = length(avg_temp))
  
  for (i in 1:nrow(elevation_bands)) {
    potential_snowmelt[,i] <-  snow_melt_calculations_model_daily(temp_avg_dataset = 
                                                                    avg_temp,
                                                                  degree_day_factor = 
                                                                    degree_day_factor_snow, 
                                                                  avg_altitude_basin = 
                                                                    basin_altitude,
                                                                  avg_altitude_elevation_band = 
                                                                    elevation_bands[i, "Altitude"])
  }
  potential_snowmelt_mm <- as.data.frame(potential_snowmelt)
  return(potential_snowmelt_mm) # mm/day
}

potential_snow_accumulation_bands <- function(elevation_bands, # mm/day accumulated
                                              avg_temp,
                                              precip_data,
                                              degree_day_factor_snow,
                                              basin_altitude){
  
  potential_snow_accumulation <- matrix(ncol=nrow(elevation_bands), 
                                        nrow = length(avg_temp))
  
  for (i in 1:nrow(elevation_bands)) {
    potential_snow_accumulation[,i] <-  snow_accumulation_model_daily(temp_avg_dataset = 
                                                                        avg_temp,
                                                                      degree_day_factor = 
                                                                        degree_day_factor_snow, 
                                                                      avg_altitude_basin = 
                                                                        basin_altitude,
                                                                      avg_altitude_elevation_band = 
                                                                        elevation_bands[i, "Altitude"],
                                                                      precipitation_daily = 
                                                                        precip_data)
  }
  potential_accumulation <- as.data.frame(potential_snow_accumulation)
  
  return(potential_accumulation) # mm/day
}

snowmelt_calculations <- function(elevation_bands, 
                                  avg_temp, 
                                  degree_day_factor, 
                                  precip_data,
                                  basin_altitude,
                                  glacier_area,
                                  frequency_date, 
                                  start_date){
  
  potential_snow_melt_bands(elevation_bands = elevation_bands,
                            avg_temp = avg_temp,
                            degree_day_factor_snow = degree_day_factor,
                            basin_altitude = basin_altitude) -> potential_snowmelt_model
  
  potential_snow_accumulation_bands(elevation_bands = elevation_bands, 
                                    avg_temp = avg_temp,
                                    precip_data = precip_data,
                                    degree_day_factor_snow = degree_day_factor,
                                    basin_altitude = basin_altitude) -> potential_snow_accumulation_model
  
  adjusted_elevation_bands_glacier(elevation_bands = elevation_bands, 
                                   glacier_area_ts = glacier_area ) -> adjusted_elevation_bands
  
  potential_snow_accumulation_model*adjusted_elevation_bands -> potential_accumulated_snow
  potential_snowmelt_model*adjusted_elevation_bands -> potential_snowmelt
  
  ts(potential_accumulated_snow, frequency=frequency_date, start=start_date) -> potential_accumulated_snow_ts
  ts(potential_snowmelt, frequency=frequency_date, start=start_date) -> potential_snowmelt_ts
  
  ifelse(stats::lag(potential_accumulated_snow_ts) + potential_accumulated_snow_ts - potential_snowmelt_ts < 0, 
         0, stats::lag(potential_accumulated_snow_ts) + potential_accumulated_snow_ts - potential_snowmelt_ts) -> accumulated_snow_ts
  ifelse(stats::lag(accumulated_snow_ts) <= 0, 0, ifelse(stats::lag(accumulated_snow_ts) - potential_snowmelt_ts <0, 
                                                         stats::lag(accumulated_snow_ts), potential_snowmelt_ts)) -> snowmelt_ts
  rowSums(snowmelt_ts) -> total_snowmelt_basin 
  ts(total_snowmelt_basin, frequency=frequency_date, start=start_date) -> total_snowmelt_basin_ts
  return(total_snowmelt_basin_ts)
}

## Rain

rain_by_elevation_bands <- function(elevation_bands, # mm/day melt
                                    avg_temp, 
                                    avg_altitude_basin,
                                    precip_daily){
  
  rain_elevation_band <- matrix(ncol=nrow(elevation_bands), 
                                nrow = length(precip_daily))
  
  for (i in 1:nrow(elevation_bands)) {
    rain_elevation_band[,i] <-  rain_model_elevation_band_daily(temp_avg_dataset = avg_temp,
                                                                avg_altitude_basin = avg_altitude_basin,
                                                                avg_altitude_elevation_band = 
                                                                  elevation_bands[i, "Altitude"],
                                                                precipitation_daily = precip_daily)
  }
  rain_elevation_band <- as.data.frame(rain_elevation_band)
  
  return(rain_elevation_band) # mm/day
}

## Glacier Melt ----

### Potential Melt

glacier_melt_calculations_model_daily <- function(temp_avg_dataset, degree_day_factor,
                                                  avg_altitude_basin, avg_altitude_glacier){
  (temp_avg_dataset - 0.0065*(avg_altitude_glacier - avg_altitude_basin)) -> temp_glacier
  ifelse(temp_glacier < 0, 0, temp_glacier) -> degree_days
  degree_days*degree_day_factor -> potential_daily_melt
  return(potential_daily_melt)
} # degree days

### Rain on Glacier

glacier_precipitation_model_daily <- function(temp_avg_dataset, 
                                              avg_altitude_basin, 
                                              avg_altitude_glacier, 
                                              precipitation_daily){
  (temp_avg_dataset - 0.0065*(avg_altitude_glacier - avg_altitude_basin)) -> temp_glacier
  ifelse(temp_glacier < 0, 0, 1) -> precipitation_day
  precipitation_day*precipitation_daily -> precipitation_on_glacier_day
  return(precipitation_on_glacier_day)
} 

### Accumulation

glacier_accumulation_model_daily <- function(temp_avg_dataset, degree_day_factor,
                                             avg_altitude_basin, avg_altitude_glacier, 
                                             precipitation_daily){
  (temp_avg_dataset - 0.0065*(avg_altitude_glacier - avg_altitude_basin)) -> temp_glacier
  ifelse(temp_glacier < 0, 1, 0) -> accumulation_day
  accumulation_day*precipitation_daily -> potential_daily_accumulation
  return(potential_daily_accumulation)
} 

### Area Time Series 

glacier_melt_area_model_daily <- function(potential_glacier_melt, 
                                          potential_glacier_accumulation, 
                                          glacier_area, 
                                          start_date, 
                                          frequency_date, 
                                          glacier_blocks){
  glacier_area_1 <- glacier_area
  glacier_area_ts <- ts(rep.int(0,length(potential_glacier_melt)), 
                        frequency=frequency_date, start=start_date)
  glacier_volume_ts <- ts(rep.int(0,length(potential_glacier_melt)), 
                          frequency=frequency_date, start=start_date)
  for (i in 1:length(potential_glacier_melt)){
    glacier_volume <- (glacier_area_1^1.375)*1000
    glacier_volume_ts[i] <- ifelse((glacier_volume - potential_glacier_melt[i]*glacier_area_1/(10^3) 
                                    + potential_glacier_accumulation[i]*glacier_area_1/10^3)<0,0,
                                   (glacier_volume - potential_glacier_melt[i]*glacier_area_1/(10^3) 
                                    + potential_glacier_accumulation[i]*glacier_area_1/(10^3)))
    glacier_area_ts[i] <- (glacier_volume_ts[i]/1000)^(1/1.375)
    glacier_area_1 <- glacier_area_ts[i]
  }
  glacier_area_ts <- glacier_area_ts*glacier_blocks
  return(glacier_area_ts)
}

glacier_melt_area_daily <- function(temp_avg_dataset, degree_day_factor,
                                    avg_altitude_basin, avg_altitude_glacier, 
                                    glacier_area, precipitation_daily, start_date, 
                                    frequency_date, glacier_blocks){
  glacier_melt_calculations_model_daily(temp_avg_dataset=temp_avg_dataset, 
                                        degree_day_factor=degree_day_factor,
                                        avg_altitude_basin=avg_altitude_basin, 
                                        avg_altitude_glacier=avg_altitude_glacier) -> potential_melt
  glacier_accumulation_model_daily(temp_avg_dataset = temp_avg_dataset, 
                                   degree_day_factor = degree_day_factor, 
                                   avg_altitude_basin = avg_altitude_basin, 
                                   avg_altitude_glacier = avg_altitude_glacier, 
                                   precipitation_daily = precipitation_daily) -> potential_accumulation
  glacier_melt_area_model_daily(potential_glacier_melt = potential_melt, 
                                potential_glacier_accumulation = potential_accumulation, 
                                glacier_area = glacier_area, start_date = start_date,
                                frequency_date = frequency_date, 
                                glacier_blocks = glacier_blocks) -> glacier_area_ts
  return(glacier_area_ts)
}

### Glacier Runoff

glacier_runoff_daily <- function(temp_avg_dataset, degree_day_factor,
                                 avg_altitude_basin, avg_altitude_glacier, 
                                 glacier_area, precipitation_daily, 
                                 start_date, frequency_date, glacier_blocks){
  glacier_melt_calculations_model_daily(temp_avg_dataset=temp_avg_dataset, 
                                        degree_day_factor=degree_day_factor,
                                        avg_altitude_basin=avg_altitude_basin, 
                                        avg_altitude_glacier=avg_altitude_glacier) -> potential_melt
  glacier_accumulation_model_daily(temp_avg_dataset = temp_avg_dataset, 
                                   degree_day_factor = degree_day_factor, 
                                   avg_altitude_basin = avg_altitude_basin, 
                                   avg_altitude_glacier = avg_altitude_glacier, 
                                   precipitation_daily = precipitation_daily) -> potential_accumulation
  glacier_melt_area_model_daily(potential_glacier_melt = potential_melt, 
                                potential_glacier_accumulation = potential_accumulation, 
                                glacier_area = glacier_area, start_date = start_date,
                                frequency_date = frequency_date,
                                glacier_blocks = glacier_blocks) -> glacier_area_ts
  (potential_melt)*glacier_area_ts -> melt_runoff
  return(melt_runoff)
}

### Rain on Glacier Runoff

rain_on_glacier_runoff_daily <- function(temp_avg_dataset, degree_day_factor,
                                         avg_altitude_basin, avg_altitude_glacier, 
                                         glacier_area, precipitation_daily, 
                                         start_date, frequency_date, glacier_blocks){
  glacier_melt_calculations_model_daily(temp_avg_dataset=temp_avg_dataset, 
                                        degree_day_factor=degree_day_factor,
                                        avg_altitude_basin=avg_altitude_basin, 
                                        avg_altitude_glacier=avg_altitude_glacier) -> potential_melt
  glacier_precipitation_model_daily(temp_avg_dataset = temp_avg_dataset, 
                                    avg_altitude_basin = avg_altitude_basin,
                                    avg_altitude_glacier = avg_altitude_glacier, 
                                    precipitation_daily = precipitation_daily) -> rain_on_glacier_area
  glacier_accumulation_model_daily(temp_avg_dataset = temp_avg_dataset, 
                                   degree_day_factor = degree_day_factor, 
                                   avg_altitude_basin = avg_altitude_basin, 
                                   avg_altitude_glacier = avg_altitude_glacier, 
                                   precipitation_daily = precipitation_daily) -> potential_accumulation
  glacier_melt_area_model_daily(potential_glacier_melt = potential_melt, 
                                potential_glacier_accumulation = potential_accumulation, 
                                glacier_area = glacier_area, start_date = start_date,
                                frequency_date = frequency_date,
                                glacier_blocks = glacier_blocks) -> glacier_area_ts
  (rain_on_glacier_area)*glacier_area_ts -> rain_on_glacier_area_runoff
  return(rain_on_glacier_area_runoff)
}

## Adjusted Elevation Bands for Runoff Calculations 

adjusted_elevation_bands_glacier <- function(elevation_bands, 
                                             glacier_area_ts){
  bands <- nrow(elevation_bands)
  glacier_area <- glacier_area_ts
  new_elevation_bands <- matrix(nrow=length(glacier_area_ts), ncol = bands)
  for (i in 1:nrow(elevation_bands)){
    new_elevation_bands[,bands+1-i] <- ifelse(
      glacier_area > elevation_bands$Area[bands+1-i], 0, 
      elevation_bands$Area[bands+1-i] - glacier_area)
    glacier_area <- glacier_area - elevation_bands$Area[(bands+1-i)]
    glacier_area <- ifelse(glacier_area > 0, glacier_area, 0)
  }
  return(new_elevation_bands)
}


### Historical Calculations for Glacier Melt

glacier_runoff_historical_daily <- function(temp_avg_dataset, degree_day_factor,
                                            avg_altitude_basin, avg_altitude_glacier, 
                                            glacier_area){
  glacier_melt_calculations_model_daily(temp_avg_dataset=temp_avg_dataset, 
                                        degree_day_factor=degree_day_factor,
                                        avg_altitude_basin=avg_altitude_basin, 
                                        avg_altitude_glacier=avg_altitude_glacier) -> potential_melt
  potential_melt*glacier_area -> melt_runoff
  return(melt_runoff)
}

rain_on_glacier_runoff_historical_daily <- function(temp_avg_dataset, 
                                                    avg_altitude_basin, 
                                                    avg_altitude_glacier,
                                                    precipitation_daily,
                                                    glacier_area){
  glacier_precipitation_model_daily(temp_avg_dataset, 
                                    avg_altitude_basin, 
                                    avg_altitude_glacier, 
                                    precipitation_daily) -> rain_on_glacier
  rain_on_glacier*glacier_area -> rain_on_glacier_runoff
  return(rain_on_glacier_runoff)
}

### Potential Evapotranspiration Calculations ----


PET_calculation_eto_daily_calibration <- function(LAI, rh, ra, avg_temp, 
                                                  avg_altitude_basin, 
                                                  r_arc, r_aero, gs){
  
  temp_celsius <- avg_temp
  temp_kelvin <- temp_celsius + 273
  
  r_aero <- r_aero # s/m
  ga <- 1/r_aero # m/s
  
  LAI <- LAI # (m2/m2)
  ga_asterisc <- ga*40/LAI # mol/m2/s
  
  r_arc <- r_arc # s/m (assumption)
  gb <- 1/r_arc/LAI # m/s
  gb_asterisc <- gb*40 # mol/m2/s
  
  gab_CO2 <- 1/(1/ga_asterisc+1.37/gb_asterisc)
  
  slope <- 610.8*exp(17.27*temp_celsius/(temp_celsius+237.3))*17.27*237.3/((temp_celsius+237.3)^2) # PA/K
  es <- 610.8*exp(17.27*temp_celsius/(temp_celsius+237.3))
  
  lapse_rate <- -0.0065 # K/m
  cp <- 1012 # J/Kg/K Specific Heat of Air
  ps <- 101300 # PA Air pressure at sea level
  z <- avg_altitude_basin #m Elevation
  
  Lv <- 2501000-2361*temp_celsius # J/Kg latent heat of evaporation
  h <- 287/9.81*(temp_kelvin+0.5*z*lapse_rate) # m standard height
  pz <- ps*exp(-z/h) # Pa Pressure at z
  gamma <- cp/0.622*pz/Lv #PA/K
  rho_air <- 0.003486*pz/(275+temp_celsius) #Kg/m3 Air density
  
  rho_w <- 1000 #kg/m3 water density
  Rad <- ra # W/m2 Longwave Radiation
  Rh <- rh # relative humidity
  VPD <- es*(100-Rh)/100
  
  gs <- gs/1000 # m/s Stomatal Conductance
  
  Eto <- (slope*Rad + rho_air*cp*VPD*ga)/(rho_w*Lv*(slope+gamma))
  
  return(Eto)
}

soil_moisture_calculation_daily <- function(precip_data, Eto, Sm, start_date, frequency_date){
  St_1 <- Sm/2 
  St <- ts(rep.int(0,length(precip_data)), frequency=frequency_date, start=start_date)
  AET <- ts(rep.int(0,length(precip_data)), frequency=frequency_date, start=start_date)
  for (i in 1:length(Eto)){
    beta <- (5*(St_1/Sm) - 2*(St_1/Sm)^2)/3
    gamma <- (1-exp(-1*St_1/Sm))/(1-exp(-1))
    AET[i] <- beta*Eto[i]
    AET[i] <- ifelse(AET[i]<0,0,AET[i])
    St[i] <- ifelse((St_1 + precip_data[i] - Eto[i]) >= Sm, Sm, ifelse((gamma*St_1 - AET[i]) < 0, 0, ifelse(gamma*St_1 + precip_data[i] - AET[i]<= Sm, gamma*St_1 + precip_data[i] - AET[i], Sm )))
    St_1 <- St[i]
  }
  return(St)
}

aet_calculation_daily <- function(precip_data, Eto, Sm, start_date, frequency_date){
  St_1 <- Sm/2
  St <- ts(rep.int(0,length(precip_data)), frequency=frequency_date, start=start_date)
  AET <- ts(rep.int(0,length(precip_data)), frequency=frequency_date, start=start_date)
  for (i in 1:length(Eto)){
    beta <- (5*(St_1/Sm) - 2*(St_1/Sm)^2)/3
    gamma <- (1-exp(-1*St_1/Sm))/(1-exp(-1))
    AET[i] <- beta*Eto[i]
    AET[i] <- ifelse(AET[i]<0,0,AET[i])
    St[i] <- ifelse((St_1 + precip_data[i] - Eto[i]) >= Sm, Sm, ifelse((gamma*St_1 - AET[i]) < 0 , 0, ifelse(gamma*St_1 + precip_data[i] - AET[i]<= Sm, gamma*St_1 + precip_data[i] - AET[i], Sm )))
    St_1 <- St[i]
  }
  return(AET)
}


# Elevation Bands ----

elevation_bands_dataframe <- function(dem_basin,
                                      basin_name){
  cut(dem_basin, breaks=c(0,500,1000,1500,
                          2000,2500,3000,3500,
                          4000,4500,5000,5500,
                          6000,6500,7000)) -> reclassified_dem
  rasterToPolygons(reclassified_dem, dissolve=TRUE) -> pols_dem
  as.data.frame(extract(dem_basin, pols_dem, fun=mean)) -> elevation_dataframe
  area(pols_dem)/10^6 -> elevation_dataframe$area
  elevation_dataframe[order(elevation_dataframe$V1),] -> elevation_dataframe
  c("Altitude", "Area") -> colnames(elevation_dataframe)
  write.csv(elevation_dataframe,
            paste0("elevation_bands_", basin_name, ".csv"))
}

