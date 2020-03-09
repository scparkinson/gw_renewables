

# Clear memory and close all windows
rm(list = ls())
graphics.off()

# Load the required libraries
library(raster)
library(rasterVis)
library(countrycode)
library(rgeos)
library(rgdal)
library(tidyverse)
library(dplyr)
library(ncdf4)
library(abind)
library(geosphere)
library(overpass)
library(geobuffer)
memory.limit(size=1e9)

# Open gw stress locations
# gw.df = data.frame( read.csv( 'H:/wada_groundwaterdepletion_2010.csv', stringsAsFactors=FALSE ) )
# gw.df$mcm = round( gw.df$mcm, digits = 6 )

# # Wind
# wind_potential.rds = readRDS('H:/IIASA-WaterAndEnergy/WATER_STRESS/IIASA_WATER_STRESSED_WIND.rds')
# mnth = sapply( unlist( wind_potential.rds[,1] ), function(a){ as.numeric(unlist(strsplit( as.character(a[1]),'-'))[2]) } )
# gw.df$wind_cf = sapply( 1:nrow(gw.df), function(iii){ round( mean( wind_potential.rds[ , iii+1 ] ), digits = 3) } )
# tmp = data.frame( do.call( cbind, lapply( 1:12, function(m){ sapply( 1:nrow(gw.df), function(iii){ round( mean( wind_potential.rds[ which( mnth == m ) , iii+1 ] ), digits = 3) } ) } ) ) )
# names(tmp) = paste('wind_cf',c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'),sep='.')
# gw.df = cbind( gw.df, tmp )
# rm(wind_potential.rds, tmp)
# gc()

# # Solar
# solar_potential.rds = readRDS('H:/IIASA-WaterAndEnergy/WATER_STRESS/IIASA_WATER_STRESSED_SOLAR.rds')
# mnth = sapply( unlist( solar_potential.rds[,1] ), function(a){ as.numeric(unlist(strsplit( as.character(a[1]),'-'))[2]) } )
# gw.df$solar_cf = sapply( 1:nrow(gw.df), function(iii){ round( mean( solar_potential.rds[ , iii+1 ] ), digits = 3) } )
# tmp = do.call( cbind, lapply( 1:12, function(m){ sapply( 1:nrow(gw.df), function(iii){ round( mean( solar_potential.rds[ which( mnth == m ) , iii+1 ] ), digits = 3 ) } ) } ) )
# names(tmp) = paste('solar_cf',c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'),sep='.')
# gw.df = cbind( gw.df, tmp )
# rm(solar_potential.rds, tmp)
# gc()

# # Create 200 km buffer around the stressed locations
# spts = SpatialPoints( gw.df[,c('x','y') ], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") )
# buff.sp = spTransform( geobuffer_pts(xy = gw.df[,c('x','y') ], dist_m = 200*10^3), crs(spts ) )
	
# # Distance to transmission (openstreetmap)
# # Grab electricity transmission data corresponding to extent from basin polygon
# ltypes = c('line','cable','minor_line')	
# trans_dist = NULL
# for( iii in 1:length(buff.sp) ) 
	# {
	
	# print( iii )
	
	# # Get transmission data from openstreetmap api
	# dis = lapply( ltypes, function(ltype){
		# ext = paste('(',
					# paste(	bbox(buff.sp[iii])[2,1] , 
							# bbox(buff.sp[iii])[1,1], 
							# bbox(buff.sp[iii])[2,2], 
							# bbox(buff.sp[iii])[1,2],
							# sep=','),
					# ')',sep='')
	    # pth = paste('[out:xml][timeout:100];(node["power"="',
					# ltype,
					# '"]',
					# ext,
					# ';way["power"="',
					# ltype,
					# '"]',
					# ext,
					# ';relation["power"="',
					# ltype,
					# '"]',
					# ext,
					# ';);out body;>;out skel qt;',
					# sep = '' ) 
		# fl = 1
		# while( fl == 1 ){
			# frb = tryCatch( overpass_query( pth, quiet = TRUE ), error = function(e){0} )
			# if( !is.null( frb ) ){ 
				# if( class(frb) == "SpatialLinesDataFrame" | class(frb) == "SpatialPointsDataFrame"  ){ 
					# fl = 0 
					# }
				# else{
					# fl = 1
					# } 
				# }
			# else{ 
				# fl = 0 
				# } 
			# }
		# if(!is.null(frb) )
			# {
			# crs(frb) = crs(buff.sp)
			# utmzone = floor( (spts[iii]$x+180)/6 %% 60) +1
			# projCRS = paste0("+proj=utm +zone=", utmzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
			# ds = gDistance( spTransform( spts[iii], projCRS ), spTransform( frb, projCRS ))/1e3 # distance in km
			# }else{
			# ds = NA
			# }
		# return( ds )
		# } )
	# names(dis) = ltypes
	
	# print(dis)
	
	# trans_dist = c( trans_dist, min( unlist( dis ), na.rm=TRUE ) )
	
	# }
	
# gw.df = cbind( gw.df, data.frame( dist2trans_km = trans_dist ) )

# write.csv(gw.df,'C:/Users/parkinso/Documents/gw_renewables3.csv',row.names=FALSE)

# load data from script above saved previously
gw.df = read.csv('C:/Users/parkinso/Documents/gw_renewables_new.csv',stringsAsFactors=FALSE) %>%
	dplyr::select( "x", "y", "mcm", "wind_cf", "solar_cf", "dist2trans_km" ) 
spts = SpatialPoints( gw.df[,c('x','y') ], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") )
buff.sp = spTransform( geobuffer_pts(xy = gw.df[,c('x','y') ], dist_m = 200*10^3), crs(spts ) )
buff2.sp = spTransform( geobuffer_pts(xy = gw.df[,c('x','y') ], dist_m = 100*10^3), crs(spts ) )

# Get irrigated area in hectares GMIA v5
gmia.raster = raster( 'H:\\global_map_irrigated_areas_v5\\gmia_v5_aei_ha.asc' )
proj4string(gmia.raster) = proj4string(spts)
crs(gmia.raster) = crs(spts)
# make polygons for raster cells to estimate irrigated area in each groundwater stressed location
spts_grid = spts
gridded(spts_grid) = TRUE
spts_rstply = as( rasterToPolygons( raster( spts_grid ) ), 'SpatialPolygons' )
spts2 = spts_rstply[ which(!is.na(over(spts_rstply,spts,byid=TRUE))) ] 
gmia = unlist( lapply( raster::extract( gmia.raster, spts2, byid =TRUE ), sum ) )
# total area of grid cells
totarea = sapply( 1:length( spts2 ), function( iii ){
	utmzone = floor( (spts[iii]$x+180)/6 %% 60) + 1
	projCRS = paste0("+proj=utm +zone=", utmzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
	return( round( gArea( spTransform(spts2[iii],projCRS)) / 1e6 ) ) # in km2
	} )
gw.df = cbind( gw.df, data.frame( irrigated_area = gmia*0.01 ) ) # hectares to km2
gw.df = cbind( gw.df, data.frame( total_area = totarea ) )

# alternative from:
# Meier, J et al. (2018): A global approach to estimate irrigated areas – a comparison between different data and statistics. 
# Hydrology and Earth System Sciences, 22(2), 1119-1133, https://doi.org/10.5194/hess-22-1119-2018
irrmap.raster = raster( 'H:\\global_irrigated_areas\\global_irrigated_areas\\global_irrigated_areas.tif'  )


# Get the crop production from GAEZ
# Crop production data in 2000, summing irrigated and rainfed crops 
# data from GAEZ, original unit: 1000t -  this is the historical production capacity
# Crops to inlcude - must match the technology file
crop_names = c('wheat','rice','cotton','fodder','sugarcane','pulses','maize','fruit','vegetables')
vars = c('irr_', 'rain_', '')
tps = c('YIELD', 'YIELD', 'PROD')
gaez.df = bind_cols( lapply( seq_along(crop_names), function( ii ){
	bind_cols( lapply( 1:3, function( jj ){
		fld = list.dirs( path = paste0( 'H:\\land_maps_crop_yields\\', tps[ jj ] ) )
		fld = fld[ grep( paste0( vars[ jj ], crop_names[ii] ), fld ) ]
		fl = list.files( path = fld, pattern = '.tif' )
		# raster in metric tons DW per megahectare (tons/Mha)
		rs = raster( paste0( fld, '\\', fl ) )
		proj4string( rs ) = proj4string( spts2 )
		# 100 ha per 1 km2 -> 1e6 ha per 1 Mha -> 1e4 km2 per 1Mha : ton / Mha * 1 e-4 -> ton/km2 
		rs = rs * 1e-4
		# use mean yield in half degree cell
		lst = raster::extract( rs, spts2, byid =TRUE )
		df = data.frame( unlist( lapply( lst, mean, na.rm=TRUE ) ) )
		df[ is.na(df) ] = 0
		names( df ) = paste0( crop_names[ ii ], '.', vars[ jj ], tps[ jj ], '.', 'ton_per_km2' )
		return( df )
		} ) )
	} ) )
# merge into df tracking all parameters
gw.df = left_join( gw.df, cbind( data.frame( spts ), gaez.df ), by = c( 'x', 'y' ) )

# Country polygons
# Assemble the spatial polygons data.frame 
fls = list.files( path = 'P:\\ene.model\\data\\Water\\country_polygons' )
cntry.spdf = do.call( rbind, lapply( fls , function( iso3 ){ 
	ply = as( get( load( paste( 'P:\\ene.model\\data\\Water\\country_polygons',iso3, sep="\\" ) ) ), 'SpatialPolygons' )
	row.names(ply) = unlist(strsplit(iso3,'_'))[1]
	return( ply )
	} ) )
cntry.spdf = spTransform(cntry.spdf,crs(spts))
# match pts with country
cntry.df = bind_rows( lapply( 1:length(cntry.spdf), function(iii){
	pts_in_cnt = spts[ cntry.spdf[ iii, ], ]
	if( length( pts_in_cnt ) > 0 ){
		return( data.frame( pts_in_cnt ) %>% mutate( iso = names( cntry.spdf[ iii, ] ) ) )
		}else{
		return(NULL)
		}
	} ) )
# merge into df tracking all parameters
gw.df = left_join( gw.df, cntry.df, by = c('x','y') )

# Distance to populated area
urban_area.raster = raster( 'H:\\GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V1_0\\GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V1_0.tif' )
urban_area.raster = trunc( urban_area.raster / 10 ) # convert to L1 classification
tmp = spTransform(buff.sp,crs(urban_area.raster))
urban_area.df = bind_rows( lapply( 1:length(tmp), function(i){
	chk_rst = crop(urban_area.raster,extent(tmp[i]))
	df = NULL
	if( max(chk_rst[],na.rm=TRUE) > 1 ){ 
		xyz = rasterToPoints(chk_rst,spatial=TRUE)
		xyz = xyz[ xyz@data$layer > 1, ]
		utmzone = floor( (spts[i]$x+180)/6 %% 60) + 1
		projCRS = paste0("+proj=utm +zone=", utmzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
		ds = gDistance( spTransform( spts[i], projCRS ), spTransform( xyz, projCRS ))/1e3 # distance in km
		df = data.frame( spts[i] ) %>% mutate( urban_dist_km = ds, bigurban_dist_km = NA )
		if( max(chk_rst[],na.rm=TRUE) > 2 ){ 
			xyz = xyz[ xyz@data$layer > 2, ]
			ds = gDistance( spTransform( spts[i], projCRS ), spTransform( xyz, projCRS ))/1e3 # distance in km	
			df$bigurban_dist_km = ds
			}
		}	
	return( df )	
	} ) ) 
# merge into df tracking all parameters
gw.df = left_join( gw.df, urban_area.df, by = c('x','y') )
	
# Grid emission factor from IGES database from Sept 2019
# This is the data reported at country-level for CDM project analysis
iges_grid_ef_tco2_mwh_2019.df = read.csv('C:/Users/parkinso/Documents/iges_grid_ef_tco2_mwh_2019.csv',stringsAsFactors=FALSE)
# Fill country column and get most recent EF
iges_grid_ef_tco2_mwh_2019.df$EF = NA
for( rrr in 1:nrow(iges_grid_ef_tco2_mwh_2019.df) ){
	nm = iges_grid_ef_tco2_mwh_2019.df[rrr,1]
	if( nm == "" ){ iges_grid_ef_tco2_mwh_2019.df[rrr,1] = nm0 }else{ nm0 = nm }
	vals = as.numeric( iges_grid_ef_tco2_mwh_2019.df[ rrr,
		( which( names(iges_grid_ef_tco2_mwh_2019.df) == 'Method' ) + 1 ):( ncol( iges_grid_ef_tco2_mwh_2019.df ) - 1 ) ] )
	if( length( !is.na(vals) ) > 0 ){ iges_grid_ef_tco2_mwh_2019.df$EF[rrr] = vals[ !is.na(vals) ][1] }
	}
iges_grid_ef_tco2_mwh_2019.df = iges_grid_ef_tco2_mwh_2019.df %>% 
	dplyr::select( Country, EF ) %>%
	group_by( Country ) %>%
	summarise( EF = mean( EF, na.rm=TRUE ) ) %>%
	ungroup() %>% data.frame()
iges_grid_ef_tco2_mwh_2019.df$iso = countrycode(iges_grid_ef_tco2_mwh_2019.df$Country,'country.name','iso3c')
iges_grid_ef_tco2_mwh_2019.df = iges_grid_ef_tco2_mwh_2019.df %>% dplyr::select( iso, EF )
# incoporate project based data for missing countries and range
iges_cdmgrid_ef_tco2_mwh_2019.df = read.csv('C:/Users/parkinso/Documents/iges_cdmgrid_ef_tco2_mwh_2019.csv',stringsAsFactors=FALSE)
iges_cdmgrid_ef_tco2_mwh_2019.df$iso = countrycode(iges_cdmgrid_ef_tco2_mwh_2019.df$country,'country.name','iso3c')
iges_cdmgrid_ef_tco2_mwh_2019.df = iges_cdmgrid_ef_tco2_mwh_2019.df[ which( !is.na( iges_cdmgrid_ef_tco2_mwh_2019.df$iso ) ), ]
iges_cdmgrid_ef_tco2_mwh_2019.df = iges_cdmgrid_ef_tco2_mwh_2019.df %>% dplyr::select( iso, average_ef, max_ef, min_ef )
# combine 
iges_ef.df = full_join( iges_cdmgrid_ef_tco2_mwh_2019.df, iges_grid_ef_tco2_mwh_2019.df, by = 'iso' )
iges_ef.df$EF0 = sapply( 1:nrow( iges_ef.df ), function( iii ){ mean( c( iges_ef.df$EF[iii], iges_ef.df$average_ef[iii] ), na.rm=TRUE ) } )

# mereg with gw df
gw.df = left_join( gw.df, iges_ef.df, by = 'iso' )

# make a additional entry that will combine the iges cdm data with 
# CARBON FOOTPRINT: COUNTRY SPECIFIC ELECTRICITY GRID GREENHOUSE GAS EMISSION FACTORS
# Last Updated June 2019
alt_ef.df = read.csv('C:/Users/parkinso/Documents/Grid Electricity Emissions Factors v1p0 June 2019.csv',stringsAsFactors=FALSE)
alt_ef.df$iso = countrycode(alt_ef.df$country,'country.name','iso3c')
alt_ef.df = alt_ef.df %>% dplyr::select( iso, mtco2_mwh )

# mereg with gw df
gw.df = left_join( gw.df, alt_ef.df, by = 'iso' )

# create combined data for ef that covers all countries
gw.df$EF1 = sapply( 1:nrow( gw.df ), function( iii ){ mean( c( gw.df$EF0[iii], gw.df$mtco2_mwh[iii] ), na.rm=TRUE ) } )

# create aggregation regions for estimaating missing countries
iso_reg.df = left_join( 
	data.frame( iso = unique( gw.df %>% dplyr::select( iso ) %>% unlist() ) ),
	read.csv('C:/Users/parkinso/Documents/iso_reg.csv',stringsAsFactors=FALSE)[,c('iso','node')],
	by = 'iso' ) %>% left_join( ., gw.df %>% dplyr::select( iso, EF1 ), by = 'iso' ) %>%
	group_by( iso, node ) %>% summarise( EF1 = mean( EF1,na.rm=TRUE ) ) %>% ungroup() %>% data.frame() %>%
	filter( !is.na(iso) )
iso_reg.df$EF1[ which( is.na( iso_reg.df$EF1 ) ) ] = sapply( which( is.na( iso_reg.df$EF1 ) ), function( iii ){
	mean( iso_reg.df$EF1[ which( iso_reg.df$node == iso_reg.df$node[iii] ) ], na.rm=TRUE )
	} )
# use average for remaining
iso_reg.df$EF1[ which( is.na( iso_reg.df$EF1 ) ) ] = mean( iso_reg.df$EF1, na.rm= TRUE )
iso_reg.df$EF_final = iso_reg.df$EF1
iso_reg.df = iso_reg.df %>% dplyr::select( iso, EF_final )
	
# add to gw.df	
gw.df = left_join( gw.df %>% filter( !is.na(iso) ), iso_reg.df, by = 'iso' )
	
# cost of capital
# following similar approach as in 'Bias in energy system models with uniform cost of capital'
base_coc = 0.031 # value for Germany as base discount rate
crp.df = read.csv('C:/Users/parkinso/Documents/amodar_country_risk_premium_jan2020.csv',stringsAsFactors=FALSE)
crp.df$wacc = base_coc + crp.df$CRP # add base discount rate to risk premium dataset
crp.df$iso = countrycode(crp.df$Country,'country.name','iso3c') 

# merge into gw.df
gw.df = left_join( gw.df, crp.df[,c('iso','wacc')], by = 'iso' )

# Crop price from fao
crop_price.df = read.csv('C:/Users/parkinso/Documents/fao_crop_price.csv',stringsAsFactors=FALSE) 
crop_price.df = crop_price.df %>%
	filter( Unit == 'USD' ) %>%
	mutate( iso = countrycode(Area,'country.name','iso3c') ) %>%
	dplyr::select( iso, Item, names(crop_price.df)[grepl('Y',names(crop_price.df))] )
# map to crop names from GAEZ - has to match crop_names above and csv file with fao data
map2gaez =  list( 
	wheat = c('Wheat'),
	rice = c('Rice, paddy'),
	cotton = c('Cotton lint','Seed cotton'),
	fodder = c('Maize', 'Maize, green'), # assumed for simplicity
 	sugarcane = c('Sugar cane'),
	pulses = c('Pulses nes','Lentils'),
	maize = c('Maize', 'Maize, green'),
	fruit = c('Fruit, fresh nes', 'Fruit, citrus nes', 'Fruit, stone nes', 'Fruit, pome nes', 'Fruit, tropical fresh nes'),
	vegetables = c('Vegetables, fresh nes', 'Vegetables, leguminous nes')
	) 
map2gaez = bind_rows( lapply( names( map2gaez ), function( nnn ){
	df = data.frame( fao = unlist( map2gaez[[ nnn ]] ) )
	df$gaez = nnn
	return(df)
	} ) )
# merge in crop price  
map2gaez = left_join( map2gaez, crop_price.df %>% dplyr::rename(fao = Item), by = 'fao' )
# calculate max min and mean
map2gaez = bind_cols( map2gaez, bind_rows( lapply( 1:nrow(map2gaez), function( rrr ){
	x = unlist( map2gaez[ rrr, grepl( 'Y1|Y2', names( map2gaez ) ) ] )
	return( data.frame( 
		usd_tonne_max = max( x, na.rm=TRUE ), 
		usd_tonne_min = min( x, na.rm=TRUE ),  
		usd_tonne_avg = mean( x, na.rm=TRUE ) ) )
	} ) ) )	%>% dplyr::select( iso, gaez, usd_tonne_max, usd_tonne_min, usd_tonne_avg ) %>%
	pivot_longer( cols = starts_with('usd_tonne'), names_to = 'price_type', values_to = 'price' ) %>%
	mutate( price_type = paste( gaez, price_type, sep = '.' ) ) %>%
	dplyr::select( iso, price_type, price ) %>%
	group_by(iso, price_type) %>% summarise( price = mean( price, na.rm = TRUE ) ) %>%
	pivot_wider( names_from = 'price_type', values_from = 'price') %>%
	data.frame()

# merge prices into gw.df
gw.df = left_join( gw.df, map2gaez, by = 'iso' )	


# Irrigation withdrawals at half degree
irrigation.stack = stack( 'P:/is-wel/indus/message_indus/input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4' )
irrigation.stack = irrigation.stack[[(nlayers(irrigation.stack)-11):nlayers(irrigation.stack)]]
irrigation.df = data.frame( rasterToPoints( resample( sum( irrigation.stack  ), raster( spts_grid ), 'bilinear' ) ) ) %>%
		right_join( ., gw.df[,c('x','y')], by = c('x','y') ) %>%
		mutate( irrigation_mcm = layer ) %>% dplyr::select( x, y, irrigation_mcm )
# keep 2010 and resample to match gw.df, output to df, merge with gw.df
gw.df = left_join( gw.df, irrigation.df, by = c('x','y') )


# LCOE calculation
# parameter ranges must be defined such that the panel densities and crop yield degradations match

# wind and pv capex in USD2018/kw from IRENA Renewable energy costs 2018, Figure S.2
wind_capex = data.frame( min = 1174.7, max = 2438.7, avg = 1498.5 ) 
solar_capex = data.frame( min = 796.3, max = 2745.7, avg = 1210 )

# wind opex in USD2017/kw/yr - assuming 7% inflation to bring to 2018USD
# https://atb.nrel.gov/electricity/2019/index.html?t=inlwowsusdsrscgthpcgcccncb&m=1
wind_opex = data.frame( min = 10*1.07, max = 140*1.07, avg = 44.4*1.07 )
solar_opex = data.frame( min = 10*1.07, max = 20*1.07, avg = 14*1.07 )

# wind turbine deprecition in percent per year
# https://doi.org/10.1016/j.renene.2013.10.041
wind_depr = data.frame( min = 0.48, max = 1.8, avg = 1.6) 

# solar depreciation in percent per year
# https://doi.org/10.1002/pip.3189
solar_depr = data.frame( min = 0.2, max = 0.9, avg = 0.5) 

# wind turbine specific area is reciprocal of density  
# from Eurek et al. 2017 https://doi.org/10.1016/j.eneco.2016.11.015
# using the half density as the max
wind_speca = data.frame( min = 1/6, max = 1/2.5, avg = 1/5 ) # 

# solar PV specific area is reciprocal of density  
# Ong et al (2013) claims roughly 30 MW/km2 for PV plants
# https://www.nrel.gov/docs/fy13osti/56290.pdf
# using the range presented in Table 9
# half density is the max for crop yield, full density is the min
# min specific area corresponds to min for crop yield degradation
solar_speca = data.frame( min = 1/30, max = 1/10, avg = 1/15 ) # 

# assumed frequency of replacement for wind and solar in device per year span
wind_freq = data.frame( min = 1/30, max = 1/20, avg = 1/25 ) # 
solar_freq = data.frame( min = 1/30, max = 1/20, avg = 1/25 ) # 

# grid extension costs in USD per km 
# From J. Johnston, A. Mileva, J. Nelson, D. Kammen, SWITCH-WECC: Data, Assumptions,
# and Model Formulation, Tech. rep., Renewable and Appropriate Energy Laboratory,
# University of California Berkeley (2013).
# "An average high voltage transmission cost of $1130MW-1km-1($2013) is adopted by 
# default based on a range of values found in the Western Renewable Energy Zones (WREZ) 
# transmission model (Western Governor’s Association 2009a)
# for building new high voltage transmission lines in WECC. To calculate the total cost per MW of
# building transmission in SWITCH, the terrain cost multiplier of each new transmission path is
# normalized by the average terrain cost multiplier for existing transmission (1.50), multiplied by
# the per unit transmission cost ($1130 MW-1km-1)"
# using the same value 1130 USD/ MW/ km for the analysis here
# using 1.5 terrain multiplier to define range
# USAID 2016: Guides for Electric Cooperative Development and Rural Electrifi cation
# http://www.nrecainternational.coop/wp-content/uploads/2016/11/GuidesforDevelopment.pdf
# Table 5 summarizes costs for medium scale voltage transmission
#Cost ($ US per Kilometer) Capacity Limit (Range in KiloWatts)
# $8,961: 400 kW
# $9,140: 600-1,600 kW
# 10,766: 1,800-2,500 kW
# $15,072: 3,000-6,000 kW
# $24,314: 6,500-7,000 kW
# #ssuming most projects utility scale, multi-MW
# using the 3000 - 7000 kW range as avg cost per km per MW
# using the terrain mulutplier and lower high voltage bound from SWITCH for range
grid_extension = data.frame( min = 1130, max = 1.5*3690, avg = 3690 )

# from: Dupraz et al: 2011 
# Combining solar photovoltaic panels and food crops for optimising land use: 
# Towards new agrivoltaic schemes, https://doi:10.1016/j.renene.2011.03.005
# Table 2: relative durum whear yields matching the specific areas  - average is the mid-point
wheat_depr = data.frame( min = 0.73, max = 1, avg = 0.87 )
pulses_depr = wheat_depr # assumption due to lack of data

# from : Solar Sharing for Both Food and Clean Energy Production: Performance of 
# Agrivoltaic Systems for Corn, A Typical Shade-Intolerant Crop
# "Surprisingly, the corn yield of the low-density conﬁguration was larger not only 
# than that of the high-density conﬁguration, but also than that of the no-module 
# control conﬁguration (Tables 1 and 2)." -> use as minboundary 
# from : 
# " Reed et al. (1988) found [maize] grain yield to be reduced
# by 12% when shading (50% reduction of incoming radiation)
# was applied during the vegetative stage. When applied during
# flowering or grain filling, yields were reduced by 20% and
# 19%, respectively (Reed et al. 1988). -> using as avg and max boundary
maize_depr = data.frame( min = 0.8, max = 1, avg = 0.88 )
# Same as maize in this case based on assumption above for the input intensities
fodder_depr = maize_depr

#https://www.ise.fraunhofer.de/en/press-media/press-releases/2019/agrophotovoltaics-hight-harvesting-yield-in-hot-summer-of-2018.html
# Cotton up to 40% increase in yields under partial shading
cotton_depr = data.frame( min = 0.8, max = 1.4, avg = 1 )

# See: Agrivoltaics provide mutual benefits across the
# food–energy–water nexus in drylands
# Tomatoes increase yields under shading due to cooling benefits
fruit_depr = data.frame( min = 0.8, max = 1.4, avg = 1)

# https://www.ise.fraunhofer.de/en/press-media/press-releases/2019/agrophotovoltaics-hight-harvesting-yield-in-hot-summer-of-2018.html
# potatoes not really impacted by shading
# lettuce weight can also increase under shading
# See: Dinesh et al 2016: The Potential of Agrivoltaic Systems
# limited information on other fruits assuming averages for range
vegetables_depr = data.frame( min = 0.8, max = 1.2, avg = 1)

# See: https://doi.org/10.24778/jjser.37.6_23
# 20% shading reduces yield by 20% - assuming thats for 50% density
# From Elborg 2017
# "light distribution underneath the PV arrays is calculated. Total received sun light at 
# ground level varies greatly between the 
# two designs with 81% and 43% compared to unobstructed condition
# Using these ranges to bound the assumptions for rice
rice_depr = data.frame( min = 0.43, max = 0.81, avg = 0.62 )

# See:  SEASONAL GROWTH, SHADING POTENTIAL, AND YIELD OF SIX SUGARCANE
# CULTIVARS, shows shading level almost proportional to yield loss
# assuming same range as rice
sugarcane_depr = data.frame( min = 0.43, max = 0.81, avg = 0.62 )

# combine
crop_depr.df = cbind( data.frame( crop = crop_names, bind_rows( lapply( crop_names, function( ccc ){ get( paste0( ccc, '_depr' ) ) } ) ) ) )

# Cost calculation

# filter out locations too far from transmissin/demands and without irrigated area
gwr.df = gw.df %>% filter( 
	dist2trans_km < Inf, urban_dist_km < Inf, 
	!is.na(dist2trans_km), !is.na(urban_dist_km), 
	irrigated_area > 0, irrigation_mcm > 0 )
#get min dist to urban or transmission	
gwr.df$min_dist = sapply( 1:nrow( gwr.df ), function( iii ){
	min( c( gwr.df$dist2trans_km[iii], gwr.df$urban_dist_km[iii] ), na.rm=TRUE )
	} ) 
	
# clean NA from wacc vector
if( length( which( is.na( gwr.df$wacc ) ) ) > 0 )
	{
	gwr.df$wacc[ is.na( gwr.df$wacc ) ] = sapply( gwr.df$iso[ is.na( gwr.df$wacc ) ], function( ooo ){
		nds = read.csv('C:/Users/parkinso/Documents/iso_reg.csv',stringsAsFactors=FALSE)[,c('iso','node')] %>%
			mutate( node = ifelse( node %in% c('CAS'), 'SAS', node ) )
		nds = nds %>% filter( node == unlist( nds %>% filter( iso == ooo ) %>% dplyr::select( node ) ) ) %>% dplyr::select( iso ) %>% unlist()
		return( mean( unique( gwr.df %>% filter( iso %in% nds ) %>% dplyr::select( wacc ) %>% unlist() ), na.rm = TRUE ) ) 
		} )
	}
# clean NA from crop prices
for( hhh in  names( gwr.df[ , grepl( paste0('usd_tonne_'), names(gwr.df) ) ] ) ) 
	{ gwr.df[ which( is.na( gwr.df[ , hhh ] ) ), hhh ] = mean( c(gwr.df[ , hhh ]), na.rm=TRUE ) }
	

# max gw cons determined by irrigation level
gwr.df$gw_cons_mcm = sapply( 1:nrow( gwr.df ), function( iii ){ min( c( gwr.df$mcm[iii], gwr.df$irrigation_mcm[iii] ) ) } )
	
# Costs based on ranges above - rng2 is for cost uncertainty; rng is for performance uncertainty	
cost.df = bind_cols( lapply( c('avg','min','max'), function( rng2 ){
	bind_cols( lapply( c('avg','min','max'), function( rng ){
		# crop yield under current production type
		crop_cost.df = bind_rows( lapply( 1:nrow( gwr.df ), function( iii ){
			df = full_join( 
				data.frame( 
					crop = unlist( strsplit( names(unlist( gwr.df[ iii, grepl( 'rain_YIELD', names(gwr.df) ) ] )), '[.]' ) )[
						seq(1,length(unlist( strsplit( names(unlist( gwr.df[ iii, grepl( 'rain_YIELD', names(gwr.df) ) ] )), '[.]' ) )), by = 3)],
					rain = unlist( gwr.df[ iii, grepl( 'rain_YIELD', names(gwr.df) ) ] )
					),
				data.frame( 
					crop = unlist( strsplit( names(unlist( gwr.df[ iii, grepl( 'irr_YIELD', names(gwr.df) ) ] )), '[.]' ) )[
						seq(1,length(unlist( strsplit( names(unlist( gwr.df[ iii, grepl( 'irr_YIELD', names(gwr.df) ) ] )), '[.]' ) )), by = 3)],
					irr = unlist( gwr.df[ iii, grepl( 'irr_YIELD', names(gwr.df) ) ] )
					),	
				by = 'crop' ) %>%
				left_join( 
					data.frame( 
						crop = unlist( strsplit( names(unlist( gwr.df[ iii, grepl( paste0('usd_tonne_',rng2 ), names(gwr.df) ) ] )), '[.]' ) )[
							seq(1,length(unlist( strsplit( names(unlist( gwr.df[ iii, grepl( paste0('usd_tonne_',rng2 ), names(gwr.df) ) ] )), '[.]' ) )), by = 2)],
						price = unlist( gwr.df[ iii, grepl( paste0('usd_tonne_',rng2 ), names(gwr.df) ) ] )
						),
					by = 'crop'
					) %>% 
				left_join( ., crop_depr.df[ , c( 'crop', rng ) ] %>% rename( depr = rng ) , by = 'crop' ) %>%	
				mutate( 
					solar_rain_yield_ton = rain * gwr.df$irrigated_area[ iii ] * depr,
					solar_rain_cost_USD = rain * price * gwr.df$irrigated_area[ iii ] * depr, 
					solar_irr_yield_ton = irr * gwr.df$irrigated_area[ iii ] ,
					solar_irr_cost_USD = irr * price * gwr.df$irrigated_area[ iii ],
					wind_rain_yield_ton = rain * gwr.df$irrigated_area[ iii ],
					wind_rain_cost_USD = rain * price * gwr.df$irrigated_area[ iii ], 
					wind_irr_yield_ton = irr * gwr.df$irrigated_area[ iii ],
					wind_irr_cost_USD = irr * price * gwr.df$irrigated_area[ iii ]
					)
			solar_rain_ind = which.max( df$solar_rain_cost_USD )
			solar_irr_ind = which.max( df$solar_irr_cost_USD )
			wind_rain_ind = which.max( df$wind_rain_cost_USD )
			wind_irr_ind = which.max( df$wind_irr_cost_USD )
			# cost difference based on max value from rain and irrigated
			dscfs = c( sum( sapply( 1:round( 1/unlist( solar_freq[rng2] ) ), function( ttt ){ ( 1 + gwr.df$wacc[ iii ] )^(-1*ttt) } ) ) )
			dscfw = c( sum( sapply( 1:round( 1/unlist( wind_freq[rng2] ) ), function( ttt ){ ( 1 + gwr.df$wacc[ iii ] )^(-1*ttt) } ) ) )
			df2 = cbind( 
				df[ solar_rain_ind, c('crop','solar_rain_yield_ton','solar_rain_cost_USD') ] %>% dplyr::rename( solar_rain_crop = crop ), 	
				df[ solar_irr_ind, c('crop','solar_irr_yield_ton','solar_irr_cost_USD') ] %>% dplyr::rename( solar_irr_crop = crop ),
				df[ wind_rain_ind, c('crop','wind_rain_yield_ton','wind_rain_cost_USD') ] %>% dplyr::rename( wind_rain_crop = crop ), 	
				df[ wind_irr_ind, c('crop','wind_irr_yield_ton','wind_irr_cost_USD') ] %>% dplyr::rename( wind_irr_crop = crop )
				) %>% 
				mutate( solar_crop_cost = solar_irr_cost_USD - solar_rain_cost_USD  )  %>%
				mutate( solar_crop_LCOL = solar_crop_cost * round( 1/unlist( solar_freq[rng2] ) ) * dscfs / gwr.df$irrigated_area[ iii ] ) %>%
				mutate( wind_crop_cost = wind_irr_cost_USD - wind_rain_cost_USD ) %>%
				mutate( wind_crop_LCOL = wind_crop_cost * round( 1/unlist( wind_freq[rng2] ) ) * dscfw / gwr.df$irrigated_area[ iii ])
			return(df2)	
			} ) )
		# solar and wind yield - area is in hectares, densities in km2 - hence 0,01 conversion
		solar_capacity_MW = unlist( solar_speca[rng] )^(-1) * gwr.df$irrigated_area 
		solar_energy_MWh = solar_capacity_MW * gwr.df$solar_cf * 8760 
		# LCOE caclulation
		solar_LCOE = sapply( 1:nrow( gwr.df ), function( iii ){
			c( unlist( solar_capex[rng2] )*1e3 * solar_capacity_MW[ iii ] +	
			unlist( grid_extension[rng2] ) * gwr.df$min_dist[ iii ] * solar_capacity_MW[ iii ] + 	
			sum( sapply( 1:round( 1/unlist( solar_freq[rng2] ) ), function( ttt ){
				c( unlist( solar_opex[rng2] )*1e3 * solar_capacity_MW[ iii ] + 
				unlist( crop_cost.df$solar_crop_cost[ iii ] ) ) /
				( 1 + gwr.df$wacc[ iii ] )^ttt 
				} ) ) ) / 
			c( sum( sapply( 1:round( 1/unlist( solar_freq[rng2] ) ), function( ttt ){
				solar_energy_MWh[ iii ] * ( 1 - unlist( solar_depr[rng2] )/100 )^ttt /
				( 1 + gwr.df$wacc[ iii ] )^ttt 
				} ) ) )
			} )
		wind_capacity_MW = unlist( wind_speca[rng] )^(-1) * gwr.df$irrigated_area 
		wind_energy_MWh = wind_capacity_MW * gwr.df$wind_cf * 8760
		# LCOE caclulation
		wind_LCOE = sapply( 1:nrow( gwr.df ), function( iii ){
			c( unlist( wind_capex[rng2] )*1e3 * wind_capacity_MW[ iii ] +	
			unlist( grid_extension[rng2] ) * gwr.df$min_dist[ iii ] * wind_capacity_MW[ iii ] + 	
			sum( sapply( 1:round( 1/unlist( wind_freq[rng2] ) ), function( ttt ){
				c( unlist( wind_opex[rng2] )*1e3 * wind_capacity_MW[ iii ] +
				unlist( crop_cost.df$wind_crop_cost[ iii ] ) ) /
				( 1 + gwr.df$wacc[ iii ] )^ttt 
				} ) ) ) / 
			c( sum( sapply( 1:round( 1/unlist( wind_freq[rng2] ) ), function( ttt ){
				wind_energy_MWh[ iii ] * ( 1 - unlist( wind_depr[rng2] )/100 )^ttt /
				( 1 + gwr.df$wacc[ iii ] )^ttt 
				} ) ) )
			} )	
		# total energy generation accounting for depreciation
		solar_energy_MWh_yr = solar_energy_MWh * 
			sum( sapply( 1:round( 1/unlist( solar_freq[rng2] ) ), function (ttt){( 1 - unlist( solar_depr[rng] )/100 )^( ttt )} ) ) * 
			unlist( solar_freq[rng2] )
		wind_energy_MWh_yr = wind_energy_MWh * 
			sum( sapply( 1:round( 1/unlist( wind_freq[rng2] ) ), function (ttt){( 1 - unlist( wind_depr[rng] )/100 )^( ttt )} ) ) *
			unlist( wind_freq[rng2] )
		# Emissions abatement
		solar_tCO2_yr = solar_energy_MWh_yr * gwr.df$EF_final 
		wind_tCO2_yr = wind_energy_MWh_yr * gwr.df$EF_final
		# output dataframe with all data for plotting
		df = cbind( data.frame( 
			solar_capacity_MW = solar_capacity_MW, 
			solar_energy_MWh_yr = solar_energy_MWh_yr, 
			solar_LCOE = solar_LCOE, 
			solar_tCO2_yr = solar_tCO2_yr, 
			wind_capacity_MW = wind_capacity_MW, 
			wind_energy_MWh_yr = wind_energy_MWh_yr, 
			wind_LCOE = wind_LCOE, 
			wind_tCO2_yr = wind_tCO2_yr ), crop_cost.df )
		names( df ) = paste( names( df ), paste( rng, rng2, sep = '.' ), sep= '.' )
		return( df )	
		} ) ) 
	} ) )
	
# merge into gw.df
gwr.df = cbind( gwr.df, cost.df )	

# filter out entries without land value because no yields from GAEZ
gwr.df = gwr.df %>% filter( solar_crop_LCOL.avg.avg > 1e-6 )

# create list for plotting supply curve representation
sc_chk = c( 'avg.avg', 'min.avg', 'max.avg', 'avg.min', 'avg.max', 'min.min', 'max.max')
sc_nm = c('average','min cost','max cost','min perfom','max perform','best','worst')
sc_color = c('black', 'blue', 'blue', 'green', 'green', 'red', 'red')
solar_supply_curve.list = lapply( sc_chk, function( sc ){
	gwr.df %>% 
		dplyr::select( 
			paste0('solar_LCOE.',sc), 
			paste0('solar_energy_MWh_yr.',sc),
			gw_cons_mcm,
			paste0('solar_tCO2_yr.',sc)) %>%
		'names<-'(c('LCOE', 'MWh', 'MCM', 'tCO2')) %>%
		arrange(LCOE) %>%
		mutate( PWh = cumsum( MWh )/1e9 ) 
	} )	
names( 	solar_supply_curve.list ) = sc_chk
wind_supply_curve.list = lapply( sc_chk, function( sc ){
	gwr.df %>% 
		dplyr::select( 
			paste0('wind_LCOE.',sc), 
			paste0('wind_energy_MWh_yr.',sc),
			gw_cons_mcm,
			paste0('wind_tCO2_yr.',sc)) %>%
		'names<-'(c('LCOE', 'MWh', 'MCM', 'tCO2')) %>%
		arrange(LCOE) %>%
		mutate( PWh = cumsum( MWh )/1e9 ) 
	} )	
names( 	wind_supply_curve.list ) = sc_chk

solar_gw_supply_curve.list = lapply( sc_chk, function( sc ){
	gwr.df %>% 
		dplyr::select( 
			paste0('solar_LCOE.',sc), 
			paste0('EF_final'),
			gw_cons_mcm,
			paste0('solar_tCO2_yr.',sc)) %>%
		'names<-'(c('LCOE', 'EF', 'MCM', 'tCO2')) %>%
		mutate( USDtCO2 = LCOE / EF ) %>%
		arrange(USDtCO2) %>%
		mutate( km3 = cumsum( MCM )/1e3 )  
	} )	
names( 	solar_gw_supply_curve.list ) = sc_chk
wind_gw_supply_curve.list = lapply( sc_chk, function( sc ){
	gwr.df %>% 
		dplyr::select( 
			paste0('wind_LCOE.',sc), 
			paste0('EF_final'),
			gw_cons_mcm,
			paste0('wind_tCO2_yr.',sc)) %>%
		'names<-'(c('LCOE', 'EF', 'MCM', 'tCO2')) %>%
		mutate( USDtCO2 = LCOE / EF ) %>%
		arrange(USDtCO2) %>%
		mutate( km3 = cumsum( MCM )/1e3 )  
	} )	
names( 	wind_gw_supply_curve.list ) = sc_chk


# Plots
# 1) Supply curve for solar and wind energy showing potential vs. LCOE
PWh_maty = as.matrix( 
	bind_cols( lapply( 1:length( sc_chk ), function( sc ){
		df = bind_cols( 
			data.frame( solar_supply_curve.list[[ sc_chk[ sc ] ]]$PWh ), 
			data.frame( wind_supply_curve.list[[ sc_chk[ sc ] ]]$PWh )
			) 
		names(df) = c( paste0( 'Solar: ', sc_nm[ sc ] ), paste0( 'Wind: ', sc_nm[ sc ] ) )	
		return(df)
		} ) )
	)
PWh_matx = as.matrix( 
	bind_cols( lapply( 1:length( sc_chk ), function( sc ){
		df = bind_cols( 
			data.frame( solar_supply_curve.list[[ sc_chk[ sc ] ]]$LCOE ), 
			data.frame( wind_supply_curve.list[[ sc_chk[ sc ] ]]$LCOE )
			) 
		names(df) = c( paste0( 'Solar: ', sc_nm[ sc ] ), paste0( 'Wind: ', sc_nm[ sc ] ) )	
		return(df)
		} ) )
	)	

# 2) mitigation cost vesus groundwater mitigation potentials
km3_maty = as.matrix( 
	bind_cols( lapply(1:length( sc_chk ), function( sc ){
		df = bind_cols( 
			data.frame( solar_gw_supply_curve.list[[ sc_chk[ sc ] ]]$km3 ), 
			data.frame( wind_gw_supply_curve.list[[ sc_chk[ sc ] ]]$km3 )
			)
		names(df) = c( paste0( 'Solar: ', sc_nm[ sc ] ), paste0( 'Wind: ', sc_nm[ sc ] ) )	
		return(df)		
		} ) )
	)
km3_matx = as.matrix( 
	bind_cols( lapply( 1:length( sc_chk ), function( sc ){
		df = bind_cols( 
			data.frame( solar_gw_supply_curve.list[[ sc_chk[ sc ] ]]$USDtCO2 ), 
			data.frame( wind_gw_supply_curve.list[[ sc_chk[ sc ] ]]$USDtCO2 )
			)
		names(df) = c( paste0( 'Solar: ', sc_nm[ sc ] ), paste0( 'Wind: ', sc_nm[ sc ] ) )	
		return(df)		
		} ) )
	)	
	
pdf( 'gw_re_res_fig1.pdf', width=11, height=4.5 )
p1 = layout( matrix( c(1,2,3),1,3, byrow=TRUE ), widths=c(0.3,0.3,0.1), heights=c(0.9) )
cols = c( 'red', 'navy',  rep( c('darkorange1','dodgerblue2'), 6 ) )
ltys = c(1,1,2,2,2,2,4,4,4,4,1,1,1,1)
lwds = c(2,2,c(rep(1,12)) )
par(mar=c(4,4,0.5,3),oma=c(2,2,2,2))
matplot( PWh_matx, PWh_maty,
	type = 'l', 
	xlim = c(0,250),
	ylim = c(0,40),
	xlab = 'Levelized Cost of Energy [ USD / MWh ]', 
	ylab = 'Renewable Energy Potential [ PWh / yr ]', 
	lty = ltys,
	col = cols,	
	lwd = lwds,
	xaxs = 'i' ,
	yaxs = 'i',
	bty = 'n'
	)
text( 10, 37, 'a', cex = 1.3, font = 2 )	
matplot( 	
	km3_matx, km3_maty, type = 'l', 
	xlim = c(0,600),
	ylim = c(0, 160),
	xlab = 'CO2 Mitigation Cost [ USD / tCO2 ]', 
	ylab = 'Nonrenewable Groundwater Extraction [ km3 / yr ]', 
	lty = ltys, 
	col = cols,
	lwd = lwds,
	xaxs = 'i' ,
	yaxs = 'i',
	bty = 'n'
	)
text( 20, 150, 'b', cex = 1.3, font = 2 )	
par(mar=c(4,0,0.5,0))
plot.new()	
legend( 
	'center',
	legend = c(
		'Solar: Average',
		'Wind: Average',
		'Solar: Min/Max Cost',
		'Wind: Min/Max Cost', 
		'Solar: Min/Max Perf',
		'Wind: Min/Max Perf',
		'Solar: Worst/Best',
		'Wind: Worst/Best'	), 
	col = c( 'red', 'navy',  rep( c('darkorange1','dodgerblue2'), 3 ) ), 
	lty = c(1,1,2,2,4,4,1,1),
	lwd = c(2,2,1,1,1,1,1,1), bty = 'n', ncol =1, cex = 1, y.intersp = 1.7 ) 		
dev.off()

# barplots showing the max potential for solar / wind as well as change in production of crops
cwr.df = gwr.df %>% 
	dplyr::select( iso, gw_cons_mcm, EF_final, names(gwr.df)[grepl( 'yield_ton|energy_MWh_yr', names(gwr.df) )] ) %>% 
	group_by( iso, EF_final ) %>% 
	summarise_all( sum ) %>% 	
	ungroup() %>% data.frame() %>%
	arrange(gw_cons_mcm)
	
tbl1 = data.frame( do.call( cbind, lapply( c('solar_energy', 'wind_energy' ), function( ttt ){
	do.call( rbind, lapply( unique( cwr.df$iso ), function( ccc ){
		df = cwr.df[, grepl( paste0('iso|',ttt), names(cwr.df) )] %>% 
			filter( iso %in% ccc ) 
		return( paste0( 
				round( df[,grepl( 'avg.avg', names(df) )]/1e6 ), ' ', 
				'(', round( min( unlist( df[,2:ncol(df)] ) )/1e6 ), ',', 
				round( max( unlist( df[,2:ncol(df)] ) )/1e6 ), ')' 
				) )
		} ) )
	} ) ) )
	
tbl1$iso = unique( cwr.df$iso )
tbl1$ef = round(cwr.df$EF_final, digits = 2 )
tbl1$bcm = round(cwr.df$gw_cons_mcm/1e3, digits = 2 )
tbl1 = tbl1[,c(3,5,4,1,2)] %>% arrange(desc(bcm)) 
names( tbl1 ) = c( 'Country', 'km3/yr', 'MtCO2/TWh', 'Solar TWh/yr', 'Wind TWh/yr ')
#tbl1$Country = countrycode(as.character(tbl1$Country),'iso3c','country.name')
	
# impact of switch to RE on crop yields
tbl2 = do.call( rbind, lapply( tbl1$Country, function( ccc ){
	do.call( rbind, lapply( crop_names, function( ttt ){	
		df = gwr.df %>%
			filter( iso == ccc ) %>%
			dplyr::select( names(gwr.df)[grepl( paste0(ttt,'|rain_crop|irr_crop|rain_yield|irr_yield|irrigated_area'), names( gwr.df ) )] )
		# lost yield potential from no irrigation
		# df_irr = df %>% 
			# dplyr::select( names( df )[ grepl( 'irr_crop', names( df ) ) ] )
		# df_rain = df %>% 
			# dplyr::select( names( df )[ grepl( 'rain_crop', names( df ) ) ] )
		res1 = bind_rows( lapply( c( 'avg', 'min', 'max'), function( rng1 ){ 
			bind_rows( lapply( c( 'avg', 'min', 'max'), function( rng2 ){
				bind_rows( lapply( c('solar', 'wind' ), function(rng3){
					x =  df %>% 
						dplyr::select( names( df )[ grepl( paste0( paste0( paste0(rng3,'_irr_yield_ton.'),rng1,'.',rng2 , '|',paste0(rng3,'_irr_crop.'),rng1,'.',rng2,sep='')), names( df ) ) ] )
					x = x[ which( x[,1] == ttt ), ]	
					return( data.frame(
						Country = ccc,
						crop = ttt,
						rng1 = rng1,
						rng2 = rng2,
						rng3 = rng3,
						value = round( max( 0, sum( 1e-3 * x[,2], na.rm =TRUE ) ), digits = 2 ) ) )
					} ) )
				} ) )
			} ) )
		res2 = bind_rows( lapply( c( 'avg', 'min', 'max'), function( rng1 ){ 
			bind_rows( lapply( c( 'avg', 'min', 'max'), function( rng2 ){
				bind_rows( lapply( c('solar', 'wind' ), function(rng3){
					x =  df %>% 
						dplyr::select( names( df )[ grepl( paste0( paste0( paste0(rng3,'_rain_yield_ton.'),rng1,'.',rng2 , '|',paste0(rng3,'_rain_crop.'),rng1,'.',rng2,sep='')), names( df ) ) ] )
					x = x[ which( x[,1] == ttt ), ]	
					return( data.frame(
						Country = ccc,
						crop = ttt,
						rng1 = rng1,
						rng2 = rng2,
						rng3 = rng3,
						value = round( max( 0, sum( 1e-3 * x[,2], na.rm =TRUE ) ), digits = 2 ) ) )
					} ) )
				} ) )
			} ) )	
		res = full_join( res1,res2,by = c( 'Country', 'crop', 'rng1', 'rng2',  'rng3' ) )
		
		
		res$value = res$value.y - res$value.x
		res = c( 
			mean( res %>% filter( rng1 == 'avg', rng2 == 'avg' ) %>% dplyr::select( value ) %>% unlist() ),
			min( res %>% filter( rng1 == 'min', rng2 == 'min' ) %>% dplyr::select( value ) %>% unlist() ),
			max( res %>% filter( rng1 == 'max', rng2 == 'max' ) %>% dplyr::select( value ) %>% unlist() )
			)
		res = paste0( 
			as.character( round( res[1], digits = 2 ) ),
			' (', 
			as.character( round( res[2], digits = 2 ) ), 
			',',  
			as.character( round( res[3], digits = 2 ) ), 
			')' )
		res = data.frame( Country = ccc, crop = ttt, change = res )		
		return( res )
		} ) ) 
	} ) )%>%
	pivot_wider( names_from = 'crop', values_from = 'change') %>%
	ungroup() %>% data.frame()
tbl = left_join( tbl1, tbl2, by = 'Country' ) %>%
	mutate( Country = countrycode(Country,'iso3c','country.name'))
write.csv( tbl, 'country_res.csv')	
	
	
	