

get.geog = function(odbc.name, pcodes, geog = c("lsoa", "msoa", "la", "ccg", "nuts1", "nuts3", "hpt", "phec")){
  
  library(RODBC)
  library(MRAtools)
  library(sp)
  library(rgdal)
  
  # check arguments
  
  if(!all(geog %in% c("lsoa", "msoa", "la", "ccg", "nuts1", "nuts3", "hpt", "phec"))) stop("geog argument must be a combination of the following: lsoa, msoa, la, ccg, nuts1, nuts3, hpt, phec")
  
  # clean postcodes
  
  pcodes[is.na(pcodes)] = "Z"
  
  pcodes = pcClean(pcodes)
  
  channel <- odbcConnect(odbc.name)
  
  # Get the XY of the postcode
  
  osgb <-  postcodeToXY(channel, pcodes)
  
  # NB: if you have more than one postcode to convert you can supply a character vector rather than a singleton here
  
  # if postcode is recorded as NA make dummy coordiantes as 0 
  osgb$postcode = as.character(osgb$postcode)
  
  osgb[is.na(osgb$postcode), c("x", "y")] = 0
  
  # Turn this into a SpatialPointsDataFrame:
  osgbSpatial <- SpatialPointsDataFrame(osgb[,c("x","y")], osgb, proj4string = CRS("+init=epsg:27700"))
  # Note that we supply a CRS to the constructor (in this case we simply tell R to look up the epsg 
  # Database for projection 27700 (the OS national grid))
  
  # Now use spTransform  from rgdal to convert to wgs84:
  Transformed <- spTransform(osgbSpatial, CRS("+init=epsg:4326"))
  
  # You can now get the lat long out using coordinates (or save as a shape file or whatever):
  # For coordinates:
  # Transformed = as.data.frame(Transformed, coordinates(Transformed))
  Transformed = as.data.frame(Transformed)
  
  # assign column names
  colnames(Transformed) = c("postcode", "easting", "northing", "longitude", "latitude")
  
  # place columns in order
  Transformed = Transformed[, c("postcode", "easting", "northing",  "latitude", "longitude")]
  
  # remove dummy coordinates if postcode is missing
  Transformed[is.na(Transformed$postcode), c("easting", "northing",  "latitude", "longitude")] = NA
  
  # define data frame of table look ups
  
  lsoa = c("lsoa", "ONSStats11_LSOAs_Jan13_EnWa", "[LSOACD]", "[LSOANM]")
  
  msoa = c("msoa", "ONSStats11_MSOAs_Jan13_EnWa", "[MSOACD]", "[MSOANM]")
  
  la = c("la", "ONSADMIN_LADS_2015_GB", "[GSS_CD]", "[GSS_NM]")
  
  ccg = c("ccg", "ONShealth_CCGs_Jul15_En", "[CCGCD]", "[CCGNM]")
  
  nuts1 = c("nuts1", "ONSother_NUTS1_Jan15_EnWa", "[GSS_CD]", "[GSS_NM]")
  
  nuts3 = c("nuts3", "ONSother_NUTS3_Jan15_EnWa", "[GSS_CD]", "[GSS_NM]")
  
  hpt = c("hpt", "PHEhp_HPTs_Apr16_En", "[HPTCD]", "[HPTNM]")
  
  phec = c("phec", "PHEcorp_Centres_Jul15_En", "[PHECCDGSS]", "[PHECNM]")
  
  tables = as.data.frame(rbind(lsoa, msoa, la, ccg, nuts1, nuts3, hpt, phec))
  
  colnames(tables) = c("geog", "table.name", "code", "name")
  
  tables = tables[tables$geog %in% geog, ]
  
  # loop over tables to extract out required geographies 
  
  x = Transformed
  
  for(i in 1:nrow(tables)){
    
    # define table name 
    table.name = as.character(tables[i, "table.name"])
    
    # define database table name
    db.table.name = paste0("[PORGISDB01].[GISADMIN].[", table.name, "]")
    
    # get geography from table
    
    y = genericPIP(channel, postcodes = pcClean(pcodes),
                   dbTable = db.table.name,
                   dbColumns = c(as.character(tables[i, "code"]), 
                                 as.character(tables[i, "name"])))  
    
    # add geog to column names 
    
    colnames(y) = paste(tables$geog[i], colnames(y), sep = "_")
    
    # drop postcode so as not to have multiple postcode columns
    
    y[, 1] = NULL
    
    x = cbind(x, y)
  }
  
  # Break the connection to the database
  
  odbcClose(channel)
  
  # return output
  x
}


read.excel <- function(header=TRUE, ...) {
  read.table("clipboard", sep = "\t", header = header, ...)
}



