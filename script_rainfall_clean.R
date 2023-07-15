source("~/scripts/rasterCrop.R")
source("/home/marcio/scripts/ap.R")
source("/home/marcio/scripts/msi.R")
source("/home/marcio/scripts/ap2D.R")

library(raster)
library(rgdal)
library(ncdf4)
library(tidyverse)

# Read shape file of the PNCV borders
UCs <- readOGR(dsn="/home/marcio/datasets/UC_fed_julho_2019", layer="UC_fed_julho_2019")

chapada <- UCs[302,][2]

# Read the netcdf file with Chirps information
nc.chirps <- nc_open("/home/marcio/datasets/chirps-v2.0.monthly_TSA.nc", readunlim = FALSE) # opening chirps netcdf dataset for reading

# Extract precipitation values
prec.chirps <- ncvar_get(nc.chirps, "precip") # getting prec values

# Read shape files of biomes borders
cerrado_border <- rgdal::readOGR(dsn="/home/marcio/", layer = "cerrado_border")
amazon_border <- rgdal::readOGR(dsn="/home/marcio/", layer = "brazilian_legal_amazon")

# Calculate indices to all points
apChirpsA   <- ap(prec.chirps)  # annual precipitation
#msiChirpsA  <- msi(prec.chirps) # annual msi
#mcwdChirpsA <- mwd(prec.chirps) # annual mcwd

# MAP ========
# Calculate the MAP
mapChirpsA <- apply(apChirpsA, 1:2, mean) # mean annual precipitation

# Transform in raster (for all SA)
aux1 <- apply(mapChirpsA, 1, rev)
mapChirpsAR <- raster(aux1, xmn = -90, xmx = -30, ymn = -20, ymx = 15)

# Crop the rasters with the border of each Biome
CerrMAP <- rasterCrop(mapChirpsA, cerrado_border) 
AmazMAP <- rasterCrop(mapChirpsA, amazon_border) 

# Extract the MAP of each pixel
pCerrado <- rasterToPoints(CerrMAP)
pAmazon <- rasterToPoints(AmazMAP)

# Select a column with rainfall and add a column with the biome name
pAmazon <- pAmazon %>%
  as.data.frame %>% 
  dplyr::select(3)%>% 
  add_column(Biome = rep("Amazon", pAmazon[,1] %>% length())) %>% 
  rename(Rainfall = "layer")

pCerrado <- pCerrado %>%
  as.data.frame %>% 
  dplyr::select(3)%>% 
  add_column(Biome = rep("Cerrado", pCerrado[,3] %>% length())) %>% 
  rename(Rainfall = "layer")

## Now for our sample plots

# Read coordinates of our samples 
coords <- read.table(file = "/home/marcio/datasets/coordenadas",h=T)

# Get lat and lon and set projection
acoords <- SpatialPoints(cbind(coords[,2], coords[,3]), proj4string = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Extract values from the coordinates
pPlots <- raster::extract(mapChirpsAR, acoords) %>% 
  as.data.frame %>% # transform in data.frame
  add_column(Biome = c(rep("Savanna", 10), rep("Gallery Forest", 10), rep("Dry Forest", 10))) %>% # Add column with vegetation types names
  rename(Rainfall = ".") # Rename the column rainfall

# Bind all data (Amazon, Cerrado and vegetation types)
data <- rbind(pAmazon, pCerrado, pPlots)
