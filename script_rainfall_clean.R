source("./rasterCrop.R")
source("./ap.R")
source("./msi.R")

library(raster)
library(rgdal)
library(ncdf4)
library(tidyverse)

# Read shape file of the PNCV borders
UCs <- readOGR(dsn="./UC_fed_julho_2019", layer="UC_fed_julho_2019")

chapada <- UCs[302,][2]

# Read the netcdf file with Chirps information
nc.chirps <- nc_open("./chirps-v2.0.monthly_TSA.nc", readunlim = FALSE) # opening chirps netcdf dataset for reading

# Extract precipitation values
prec.chirps <- ncvar_get(nc.chirps, "precip") # getting prec values

# Read shape files of biomes borders
cerrado_border <- rgdal::readOGR(dsn="./", layer = "cerrado_border")
amazon_border <- rgdal::readOGR(dsn="./", layer = "amazon_biome_border")
caatinga_border <- rgdal::readOGR(dsn="./", layer = "biome_border")

# Calculate indices to all points
apChirpsA   <- ap(prec.chirps)  # annual precipitation
msiChirpsA  <- msi(prec.chirps) # annual msi
#mcwdChirpsA <- mwd(prec.chirps) # annual mcwd
#sdpChirpsA <- apply(prec.chirps, 1:2, sd) # inter annual standard deviation in ap


# MAP ========
# Calculate the MAP
mapChirpsA <- apply(apChirpsA, 1:2, mean) # mean annual precipitation

# calculate CV
#cvpChirpsA <- sdpChirpsA/mapChirpsA #

# Transform in raster (for all SA)
aux1 <- apply(mapChirpsA, 1, rev)
mapChirpsAR <- raster(aux1, xmn = -90, xmx = -30, ymn = -20, ymx = 15)

# Crop the rasters with the border of each Biome
CerrMAP <- rasterCrop(mapChirpsA, cerrado_border) 
AmazMAP <- rasterCrop(mapChirpsA, amazon_border)
CaaMAP <- rasterCrop(mapChirpsA, caatinga_border)

# Extract the MAP of each pixel
pCerrado <- rasterToPoints(CerrMAP)
pAmazon <- rasterToPoints(AmazMAP)
pCaatinga <- rasterToPoints(CaaMAP)

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

pCaatinga <- pCaatinga %>%
  as.data.frame %>% 
  dplyr::select(3)%>% 
  add_column(Biome = rep("Caatinga", pCaatinga[,1] %>% length())) %>% 
  rename(Rainfall = "layer")
## Now for our sample plots

# Read coordinates of our samples 
coords <- read.table(file = "./coordenadas",h=T)

# Get lat and lon and set projection
acoords <- SpatialPoints(cbind(coords[,2], coords[,3]), proj4string = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Extract values from the coordinates
pPlots <- raster::extract(mapChirpsAR, acoords) %>% 
  as.data.frame %>% # transform in data.frame
  add_column(Biome = c(rep("Savanna", 10), rep("Gallery Forest", 10), rep("Dry Forest", 10))) %>% # Add column with vegetation types names
  rename(Rainfall = ".") # Rename the column rainfall

# Bind all data (Amazon, Cerrado and vegetation types)
data <- rbind(pAmazon, pCerrado, pCaatinga, pPlots)


# MSI ========
# Calculate the MSI
msiChirpsA <- apply(msiChirpsA, 1:2, mean) # mean annual precipitation

# Transform in raster (for all SA)
aux1 <- apply(msiChirpsA, 1, rev)
msiChirpsAR <- raster(aux1, xmn = -90, xmx = -30, ymn = -20, ymx = 15)

# Crop the rasters with the border of each Biome
CerrMSI <- rasterCrop(msiChirpsA, cerrado_border) 
AmazMSI <- rasterCrop(msiChirpsA, amazon_border) 
CaaMSI <- rasterCrop(msiChirpsA, caatinga_border) 

# Extract the MAP of each pixel
msiCerrado <- rasterToPoints(CerrMSI)
msiAmazon <- rasterToPoints(AmazMSI)
msiCaatinga <- rasterToPoints(CaaMSI)

# Select a column with rainfall and add a column with the biome name
msiAmazon <- msiAmazon %>%
  as.data.frame %>% 
  dplyr::select(3)%>% 
  add_column(Biome = rep("Amazon", msiAmazon[,1] %>% length())) %>% 
  rename(Rainfall = "layer")

msiCerrado <- msiCerrado %>%
  as.data.frame %>% 
  dplyr::select(3)%>% 
  add_column(Biome = rep("Cerrado", msiCerrado[,3] %>% length())) %>% 
  rename(Rainfall = "layer")

msiCaatinga <- msiCaatinga %>%
  as.data.frame %>% 
  dplyr::select(3)%>% 
  add_column(Biome = rep("Caatinga", msiCaatinga[,1] %>% length())) %>% 
  rename(Rainfall = "layer")
## Now for our sample plots

# Extract values from the coordinates
msiPlots <- raster::extract(msiChirpsAR, acoords) %>% 
  as.data.frame %>% # transform in data.frame
  add_column(Biome = c(rep("Savanna", 10), rep("Gallery Forest", 10), rep("Dry Forest", 10))) %>% # Add column with vegetation types names
  rename(Rainfall = ".") # Rename the column rainfall

# Bind all data (Amazon, Cerrado and vegetation types)
data1 <- rbind(msiAmazon, msiCerrado, msiCaatinga, msiPlots)


# CV ========
# Calculate the CV
# cvpChirpsA <- apply(cvpChirpsA, 1:2, mean) # mean annual precipitation
# 
# # Transform in raster (for all SA)
# aux1 <- apply(cvpChirpsA, 1, rev)
# cvpChirpsAR <- raster(aux1, xmn = -90, xmx = -30, ymn = -20, ymx = 15)
# 
# # Crop the rasters with the border of each Biome
# CerrCV <- rasterCrop(cvpChirpsA, cerrado_border) 
# AmazCV <- rasterCrop(cvpChirpsA, amazon_border) 
# 
# # Extract the MAP of each pixel
# cvCerrado <- rasterToPoints(CerrCV)
# cvAmazon <- rasterToPoints(AmazCV)
# 
# # Select a column with rainfall and add a column with the biome name
# cvAmazon <- cvAmazon %>%
#   as.data.frame %>% 
#   dplyr::select(3)%>% 
#   add_column(Biome = rep("Amazon", cvAmazon[,1] %>% length())) %>% 
#   rename(Rainfall = "layer")
# 
# cvCerrado <- cvCerrado %>%
#   as.data.frame %>% 
#   dplyr::select(3)%>% 
#   add_column(Biome = rep("Cerrado", cvCerrado[,3] %>% length())) %>% 
#   rename(Rainfall = "layer")
# 
# ## Now for our sample plots
# 
# # Extract values from the coordinates
# cvPlots <- raster::extract(cvpChirpsAR, acoords) %>% 
#   as.data.frame %>% # transform in data.frame
#   add_column(Biome = c(rep("Savanna", 10), rep("Gallery Forest", 10), rep("Dry Forest", 10))) %>% # Add column with vegetation types names
#   rename(Rainfall = ".") # Rename the column rainfall

# Bind all data (Amazon, Cerrado and vegetation types)
# data2 <- rbind(cvAmazon, cvCerrado, cvPlots)

# MAP <- data %>% ggplot(aes(x = factor(Biome), y = Rainfall))+
#   geom_boxplot(data = data, outlier.shape = NA, show.legend = F, aes(fill = factor(Biome)))+
#   scale_fill_manual(values = c("grey", "lightblue", "pink", "purple", "blue", "orange"))+
#   ylim(c(0, 4000))+
#   geom_vline(xintercept = 3.5, linetype = "dashed", size = 0.5, color = "grey")+
#   geom_hline(yintercept = c(1000,2500), linetype = "dashed", color = "red")+
#   labs(x = NULL, y = "Mean Annual Rainfall (mm)", title = NULL)+
#   geom_text(aes(x = 4.5, y = 2300, label = "Bistability zone", color = "red", size = 11, ... = "sans"), show.legend = F)+
#   geom_text(aes(x = 2.5, y = 3500, label = "Biomes", size = 11, ... = "sans"), show.legend = F)+
#   geom_text(aes(x = 5, y = 3500, label = "Our samples", size = 11, ... = "sans"), show.legend = F)+
#   geom_text(aes(x = 1, y = 3470, label = "a"))+
#   geom_text(aes(x = 2, y = 1130, label = "b"))+
#   geom_text(aes(x = 3, y = 2130, label = "c"))+
#   geom_text(aes(x = 4, y = 1700, label = "d"))+
#   geom_text(aes(x = 5, y = 1700, label = "e"))+
#   geom_text(aes(x = 6, y = 1700, label = "e"))+
#   theme(axis.text = element_text(size = 11),
#         axis.title = element_text(size = 13),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank())
# 
# MSI <- data1 %>% ggplot(aes(x = factor(Biome), y = Rainfall))+
#   geom_boxplot(data = data1, outlier.shape = NA, show.legend = F, aes(fill = factor(Biome)))+
#   scale_fill_manual(values = c("grey", "lightblue", "pink", "purple", "blue", "orange"))+
#   ylim(c(0, 0.9))+
#   geom_vline(xintercept = 3.5, linetype = "dashed", size = 0.5, color = "grey")+
#   labs(x = NULL, y = "Markham Seasonality Index (MSI)", title = NULL)+
# #  geom_text(aes(x = 1.5, y = 0.8, label = "Biomes", size = 11, ... = "sans"), show.legend = F)+
# #  geom_text(aes(x = 4, y = 0.8, label = "Our samples", size = 11, ... = "sans"), show.legend = F)+
#   geom_text(aes(x = 1, y = 0.7, label = "a"))+
#   geom_text(aes(x = 2, y = 0.83, label = "b"))+
#   geom_text(aes(x = 3, y = 0.73, label = "c"))+
#   geom_text(aes(x = 4, y = 0.7, label = "d"))+
#   geom_text(aes(x = 5, y = 0.7, label = "d"))+
#   geom_text(aes(x = 6, y = 0.7, label = "d"))+
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 13))
# png("/home/marcio/Documentos/Doutorado/precipitação/Fig_S2ab.png", res = 300, width = 1850, height = 2000)
# MAP/MSI
# dev.off()
