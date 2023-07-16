# 18 feb 2019
# function to rasterize an array file and crop given a shapefile

rasterCrop <- function(mtx, shape) {
  
  # rasterize for tropical south america
  aux <- apply(mtx, 1, rev)
  mtxR <- raster(aux, xmn = -90, xmx = -30, ymn = -20, ymx = 15)
  #mtxR <- raster(aux, xmn = -47.9072, xmx = -46.97436, ymn = -14.21634, ymx = -13.61845)
  crs(mtxR) <- crs(shape)
  rm(aux)
  
  # crop for the given shape
  cropped <- crop(mtxR, shape, snap = 'out')
  
  cropped2 <- setValues(cropped, NA)
  shpAux <- rasterize(shape, cropped2)
  rasterCrop <- mask(x = cropped, mask = shpAux)
  #rasterto
  rasterCrop
}