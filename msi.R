# 04 sep 2014
# function to calculate Markham's seasonality index (MSI)
# input is the name of the precipitation dataset already as
# an array (matrix) of lon x lat x time dimensions (use the
# array read by R using get.var.ncdf)

msi <- function(prec) {
  
  # getting matrix dimensions
  d <- dim(prec)  # d[1]: lon; d[2]: lat; d[3]: time
  
  # vector defining angle direction for each month
  dir <- as.vector(c(pi/6, pi/3, pi/2, 2*pi/3, 5*pi/6, pi, 7*pi/6, 4*pi/3, 3*pi/2, 5*pi/3, 11*pi/6, 2*pi));
  
  # create matrices to store x and y componentes in polar coordinates, 
  # the magnitude matrix per year (mag_yr) and the index matrix 
  # per year(msi_yr)
  mp_yr = array(0, dim = c(d[1], d[2], floor(d[3]/12)))
  mag_yr = array(0, dim = c(d[1], d[2], floor(d[3]/12)))
  msi_yr = array(0, dim = c(d[1], d[2], floor(d[3]/12)))
  
  for (k in 1:floor(d[3]/12)) {  # annual loop
    aux = 12*(k-1)+1
    #print(k)
    x <- array(0, dim = c(d[1], d[2], 12))
    y <- array(0, dim = c(d[1], d[2], 12))
    for (j in 0:11) {
      # creating matrices
      dataAux <- matrix(0, nrow = d[1], ncol = d[2])
      xr <- matrix(0, nrow = d[1], ncol = d[2])
      yr <- matrix(0, nrow = d[1], ncol = d[2])
      # preparing vector elements to addition
      dataAux[ , ] <- prec[ , ,aux+j]
      mp_yr[ , , k] <- mp_yr[ , , k] + dataAux[ , ]
      x[ , ,j+1] <- sin(dir[j+1])*dataAux[ , ]
      y[ , ,j+1] <- cos(dir[j+1])*dataAux[ , ]
      remove(dataAux)
    }
    
    # adding 12-month matrices x and y
    xr[ , ] <- apply(x, 1:2, sum)
    yr[ , ] <- apply(y, 1:2, sum)
    # calculating the magnitude ((x^2 + y^2)^1/2)
    mag_yr[ , ,k] <- sqrt((xr^2) + (yr^2))
    # calculating index per year
    #mp_yr[ , , k] <- mp_yr[ , , k]/12
    msi_yr[ , , k] <- mag_yr[ , ,k] / mp_yr[ , ,k]
    #print(msi_yr[90:100,70:80,k])
    
    remove(xr); remove(yr); remove(x); remove(y)
  }
  
  # calculating the mean msi for all years
  msi <- msi_yr
  #print(dim(msi_yr))
  #msi <- apply(msi_yr, 1:2, mean)
}