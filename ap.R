# 04 dec 2014
# function to calculate mean monthly precipitation (mmp)
# input is the name of the precipitation dataset already as
# an array (matrix) of lon x lat x time dimensions (use the
# array read by R using get.var.ncdf)

ap <- function(prec) {
  
  # getting matrix dimensions
  d <- dim(prec)  # d[1]: lon; d[2]: lat; d[3]: time
  
  # create matrix to store ap values for each year (t = d[3]/12)
  ap <- array(0, dim = c(d[1], d[2], floor(d[3]/12)))
  
  for (i in 1:floor(d[3]/12)) {  # yearly loop
    for (j in 1:12) {  # month loop
      aux = 12*(i-1)+j
      ap[ , , i] <- ap[ , , i] + prec[ , , aux]
      #ap[ , , 1] <- ap[ , , 1] + prec[ , , aux]
      #print(mmp[200:210,170:180,i])
    }
  }
  ap
}