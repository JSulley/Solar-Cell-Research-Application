# About this function:
# It records the smoothing spline model for every coordinate in 
# a dataset into a list.

################################################################################



ss_model_list_function <- function(wavelengths, intensities) {
  
  numberOfRows <- nrow(intensities)
  
  # Create list object with length of 'numberOfRow'
  z <- vector("list", numberOfRows)
  
  for (i in 1:numberOfRows) {
    
    z[[i]] <- smooth.spline(x = wavelengths, y = unlist(intensities[i,], use.names = FALSE))
    
  }
  z
}