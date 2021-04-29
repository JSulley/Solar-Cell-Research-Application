# About This Function:
# For each coordinate, the function finds a smoothing spline
# that describes the relationship between the given intensity values and
# wavelengths. The absolute max point on this curve is then calculated.
# The coordinate, peak wavelength, and absolute max intensity
# are recorded, and the function goes to the next coordinate repeating
# the same process.
# The absolute max intensities are then normalized by dividing each value
# by the largest absolute max intensity, and these values are placed next to the original 
# values. 
# The output is a dataframe with the following columns:
# x-coordinate, y-coordinate, Peak Wavelength, Absolute Max Intensity, Normal Intensity
# 
# Note: 
# Peak Wavelength is the wavelength that outputs the absolute max intensity

################################################################################


absolute_max_function <- function(wavelengths, coordinates, ss_model_list) {
  
  # Make vector consisting of endpoints
  # Left endpoint: Minimum wavelength
  # Right endpoint: Maximum wavelength
  min_wavelength <- min(wavelengths)
  max_wavelength <- max(wavelengths)
  wavelength_limits <- c(min_wavelength, max_wavelength)
  
  number_of_rows <- nrow(coordinates)
  
  # Initialize matrix with all zeros
  # Size: 'number_of_rows' by 5
  # 5 columns: x-coordinate, y-coordinate, peak wavelength, absolute max intensity, normal intensity
  # The number of entries = number_of_rows * 5
  number_of_columns <- 5
  entries <- rep(0, number_of_rows * number_of_columns)
  
  absolute_max_matrix <- matrix(entries, ncol = number_of_columns)
  
  for (i in 1:number_of_rows) {
    
    # Solve absolute max coordinate problem by first calculating the intensity at the
    # lower/upper limits of the wavelengths (wavelength_limits) using the
    # ith row's smooth curve from 'ss_model_list'.  
    coordinate_list <- predict(ss_model_list[[i]], x = wavelength_limits)
    
    # y-values in 'coordinate_list' are the predicted intensity values
    intensity_values <- unlist(coordinate_list$y, use.names = FALSE)
    
    # Remove names from each element
    intensity_values_endpoints <- unlist(intensity_values, use.names = FALSE)
    
    # Find critical values where first derivative is 0
    piecePolySpline <- SmoothSplineAsPiecePoly(ss_model_list[[i]])
    critical_values <- solve(piecePolySpline, deriv = 1)
    
    # Calculate intensity at each critical value
    intensity_values_critical <- predict(piecePolySpline, critical_values)
    
    # Determine if max intensity value corresponds to a critical value or endpoint
    if (max(intensity_values_critical) > max(intensity_values_endpoints)) {
      
      # Local extrema contain the absolute max
      absolute_max_intensity <- max(intensity_values_critical)
      peak_wavelength <- critical_values[which.max(intensity_values_critical)]
      
    } else {
      
      # Endpoints contain the absolute max
      absolute_max_intensity <- max(intensity_values_endpoints)
      peak_wavelength <- wavelength_limits[which.max(intensity_values_endpoints)]
      
    }
    
    # Record values
    absolute_max_matrix[i, 1:4] <- c(coordinates[i,1], coordinates[i,2], peak_wavelength, absolute_max_intensity)
    
  }
  
  # Find normalized column by determining the max absolute intensity value and dividing
  # every number in the "intensity" column by this value.
  max_intensity_value <- max(absolute_max_matrix[,4])
  normalized_intensity <- absolute_max_matrix[,4]/max_intensity_value
  
  # 5th column of matrix is normalized max intensities
  absolute_max_matrix[,5] <- normalized_intensity
  
  absolute_max_dataframe <- as.data.frame(absolute_max_matrix)
  
  absolute_max_dataframe
  
}
