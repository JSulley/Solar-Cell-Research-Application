# About this function:
# It determines and records the local max coordinates for every smoothing spline.
# The output is a list containing two lists:
# Local Max x-coordinate List, Local Max y-coordinate List

################################################################################


local_max_function <- function(wavelengths, ss_model_list) {
  
  # Make vector consisting of endpoints
  # Left endpoint: Minimum wavelength
  # Right endpoint: Maximum wavelength
  minimum_wavelength <- min(wavelengths)
  maximum_wavelength <- max(wavelengths)
  endpoints_wavelength <- c(minimum_wavelength, maximum_wavelength)
  
  number_of_rows <- length(ss_model_list)
  
  # Create lists for x and y coordinates each with length of 'number_of_rows'
  local_max_y_list <- vector("list", number_of_rows)
  local_max_x_list <- vector("list", number_of_rows)
  
  for (j in 1:number_of_rows) {
    
    # Pull smoothing spline for jth row and predict intensity at endpoints
    smooth <- ss_model_list[[j]]
    predicted <- predict(smooth, x = endpoints_wavelength)
    endpoints_intensity <- unlist(predicted$y, use.names = FALSE)
    
    # Solve for critical values where first derivative equals to 0
    # Predict intensity at those values
    piecePolySpline <- SmoothSplineAsPiecePoly(smooth)
    critical_values <- solve(piecePolySpline, deriv = 1)
    intensity_values_critical <- predict(piecePolySpline, critical_values)
    
    # Initialize zero vector with length of number of critical values
    number_of_values <- length(critical_values)
    local_max <- integer(number_of_values)
    
    # First derivative test 
    for (i in 1:number_of_values) {
      
      # If left side of critical value is increasing
      if (predict(piecePolySpline, critical_values[i] - 1e-8, deriv = 1) > 0) {
        
        # If right side of critical value is decreasing
        if (predict(piecePolySpline, critical_values[i] + 1e-8, deriv = 1) < 0) {
          
          # Critical value is a local max
          local_max[i] <- 1
          
        }
      } 
    }
    
    # Determine indexes where value is 1
    indexes <- which(local_max == 1)
    local_max_y <- intensity_values_critical[indexes]
    local_max_x <- critical_values[indexes]
    
    # Determine if absolute max value corresponds to a critical value
    if (max(local_max_y) > max(endpoints_intensity)) {
      
      # If it does, omit absolute max coordinates
      local_max_x <- local_max_x[-which.max(local_max_y)]
      local_max_y <- local_max_y[-which.max(local_max_y)]
      
    }
    
    # Add vectors to their respective list
    local_max_y_list[[j]] <- local_max_y 
    local_max_x_list[[j]] <- local_max_x
    
  }
  
  # Merge both lists into one
  local_max_list <- c(local_max_x_list,local_max_y_list)
  
  local_max_list
}
