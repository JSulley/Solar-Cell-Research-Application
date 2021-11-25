# About this function ----
# Obtaining list of local max coordinates
# This function determines and records the local max coordinates of every
# smoothing spline.
# The output is a list containing two lists:
# Local Max x-coordinate List and Local Max y-coordinate List
#
# Parameters
# wavelengths - Vector of wavelengths from uploaded dataset.
# spline_list - List of smoothing splines.


# Initializing Function ----
get_local_max_list <- function(wavelengths, spline_list) {
  min_wavelength <- min(wavelengths)  # Find minimum wavelength
  max_wavelength <- max(wavelengths)  # Find maximum wavelength
  
  # Initialize vector of limits of wavelengths
  endpoints <- c(min_wavelength, max_wavelength)
  
  number_of_rows <- length(spline_list)  # Determine number of rows
  
  # Create lists for x and y coordinates each with length of "number_of_rows"
  local_max_y_list <- vector("list", number_of_rows)
  local_max_x_list <- vector("list", number_of_rows)
  
  # Create for loop
  for (j in 1:number_of_rows) {
    smoothing_spline <- spline_list[[j]]  # Refer jth row smoothing spline
    coordinates_list <- predict(smoothing_spline, endpoints)  # Predict intensity at endpoints
    
    # Make intensities into vector without names
    endpoint_intensities <- unlist(coordinates_list$y, use.names = FALSE)
    
    # Find values where first derivative is 0
    piece_poly_spline <- SmoothSplineAsPiecePoly(smoothing_spline)
    critical_values <- solve(piece_poly_spline, deriv = 1)
    
    # Calculate intensity at each critical value
    critical_value_intensities <- predict(piece_poly_spline, critical_values)
    
    number_of_values <- length(critical_values)  # Find number of critical values
    local_max_vec <- integer(number_of_values)  # Build zero vector with length "number_of_values"
    
    # First derivative test
    for (i in 1:number_of_values) {
      # Generate test value to the left of critical value
      if (i == 1) {
        # Choose value between left endpoint and the first critical value
        limits <- c(min_wavelength, critical_values[i])
        left_test_value <- mean(limits) 
      } else {
        # Choose value between current and previous critical value
        limits <- c(critical_values[i], critical_values[i - 1])
        left_test_value <- mean(limits)
      }
      
      # Predict function value at left_test_value using the first derivative
      left_value <- predict(piece_poly_spline, left_test_value, deriv = 1)
      
      # Check if the left side of the critical value is increasing
      if (left_value > 0) {
        # Generate test value to the right of critical value
        if (i == number_of_values) {
          # Choose value between the last critical value and right endpoint
          limits <- c(critical_values[i], max_wavelength)
          right_test_value <- mean(limits) 
        } else {
          # Choose value between current and next critical value
          limits <- c(critical_values[i], critical_values[i + 1])
          right_test_value <- mean(limits)
        }
        
        # Predict function value at right_test_value using the first derivative
        right_value <- predict(piece_poly_spline, right_test_value, deriv = 1)
        
        # Check if the right side of the critical value is decreasing
        if (right_value < 0) {
          local_max_vec[i] <- 1  # Critical value is a local max
        }
      }
    }
    
    indexes <- which(local_max_vec == 1)  # Find indexes where value is 1
    local_max_y <- critical_value_intensities[indexes]  # Obtain values at indexes
    local_max_x <- critical_values[indexes]  # Obtain values at indexes
    greatest_local_max <- max(local_max_y)  # Highest local max intensity
    greatest_endpoint_int <- max(endpoint_intensities)  # Highest intensity at endpoints
    
    # Check if absolute max value corresponds to a critical value
    if (greatest_local_max > greatest_endpoint_int) {
      # Remove absolute max coordinates
      local_max_x <- local_max_x[- which.max(local_max_y)]
      local_max_y <- local_max_y[- which.max(local_max_y)]
    }
    
    # Add local_max_x and local_max_y to their respective list
    local_max_y_list[[j]] <- local_max_y
    local_max_x_list[[j]] <- local_max_x
  }
  local_max_list <- c(local_max_x_list, local_max_y_list)  # Merge lists
  local_max_list  # Print "local_max_list"
}
