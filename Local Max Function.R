# About this function:
# It determines and records the local max coordinates for every smoothing spline.
# The output is a list containing two lists:
# Local Max x-coordinate List, Local Max y-coordinate List

################################################################################

local_max_function <- function(dataframe) {
  
  #Convert dataset to matrix
  data_matrix <- as.matrix(dataframe)
  
  #Subset the wavelengths (first row) from 'data_matrix'
  #The first two entries in that row are NA so omit them
  dataset_wavelengths <- data_matrix[1,-(1:2)]
  
  #Make vector consisting of endpoints
  #Left endpoint: Minimum wavelength
  #Right endpoint: Maximum wavelength
  minimum_wavelength <- min(dataset_wavelengths)
  maximum_wavelength <- max(dataset_wavelengths)
  endpoints_wavelength <- c(minimum_wavelength, maximum_wavelength)
  
  #Remove wavelengths and coordinates
  #Wavelengths are in the first row
  #Coordinates are in the first two columns
  data_matrix <- data_matrix[-1,-(1:2)]
  
  #Number of rows
  number_of_rows <- nrow(data_matrix)
  
  #Create a list for local max values
  local_max_y_list <- vector("list", number_of_rows)
  local_max_x_list <- vector("list", number_of_rows)
  
  for (j in 1:number_of_rows) {
    
    #Subset the ith row and convert to a vector by unlisting along with
    #getting rid of the names associated with each element.
    intensity <- unlist(data_matrix[j,], use.names = FALSE)
    
    #Normalize intensity values by dividing by max intensity
    normal_intensity <- intensity/max(intensity)
    
    #Create smooth spline based on values
    smooth <- smooth.spline(dataset_wavelengths, normal_intensity)
    predicted <- predict(smooth, x = endpoints_wavelength)
    endpoints_intensity <- unlist(predicted$y, use.names = FALSE)
    
    #Solve for critical values where derivative equals to 0
    piecePolySpline <- SmoothSplineAsPiecePoly(smooth)
    critical_values <- solve(piecePolySpline, deriv = 1)
    intensity_values_critical <- predict(piecePolySpline, critical_values)
    
    #Initialize vector with 0's with length of
    #number of critical values
    number_of_values <- length(critical_values)
    local_max <- integer(number_of_values)
    
    #First derivative test 
    for (i in 1:number_of_values) {
      
      #If left side of critical value is increasing
      if (predict(piecePolySpline, critical_values[i] - 1e-8, deriv = 1) > 0) {
        
        #If right side of critical value is decreasing
        if (predict(piecePolySpline, critical_values[i] + 1e-8, deriv = 1) < 0) {
          
          #Critical value is a local max
          local_max[i] <- 1
          
        }
      } 
    }
    
    #Determine indexes where value is 1
    local_max_y <- intensity_values_critical[which(local_max == 1)]
    local_max_x <- critical_values[which(local_max == 1)]
    
    #Determine if max value corresponds to a critical value or not
    if (max(local_max_y) > max(endpoints_intensity)) {
      
      #If it is in 'local_max_y', omit it along with the x-coordinate
      local_max_x <- local_max_x[-which.max(local_max_y)]
      local_max_y <- local_max_y[-which.max(local_max_y)]
      
    }
    
    #Add vectors to their respective list
    local_max_y_list[[j]] <- local_max_y 
    local_max_x_list[[j]] <- local_max_x
    
  }
  
  #Merge both lists into one
  local_max_list <- c(local_max_x_list,local_max_y_list)
  
  #Display result
  local_max_list
}
