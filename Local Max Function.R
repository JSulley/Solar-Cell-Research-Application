#Assumes that local min values are not wanted
local_max_func <- function(x) {
  
  #Convert dataset to matrix
  z <- as.matrix(x)
  
  #Extract Wavelengths
  wave <- z[1,-(1:2)]
  
  #Wavelengths and coordinates (1st 2 columns are not needed)
  z <- z[-1,-(1:2)]
  
  #Create a list for local max values
  local_max_y_list <- vector("list", nrow(z))
  local_max_x_list <- vector("list", nrow(z))
  
  #Run through each row
  for (j in 1:nrow(z)) {
    
    #Extract jth row
    intensity <- z[j,]
    
    #Normalize it
    intensity_norm <- intensity/max(intensity)
    
    #Create smooth spline based on values
    smooth <- smooth.spline(wave, intensity_norm)
    predicted <- predict(smooth, x = wave)
    
    #Solve for critical values where derivative equals to 0
    newsmooth <- SmoothSplineAsPiecePoly(smooth)
    xs <- solve(newsmooth, deriv = 1)
    ys <- predict(newsmooth, xs)
    
    #Create 0 vector with length of xs vector
    ext_val <- integer(length(xs))
    
    #First derivative test 
    for (i in 1:length(xs)) {
      
      #If left side of critical value is increasing
      if (predict(newsmooth,xs[i] - 1, deriv = 1) > 0) {
        
        #If right side of critical value is decreasing
        if (predict(newsmooth,xs[i] + 1,deriv = 1) < 0) {
          
          #Assign value 1
          ext_val[i] <- 1
        }
      } 
    }
    
    #Determine local max values from ys vector. 
    #These values make up local_max_y
    local_max_y <- ys[which(ext_val == 1)]
    local_max_x <- xs[which(ext_val == 1)]
    
    #Determine if absolute max is one of the local max values
    if (max(predicted$y) %in% local_max_y) {
      
      #Remove if true
      local_max_y <- local_max_y[-which(local_max_y == max(predicted$y))]
      local_max_x <- local_max_x[-which(local_max_y == max(predicted$y))]
    }
    
    #Add vectors to respective list
    local_max_y_list[[j]] <- local_max_y 
    local_max_x_list[[j]] <- local_max_x
  }
  
  #Merge both lists into one
  local_max_list <- c(local_max_x_list,local_max_y_list)
}