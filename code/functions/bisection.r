bisection <- function(f, lower, upper, tol = 10^-8, max.iter = 1000) { 
  
  # from http://stackoverflow.com/questions/15314763/set-a-maximum-iteration-for-bisection-method  
  
  flow <- f(lower) 
  fupper <- f(upper) 
  feval <- 2 
  
  if (flow * fupper > 0) stop("The given interval does not contain the root!
\n") 
  diff <- upper - lower 
  
  while ((abs(diff) > tol) & (feval <= max.iter)) { 
    newpoint <- (lower + upper) / 2 
    newf <- f(newpoint) 
    if (abs(newf) <= tol) break 
    if (flow * newf < 0) upper <- newpoint 
    if (fupper * newf < 0) lower <- newpoint 
    diff <- upper - lower 
    feval <- feval + 1 
  } 
  list(x = newpoint, value = newf, fevals = feval) 
} 