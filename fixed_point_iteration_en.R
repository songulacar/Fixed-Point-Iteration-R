# We will demonstrate fixed-point iteration both as a single final result
# and by generating a step-by-step table.
# Here:
# g : the function whose fixed point we want to approximate
# p0 : initial approximation (starting point)
# tol : tolerance threshold
# iter: maximum number of iterations

fixed_point_method <- function(g,p0,tol,iter){
  i=1
  while(i<=iter){
    p=g(p0)
    if(abs(p-p0)<tol){
      return(p)
    }
    else{
      i=i+1
    }
    p0=p
    
  }
  cat("Maximum number of iterations exceeded")
  return(p)
}

# The "fixed_point_method" function is used by defining the function g.
# As an example, let's approximate g(x) = 10/(4+x) starting with p0 = 1.5
# a tolerance of 1e-4, and a maximum of 30 iterations.

g <- function(x){10/(4+x)}
fixed_point_method(g,1.5,1e-4,30)


# If we want to see the p value at each step in a table format:

fixed_point_method_table<- function(g,p0,tol,iter){
  
  i=1
  gn=numeric(iter)
  P0=p0
  while(i<=iter){
    p=g(p0)
    gn[i] <- p
    if(abs(p-p0)<tol){
      return(data.frame(n=0:i,
                        gn=c(P0,gn[1:i])
      ))
    }
    else{
      i=i+1
    }
    p0=p
    
  }
  cat("Maximum number of iterations exceeded")
  return(data.frame(n=0:iter,
                    gn=c(P0,gn)))
}
# To view the example function g in table format,
# the "fixed_point_method_table" function is called. 

fixed_point_method_table(g,1.5,1e-4,30)