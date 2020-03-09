# @description computes alpha-level estimate of active voxels.
#   
tdp1 <- function (hommel, ix, alpha) 
{
  m <- length(hommel@p)
  if (missing(ix)) {
    d <- discoveries(hommel, alpha = alpha)
    k <- m
  }
  else {
    p <- hommel@p[ix]
    k <- length(p)
    d <- discoveries(hommel, ix, alpha = alpha)
  }
  d/k
}

summary_hommel_roi <- function(hommel,ix,alpha){
  Total=length(hommel@p[ix])
  False_Null=hommel::discoveries(hommel, alpha=alpha, ix=ix)
  True_Null=Total-False_Null
  Active_Proportion= tdp1(hommel, ix=ix, alpha = alpha)
  list(Size=Total,FalseNull=False_Null,TrueNull=True_Null,ActiveProp=Active_Proportion)
}

