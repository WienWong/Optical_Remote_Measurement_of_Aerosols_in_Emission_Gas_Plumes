
# Median absolute deviation (MAD) outlier in Time Series
# 
# x - numeric vector representing a time series
# k - window length 2*k+1 in indices
# t0 - threshold, default is 3 (Pearson's rule)
#
# The 'median absolute deviation' computation is done in the [-k...k] vicinity of each point 
# at least k steps away from the end points of the interval. At the lower and upper end 
# the time series values are preserved.
#
# A high threshold makes the filter more forgiving, a low one will declare more points 
# to be outliers. t0<-3 (the default) corresponds to Ron Pearson's 3 sigma edit rule, 
# t0<-0 to John Tukey's median filter.
#
# Returning a list L with L$y the corrected time series and L$ind the indices of outliers 
# in the 'median absolut deviation' sense.
# 
# Author(s)
# HwB <hwborchers@googlemail.com> 

HampelFilter <- function (x, k, t0=3){
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  for (i in (k + 1):(n - k)) {
    x0 <- median(x[(i - k):(i + k)])
    S0 <- L * median(abs(x[(i - k):(i + k)] - x0))
    if (abs(x[i] - x0) > t0 * S0) {
      y[i] <- x0
      ind <- c(ind, i)
    }
  }
  list(y = y, ind = ind)
}
