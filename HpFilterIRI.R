# Inputs are Intensity Ratios and Intensity at 500 nm in column,
# Outputs are filtered Intensity Ratios and Intensity at 500 nm in column.
# Note that not all Intensity Ratios are filtered, only interested columns are selected.
# 2017-03-15, 1st built, Wang Weihua

HpFilterIRI <- function(kk,IRI){
  
  # kk is window length 2*kk+1 in indices
  
  source("./HampelFilter.R")

  xx0=IRI$IR_310T340
  yy0 <- HampelFilter(xx0,kk)
  # length(yy0[[1]]); length(yy0[[2]])
  plot(IRI$Time, IRI$IR_310T340, col='deepskyblue4', xlab='time', ylab='IR', main='Spikes removing on IR_310T340', type='l')
  lines(IRI$Time, yy0[[1]], col='firebrick1', xlab='time', ylab='IR', type='l')
  legend("topright", lwd=1, col=c("deepskyblue4", "firebrick1"), legend=c("bf. removing", "af. removing"))
  
  xx1=IRI$IR_340T440
  yy1 <- HampelFilter(xx1,kk)
  # length(yy1[[1]]); length(yy1[[2]])
  plot(IRI$Time, IRI$IR_340T440, col='deepskyblue4', xlab='time', ylab='IR', main='Spikes removing on IR_340T440', type='l')
  lines(IRI$Time, yy1[[1]], col='firebrick1', xlab='time', ylab='IR', type='l')
  legend("topright", lwd=1, col=c("deepskyblue4", "firebrick1"), legend=c("bf. removing", "af. removing"))
  
  xx2=IRI$IR_340T500
  yy2 <- HampelFilter(xx2,kk)
  # length(yy2[[1]]); length(yy2[[2]])
  plot(IRI$Time, IRI$IR_340T500, col='deepskyblue4', xlab='time', ylab='IR', main='Spikes removing on IR_340T500', type='l')
  lines(IRI$Time, yy2[[1]], col='firebrick1', xlab='time', ylab='IR', type='l')
  legend("topright", lwd=1, col=c("deepskyblue4", "firebrick1"), legend=c("bf. removing", "af. removing"))
  
  xx3=IRI$IR_340T675
  yy3 <- HampelFilter(xx3,kk)
  # length(yy3[[1]]); length(yy3[[2]])
  plot(IRI$Time, IRI$IR_340T675, col='deepskyblue4', xlab='time', ylab='IR', main='Spikes removing on IR_340T675', type='l')
  lines(IRI$Time, yy3[[1]], col='firebrick1', xlab='time', ylab='IR', type='l')
  legend("topright", lwd=1, col=c("deepskyblue4", "firebrick1"), legend=c("bf. removing", "af. removing"))
  
  xx4=IRI$IR_340T870
  yy4 <- HampelFilter(xx4,kk)
  # length(yy4[[1]]); length(yy4[[2]])
  plot(IRI$Time, IRI$IR_340T870, col='deepskyblue4', xlab='time', ylab='IR', main='Spikes removing on IR_340T870', type='l')
  lines(IRI$Time, yy4[[1]], col='firebrick1', xlab='time', ylab='IR', type='l')
  legend("topright", lwd=1, col=c("deepskyblue4", "firebrick1"), legend=c("bf. removing", "af. removing"))
  
  xx5=IRI$IR_440T500
  yy5 <- HampelFilter(xx5,kk)
  # length(yy5[[1]]); length(yy5[[2]])
  plot(IRI$Time, IRI$IR_440T500, col='deepskyblue4', xlab='time', ylab='IR', main='Spikes removing on IR_440T500', type='l')
  lines(IRI$Time, yy5[[1]], col='firebrick1', xlab='time', ylab='IR', type='l')
  legend("topright", lwd=1, col=c("deepskyblue4", "firebrick1"), legend=c("bf. removing", "af. removing"))
  
  xx6=IRI$IR_440T675
  yy6 <- HampelFilter(xx6,kk)
  # length(yy6[[1]]); length(yy6[[2]])
  plot(IRI$Time, IRI$IR_440T675, col='deepskyblue4', xlab='time', ylab='IR', main='Spikes removing on IR_440T675', type='l')
  lines(IRI$Time, yy6[[1]], col='firebrick1', xlab='time', ylab='IR', type='l')
  legend("topright", lwd=1, col=c("deepskyblue4", "firebrick1"), legend=c("bf. removing", "af. removing"))
  
  xx7=IRI$I_500
  yy7 <- HampelFilter(xx7,kk, t0=3)
  # length(yy7[[1]]); length(yy7[[2]])
  plot(IRI$Time, IRI$I_500, col='deepskyblue4', xlab='time', ylab='IR', main='Spikes removing on I_500', type='l')
  lines(IRI$Time, yy7[[1]], col='firebrick1', xlab='time', ylab='IR', type='l')
  legend("topright", lwd=1, col=c("deepskyblue4", "firebrick1"), legend=c("bf. removing", "af. removing"))
  
  # replace old data with filtered data 
  IRI$IR_310T340 <- yy0[[1]]
  IRI$IR_340T440 <- yy1[[1]]
  IRI$IR_340T500 <- yy2[[1]]
  IRI$IR_340T675 <- yy3[[1]]
  IRI$IR_340T870 <- yy4[[1]]
  IRI$IR_440T500 <- yy5[[1]]
  IRI$IR_440T675 <- yy6[[1]]
  IRI$I_500 <- yy7[[1]]
  
  return(IRI)
}
