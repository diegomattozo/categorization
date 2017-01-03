info_statistic = function(tb) {
  nr <- dim(tb)[1]
  g = sum(tb[,1])
  
  b = sum(tb[,2])
  #n = dim(tb)[1]
  info_stat = apply(tb, 1, func_info_stat, g, b)
  #info_stat[is.infinite(info_stat)] <- 0
  
  return(sum(info_stat))
}

func_info_stat = function(x, g, b) {
  return( (x[1]/g - x[2]/b) * log( max(x[1],0.1)/g * b/max(x[2],0.1) ) )
  
}



