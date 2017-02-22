## Quantile discretization for multivariate model.
quantile_disc = function(cov1,n=30) {

  cuts <- cutpoints(as.vector(cov1), n)
  cuts_val = numeric()
  #cuts[length(cuts)] = max(cov1)
  cuts[1] = -Inf
  disc = findInterval(cov1, cuts, rightmost.closed = T)
  while  ( sum(table(disc) < 30) > 0 )
  {
    tb = table(disc); val = as.numeric(names(tb[tb < 30]))
    for (i in val)
    {

      disc[disc == i] = i+1
      cuts_val = append(cuts_val, i+1) # found interval with less than 30 observations.
      #cuts = cuts[cuts != (i+1)]
    }

  }
  cuts[1] = -Inf;cuts[length(cuts)] = Inf;
  if (length(cuts_val) > 0)
  {
    cuts = cuts[-cuts_val]
    disc = findInterval(cov1, cuts, rightmost.closed = T)
  }
  list('data'=disc, 'cuts'=cuts[-c(1, length(cuts))]) # return without the extremes -Inf and Inf.
}


cutpoints = function(x, n=30) {
  len = length(x)
  if (len >= 30){ #data must have 30 observations
    if (len %/% n == 0) { n = 30 }
    if (len > n*200) { n = floor(0.005*len) }
    n_perc = len %/% n
    quants = seq(from=0, to=1, by=(100/n_perc)/100); quants[length(quants)] = 1
    #quants = quants[c(-1, -length(quants))]

    cpts = quantile(x, quants)
    #print(cpts)
  } else { stop("Error: Data must have at least 30 observations.")}
  unique(cpts[order(cpts)])
}

# Quantile discretization for univariate model.
quantileDisc = function(cov1,n=30) {

  cuts <- cutpoints(cov1, n)
  cuts_val = numeric()
  #cuts[length(cuts)] = max(cov1)
  cuts[1] = -Inf
  disc = findInterval(cov1, cuts, rightmost.closed = T)
  while  ( sum(table(disc) < 30) > 0 )
  {
    tb = table(disc); val = as.numeric(names(tb[tb < 30]))
    for (i in val)
    {

      disc[disc == i] = i+1
      cuts_val = append(cuts_val, i+1)
      #cuts = cuts[cuts != (i+1)]
    }

  }
  cuts[1] = -Inf;cuts[length(cuts)] = Inf;
  if (length(cuts_val) > 0)
  {
    cuts = cuts[-cuts_val]
    disc = findInterval(cov1, cuts, rightmost.closed = T)
  }
  list('data'=disc, 'cuts'=cuts)
}

# cutpoints = function(x, n=30) {
#   len = length(x)
#   if (len >= 30){ #data must have 30 observations
#     if (len %/% n == 0) { n = 30 }
#     if (len > n*200) { n = floor(0.005*len) }
#     n_perc = len %/% n
#     quants = seq(from=0, to=1, by=(100/n_perc)/100); quants[length(quants)] = 1
#     #quants = quants[c(-1, -length(quants))]
#
#     cpts = quantile(x, quants)
#     #print(cpts)
#   } else { stop("Error: Data must have at least 30 observations.")}
#   unique(cpts)
# }

DiscByQuantile = function(db, n = 30) {
  len = length(db)
  Names = names(db)[-length(db)]
  resp = db[, len]
  List = list()
  cuts = list()
  for (i in Names) {
    #DB = data.frame(db[i], resp)
    aux_ = quantileDisc(db[[i]], n)
    aux = aux_$data
    cuts[[i]] = aux_$cuts
    List[[i]] = aux
  }
  #Dados Discretizados
  DiscPerc = data.frame(List, resp)

  list('data'=DiscPerc, 'cuts'=cuts)
}


#' Discretize a continuous database by quantile. To be used by the Multivariate Method.
#'
#' @param db Dataframe to discretize.
#' @param n Numer of observations by quantile.
#'
#' @return List with the discretized dataframe and a vector of cutpoints.
#' @export
quantile_discretization = function(db, n = 30) {
  len = length(db)
  Names = names(db)[-length(db)]
  resp = db[, len]
  List = list()
  cuts = list()
  for (i in Names) {
    #DB = data.frame(db[i], resp)
    aux_ = quantile_disc(db[[i]], n)
    aux = factor(aux_$data)
    cuts[[i]] = aux_$cuts
    List[[i]] = aux
  }
  DiscPerc = data.frame(List, resp)

  list('data'=DiscPerc, 'cuts'=cuts)
}
