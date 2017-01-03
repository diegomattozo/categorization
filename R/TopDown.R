# Code from discretization package
chiSq = function (tb) 
{
  tb <- tb + 1e-04
  e <- tb
  n <- sum(tb)
  p <- length(tb[, 1])
  q <- length(tb[1, ])
  mi <- numeric(p)
  ni <- numeric(q)
  for (i in 1:q) ni[i] <- sum(tb[, i])
  for (i in 1:p) {
    mi[i] <- sum(tb[i, ])
    e[i, ] <- mi[i] * ni/n
  }
  val <- sum((tb - e)^2/e)
  return(val)
}
topdown = function (data, method = 1, alpha) 
{
  maxP <- 28
  p1 <- length(data[1, ])
  p <- p1 - 1
  y <- as.integer(data[, p1])
  S <- dim(table(y))[1]
  n <- length(y)
  dCuts <- array(list(), p)
  for (i in 1:p) {
    x <- data[, i]
    od <- order(x)
    xo <- x[od]
    yo <- y[od]
    GlobalC <- 0
    cacc <- 0
    k <- 1
    addCut <- NULL
    ci <- NULL
    ci <- which(diff(xo) != 0)
    if (!is.null(ci)) 
      cuts <- (xo[ci] + xo[ci + 1])/2
    bd <- c(xo[1], xo[n])
    di <- cuts
    while (length(di) > 0) {
      ret <- findBest(xo, yo, bd, di, method)
      cacc <- ret$cacc
      Dp <- insert(ret$addCut, bd)
      di <- ret$newDi
      if(method %/% 4 == 0) alpha = 1
      if (( cacc > GlobalC*alpha || k < S) & (k < n)) {
        bd <- Dp
        GlobalC <- cacc
        k <- k + 1
      }
      else {
        Dp <- bd
        break
      }
    }
    dCuts[[i]] <- bd
  }
  return(dCuts)
}

findBest = function (x, y, bd, di, method) 
{
  n <- length(y)
  bestC <- 0
  cacc <- 0
  i <- 0
  addCut <- NULL
  iBest <- NULL
  newDi <- di
  nb <- numeric()
  for (i in 1:length(di)) {
    iw <- which(di[i] > bd)
    nb[iw] <- bd[iw]
    nl <- length(iw)
    nb[nl + 1] <- di[i]
    nb[(nl + 2):(length(bd) + 1)] <- bd[(nl + 1):length(bd)]
    bd1 <- nb
    dff <- findInterval(x, bd1, rightmost.closed = TRUE)
    tb <- table(dff, y)
    if (method == 1) 
      cacc <- caim(tb)
    if (method == 2) 
      cacc <- cacc(tb)
    if (method == 3) 
      cacc <- ameva(tb)
    if (method == 4)
      cacc <- info_statistic(tb)
    if(method == 5)
      cacc <- kendal(tb)
    if (method == 6) 
      cacc <- somersd(tb)
    if (cacc > bestC) {
      bestC <- cacc
      iBest <- i
      addCut <- di[i]
    }
  }
  if (!is.na(iBest)) 
    newDi <- di[-iBest]
  return(list(addCut = addCut, cacc = bestC, newDi = newDi, 
              bd = bd))
}

insert = function (x, a) 
{
  p <- length(a)
  i <- which(a > x)
  len <- length(i)
  if (len == p) 
    return(c(x, a))
  if (len == 0) 
    return(c(a, x))
  i1 <- i[1]
  return(c(a[1:(i1 - 1)], x, a[i1:p]))
}

disc.Topdown = function (data, method = 1, alpha = 1.05) 
{
  type <- c("CAIM", "CACC", "ameva", "Info Statistic", "Kendal-TauC", "SomersD")
  meth <- type[method]
  cutList <- topdown(data, method, alpha)
  p <- length(data[1, ]) - 1
  xd <- data
  for (i in 1:p) {
    cuts <- cutList[[i]]
    xd[, i] <- as.data.frame(as.integer(findInterval(data[, 
                                                          i], cuts, rightmost.closed = TRUE)))
  }
  list(cutp = cutList, Disc.data = xd)
}