kendal = function(tb) {
  abs(calc.KSTc(tb))
}


concordant <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  # get sum(matrix values > r AND > c)
  # for each matrix[r, c]
  mat.lr <- function(r, c)
  { 
    lr <- x[(r.x > r) & (c.x > c)]
    sum(lr)
  }
  
  # get row and column index for each
  # matrix element
  r.x <- row(x)
  c.x <- col(x)
  
  # return the sum of each matrix[r, c] * sums
  # using mapply to sequence thru each matrix[r, c]
  sum(x * mapply(mat.lr, r = r.x, c = c.x))
}


discordant <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  # get sum(matrix values > r AND < c)
  # for each matrix[r, c]
  mat.ll <- function(r, c)
  { 
    ll <- x[(r.x > r) & (c.x < c)]
    sum(ll)
  }
  
  # get row and column index for each
  # matrix element
  r.x <- row(x)
  c.x <- col(x)
  
  # return the sum of each matrix[r, c] * sums
  # using mapply to sequence thru each matrix[r, c]
  sum(x * mapply(mat.ll, r = r.x, c = c.x))
}


calc.KSTc <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  nr = dim(x)[1]
  c <- concordant(x)
  d <- discordant(x)
  m <- min(dim(x))
  n <- sum(x)
  
  KSTc <- (m * 2 * (c - d)) / ((n ^ 2) * (m - 1))
  
  KSTc
}
