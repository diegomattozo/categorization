caim = function (tb) 
{
  nr <- dim(tb)[1]
  nc <- dim(tb)[2]
  maxr <- apply(tb, 1, max)
  Mr <- apply(tb, 1, sum)
  sum(maxr^2/Mr)/nr
}