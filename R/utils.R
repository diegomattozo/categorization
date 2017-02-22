#' Separate train and test database.
#'
#' @param db Dabatase to separate between trainning and test sets (data.frame)
#' @param percentual percentual dedicated to test set (numeric)
#' @param seed A seed (numeric)
#' @return list containing the databases (list)
#' @export
train_test_split = function(db, percentual, seed = 999) {
  set.seed(seed)
  lenDB = length(db[,1])
  aux = rep(F, lenDB)
  n = floor(lenDB * percentual)
  aux[sample(1:lenDB, n)] = T
  train = db[!aux,]
  test = db[aux,]

  list('train'=train, 'test'=test)
}


#' Discretize a database with given cutpoints. To be used with cutpoints returned from multivariate disc.
#' @param db: database (data.frame)
#' @param cutpoints: vector of cutpoints (numeric)
#'
#' @return list containing discretized database (list)
#' @export
cutpoint_discretization = function(db, cutpoints) {
  len = length(db)
  Names = names(db)[-length(db)]; respName = names(db)[len]
  resp = db[, len]
  ChList = list()
  k=1
  for (i in Names) {
    cuts_ = cutpoints[[i]]

    if (is.nan(cuts_[1])) next # oh? One interval variable? remove it.

    cuts = c(-Inf, cuts_, Inf) # add extreme points. it could be min(x_i) and max(x_i)
    covaux = findInterval(db[[i]], cuts, rightmost.closed = TRUE)
    covaux = as.factor(covaux)
    ChList[[i]] = covaux
    k=k+1
  }
  ChList[[respName]] = resp
  data =  as.data.frame(ChList)
}

# Initialize cutpoints for each covariate with a NaN value. Returns a list: varname => NaN.
initialize_cutpoints = function(varNames) {
  initial_cutpoints = list()
  for (i in varNames) {
    initial_cutpoints[[i]] = NaN
  }
  initial_cutpoints
}

#' Categorize a database from the cutpoints returned from Univariate Methods.
#'
#' @param db: database (data.frame)
#' @parm cutpoints: vector of cutpoints (numeric)
#'
#' @return list containing discretized database (list)
#' @export
#'
#' @examples
disc_from_cuts = function (db, cutpoints) {
  len = length(db)
  Names = names(db)[-length(db)]; respName = names(db)[len]
  resp = db[, len]
  ChList = list()
  k=1
  for (i in Names) {
    cuts = unlist(cutpoints[i])
    cuts[1] = -Inf;cuts[length(cuts)] = Inf
    #if (k==3) { print(findInterval(testDB[[i]], cuts, rightmost.closed = TRUE)) }
    covaux = findInterval(db[[i]], cuts, rightmost.closed = TRUE)
    covaux = as.factor(covaux)
    ChList[[i]] = covaux
    k=k+1
  }
  ChList[[respName]] = resp
  data =  as.data.frame(ChList)

}
