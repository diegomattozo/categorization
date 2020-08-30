#' Discretize database via topdown greedy approach. Based on discretization package created by HyunJi Kim.
#'
#' @param db database to discretize (data.frame)
#' @param meth measures of associations available 1. Caim 2. Cacc 3. Ameva 4. Info. Statistics 5. Kendall's Tau-C
#' (numeric)
#' @param alpha Penalization to limit number of categories. Only for methods 4 and 5. (numeric)
#' @param n Desired Number of observations per Quantile. 
#'
#' @return list with discretized data, vector of quantile cutpoints and the supervised method cutpoints.
#' @export
discretize <- function(db, meth = 4, alpha = 0.05, n=30) {
  if(!is.data.frame(db)) stop('db must be a dataframe')
  if(meth < 1 || meth > 5) stop('method must be 1-5')
  db <- as.data.frame(db)
  
  db_copy <- db
  n_cols <- length(db)
  resp_name <- colnames(db)[n_cols]
  db[, resp_name] <- as.factor(db[, resp_name])

  discQ <- DiscByQuantile(db,n)
  db <- discQ$data
  Qcuts <- discQ$cuts
  
  alpha <- 1 + alpha
  Names <- names(db)[-length(db)]
  resp <- db[ , length(db)]
  cuts <- list()
  
  for (i in Names) {
    mask <- !is.na(db[[i]])
    DB <- data.frame(db[mask, i], resp[mask])
    aux <- disc.Topdown(DB, meth, alpha)
    cuts[[i]] <- aux$cutp
  }
  
  db <- apply_univariate_cutpoints(db_copy, Qcuts, cuts)
  list(data=db, qcuts=Qcuts, cuts=cuts) 
}

