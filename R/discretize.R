#' Discretize database via topdown greedy approach. Based on discretization package created by HyunJi Kim.
#'
#' @param db database to discretize (data.frame)
#' @param meth measures of associations available 1. Caim 2. Cacc 3. Ameva 4. Info. Statistics 5. Kendall's Tau-C
#' (numeric)
#' @param alpha Penalization to limit number of categories. Only for methods 4 and 5. (numeric)
#' @param n Number of observations per Quantile. If prediscretized = F.
#' @param prediscretized Are database prediscretized already? If not we prediscretize by quantile. (logical)
#'
#' @return list with discretized data and vector of cutpoints.
#' @export
discretize = function(db, meth = 4, alpha = 1.05, n=30, prediscretized = F) { #Discretize Variables
  if(!is.data.frame(db)) stop('db must be a dataframe')
  if(meth < 1 || meth > 5) stop('method must be 1-5')

  if ( prediscretized == F)  db = DiscByQuantile(db,n)$data#preprocessing the data.

  Names = names(db)[-length(db)]
  resp = db[ ,length(db)]
  ChList = list()
  cuts = list()
  for (i in Names) {
    DB = data.frame(db[i], resp)
    aux = disc.Topdown(DB, meth, alpha)
    cuts[[i]] = aux$cutp
    aux = aux$Disc.data
    ChList[[i]] = as.factor(aux[[1]])

  }
  DiscData = data.frame(ChList, resp)

  list('data'=DiscData, 'cuts'=cuts)
}

