#' Derives MLE
#'
#' @param nnk numeric vector; vector of lineage prevalence
#'   counts.
#' @param nn numeric; sample size
#'
#' @return a list with the following elements: 1) log
#'   likelihood at MLE, MLE of lambda and MLE of psi, 2) MLE
#'   of lineage frequencies.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimle}}
#'
mle <- function(nnk, nn){
  sel <- nnk
  nnk <- sel[sel>0]
  nknk <- nnk/nn
  l1 <- 2.5         # initial value
  l0 <- 0
  eps <- 10^(-8)       # precision
  out <- list(NA, NA,NA,NA,NA)
  k <- 1
  while(abs(l0-l1)>eps && k<50 && l1>0){
    k <- k+1
    l0 <- l1
    l1 <- l0-(l0+sum(log(1-nknk*(1-exp(-l0)))))/(1-sum(nknk/(exp(l0)*(1-nknk)+nknk)))
  }
  if(k==50 || l1<0){
    for(st in 1:10){
      l1 <- st
      l0 <- l1+1
      k <- 1
      while(abs(l0-l1)>eps && k<100 && l1>0){
        k <- k+1
        l0 <- l1
        l1 <- l0-(l0+sum(log(1-nknk*(1-exp(-l0)))))/(1-sum(nknk/(exp(l0)*(1-nknk)+nknk)))
      }
      if(abs(l0-l1)<eps){
        break
      }
    }
    if(abs(l0-l1)>eps){
      l1 <- Rmpfr::mpfr(10,precBits=100)
      l0 <- l1+1
      while(abs(l0-l1)>eps){
        l0 <- l1
        l1=l0-(l0+sum(log(1-nknk*(1-exp(-l0)))))/(1-sum(nknk/(exp(l0)*(1-nknk)+nknk)))
      }
    }
  }
  mle_lam <- l1
  mle_psi <- mle_lam/(1-exp(-mle_lam))
  mle_p <- -log(1-nknk*(1-exp(-mle_lam)))/mle_lam
  ml <- -nn*log(exp(mle_lam)-1) + sum(nnk*log(exp(mle_lam*mle_p)-1))
  ml_p <- array(0,length(sel))
  ml_p[sel > 0] <- mle_p

  out <- list(c(ml, mle_lam, mle_psi), ml_p)

  out
}





#' Derives profile-likelihood MLE of lineage frequencies
#'
#' @param lambda numeric; MOI parameter
#' @param nnk numeric vector; vector of lineage prevalence
#'   counts.
#'
#' @return vector of lineage frequency estimates
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimle}}
#'
mle_fixed <- function(lambda, nnk)
{
  eps <- 10^(-6)
  nl <- lambda * nnk
  bzero <- max(nl) + 1
  bone <- 0
  while (abs(bzero-bone) > eps) {
    bone <- bzero
    bzero <- bzero -  bzero *
      (lambda + sum(log(1 - nl/bzero)))/sum(nl/(bzero - nl))
  }
  mle_p <- - log(1 - nl/bzero)/lambda
  mle_p
}



#' Derives lineage prevalence counts
#'
#' @param datmarker vector; a column of data corresponding
#'   to a marker from the imported dataset.
#' @param samorder numeric vector; row numbers in excel file
#'   where the new samples start.
#'
#' @return a list with two elements: 1) sample size of the
#'   marker, 2) vector of lineage prevalence counts at the
#'   marker.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimle}}
#'
moi_nk <-
  function (datmarker, samorder)
  {
    set_nk <- list()
    nnk <- t(as.matrix(table(datmarker)))
    nn <- datmarker[samorder]
    nn <- as.matrix(length(nn[!is.na(nn)]))
    rownames(nnk) <- "Nk = "
    rownames(nn) <- "N = "
    colnames(nn) <- ""
    set_nk <- list(nn, nnk)
    set_nk
  }



