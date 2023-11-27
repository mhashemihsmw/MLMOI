#' Estimates prevalences, frequency spectra and MOI parameter.
#'
#' @description \code{moimle()} derives the maximum-likelihood
#'  estimate (MLE) of the MOI parameter (Poisson parameter)
#'  and the lineage (allele) frequencies for each molecular
#'  marker in a dataset. Additionally, the lineage
#'  prevalence counts are derived.
#'
#' @param file string or data.frame; if file is a path it
#' must specify the path to the file to be imported. The
#' dataset can also be a data.frame object in R. The dataset
#' must be in standard format (see \code{\link{moimport}()}).
#' The first column must contain sample IDs. Adjacent columns
#' can contain metadata, followed by columns corresponding
#' to molecular markers.
#' @param nummtd numeric; number of metadata columns (e.g.
#'  date, sample location, etc.) in the dataset (default
#'  value is \code{nummtd = 0}).
#' @param bounds numeric vector; a vector of size 2,
#'  specifying a lower bound (1st element) and an upper
#'  bound (2nd element) for the MOI parameter. The function
#'  derives lineage frequency ML estimates by profiling the
#'  likelihood function on one of the bounds. For a marker
#'  without sign of super-infections, the lower bound is
#'  employed. If one allele is contained in every sample,
#'  the upper bound is employed.
#'
#' @return \code{moimle()} returns a nested list,  where the
#'  outer elements correspond to molecular markers in the
#'  dataset. The inner elements for each molecular marker
#'  contain the following information: \enumerate{\item
#'  sample size, \item allele prevalence counts, \item
#'  observed prevalences \item log likelihood at MLE, \item
#'  maximum-likelihood estimate of MOI parameter, \item
#'  maximum-likelihood estimates of lineage frequencies.}
#'
#'
#'
#' @details \code{moimle()} requires a dataset in standard
#'  format which is free of typos (e.g. incompatible and
#'  unidentified entries). Therefore, users need to
#'  standardize the dataset by employing the
#'  \code{\link{moimport}()} function.
#'
#'  If one or more molecular markers contain pathological
#'  data, the ML estimate for the Poisson parameter is
#'  either 0 or does not exist. Both estimates are
#'  meaningless, however, in the former case frequency
#'  estimates exist while they do not in the later. By
#'  setting the option \code{bounds} as a range for MOI
#'  parameter \eqn{\lambda}. i.e., \code{bounds = }
#'  c(<\eqn{\lambda_min}>, <\eqn{\lambda_max}>), this
#'  problem is bypassed and the ML estimates are calculated
#'  by profiling at \eqn{\lambda_min} or \eqn{\lambda_max}.
#'  If no super-infections are observed at a marker,
#'  \code{moimle()} uses \eqn{\lambda_min} as the MOI
#'  parameter estimate, \eqn{\lambda_max} if one lineage is
#'  present in all samples. For regular data, the
#'  profile-likelihood estimate using \eqn{\lambda_min} or
#'  \eqn{\lambda_max} is returned depending on whether the
#'  ML estimate falls below \eqn{\lambda_min} or above
#'  \eqn{\lambda_max}.
#'
#' @section Warnings: Warnings are issued, if data is
#'  pathological at one or multiple markers. If the option
#'  \code{bounds} is set, but MLE of MOI parameter at a
#'  molecular marker takes a lower or higher value than
#'  \eqn{\lambda_min or \lambda_max} respectively, a warning
#'  is generated.
#'
#' @seealso To import and transform data to standard format,
#'  please see the function \code{\link{moimport}()}.
#'
#' @export
#'
#' @examples
#'
#' #basic data analysis
#' infile1 <- system.file("extdata", "testDatamerge1.xlsx", package = "MLMOI")
#' mle1 <- moimle(infile1, nummtd = 1)
#'
moimle <-
    function (file, nummtd = 0, bounds = c(NA, NA))
    {
        oldops <- options(stringsAsFactors = FALSE, digits = 12)
        on.exit(options(oldops))
        #rJava::.jpackage("MLMOI")
        if (is.null(ncol(file)) == TRUE) {
            if (file.exists(file) == FALSE) {
                stop("File cannot be found. Check the path 'file'.", call. = FALSE)
            }
            #w_b <- openxlsx::loadWorkbook(file)
            #set_d <- XLConnect::readWorksheet(w_b, sheet = 1)
            set_d <- as.matrix(openxlsx::read.xlsx(file, sheet = 1))
        }
        else if (is.data.frame(file) == TRUE) {
            set_d <- file
        }
        else {
            stop("'file' needs to be eaither a path to the file or a data frame of dataset in standard format.",
                 call. = FALSE)
        }
        if (floor(prod(nummtd)) != prod(nummtd)) {
            stop("'nummtd' needs to take integer values.", call. = FALSE)
        }
        if ((nummtd + 2) > ncol(set_d)) {
            stop("Dataset does not contain molecular-marker columns. Make sure that 'nummtd' is set correctly.",
                    call. = FALSE)
        }
        alllabels <- colnames(set_d)
        markerlabels <- alllabels[(nummtd + 2):ncol(set_d)]
        samorder <- moi_labels(set_d[,1])[[1]]
        set_d <- as.matrix(set_d[, (nummtd + 2):ncol(set_d)])
        nummarker <- ncol(set_d)
        set_nnk <- list()
        bounds <- suppressWarnings(as.numeric(bounds))
        if (length(bounds) == 1 || length(bounds) > 2) {
            stop("The argument 'bounds' needs to be a range ( bounds = c(lambda_min, lambda_max)).",
                 call. = FALSE, noBreaks. = TRUE)
        }
        else if (sum(is.na(bounds)*1) == 1) {
            if (is.na(bounds[1]) == TRUE) {
                warning("lambda_min is not set.", call. = FALSE)
            }
            if (is.na(bounds[2]) == TRUE) {
                warning("lambda_max is not set.", call. = FALSE)
            }
            stop("The argument 'bounds' needs to be a range ( bounds = c(lambda_min, lambda_max)).",
                 call. = FALSE)
        }
        else if (sum(is.na(bounds)*1) == 0){
            lambda <- bounds
            k <- 0
            for (i in 1:nummarker){
                markerlabel <- markerlabels[i]
                datmarker <- as.vector(set_d[, i])
                markerstat <- moi_nk(datmarker, samorder)
                nn <- markerstat[[1]]
                nnk <- markerstat[[2]]
                obsperv <- markerstat[[3]]
                if (max(nnk) != nn && sum(nnk) != nn) {
                    k <- k + 1
                    mpp <- mle(as.vector(nnk), as.vector(nn))
                    logl <- as.matrix(mpp[[1]][1])
                    rownames(logl) <- "log likelihood = "
                    colnames(logl) <- ""
                    poissparam <- matrix(mpp[[1]][-1], 1, 2)
                    colnames(poissparam) <- c(expression(lambda), expression(psi))
                    rownames(poissparam) <- c ("MLE = ")
                    mlest <- mpp[[1]][2]
                    mpp <- t(as.matrix(mpp[[2]]))
                    colnames(mpp) <- colnames(nnk)
                    rownames(mpp)[1] <- c("MLE_p = ")
                    if (mlest > 0) {
                        lowvalue <- mlest < lambda[1]
                        highvalue <- mlest > lambda[2]
                        if (lowvalue == TRUE) {
                            warning("The MLE of MOI parameter at marker ", paste(shQuote(markerlabel, "sh"), collapse = ", "), "(", round(mlest, digits = 4),") is lower than minimum lambda.",
                                    call. = FALSE)
                            mpp[1,] <- mle_fixed(lambda[1], nnk)
                            logl[1,1] <- -nn*log(exp(lambda[1])-1) + sum(nnk*log(exp(lambda[1]*as.vector(mpp))-1))
                            psi <- lambda[1]/(1 - exp(-lambda[1]))
                            poissparam[1,] <- c(lambda[1], psi)
                            rownames(poissparam) <- "Fixed = "
                        }
                        else if (highvalue == TRUE) {
                            warning("The MLE of MOI parameter at marker ", paste(shQuote(markerlabel, "sh"), collapse = ", "), "(", round(mlest, digits = 4),") is higher than maximum lambda.",
                                    call. = FALSE)
                            mpp[1,] <- mle_fixed(lambda[2], nnk)
                            logl[1,1] <- -nn*log(exp(lambda[2])-1) + sum(nnk*log(exp(lambda[2]*as.vector(mpp))-1))
                            psi <- lambda[2]/(1 - exp(-lambda[2]))
                            poissparam[1,] <- c(lambda[2], psi)
                            rownames(poissparam) <- "Fixed = "
                        }
                    }


                }
                else if (sum(nnk) == nn && max(nnk) == nn) {
                    warning("Marker ", shQuote(markerlabel, "sh")," contains only one lineage.", call. = FALSE)
                    logl <- NA
                    poissparam <- t(as.matrix(c(NA, NA)))
                    colnames(poissparam) <- c("lambda", "psi")
                    rownames(poissparam) <- "MLE = "
                    mpp <- t(as.matrix(rep(1, length(nnk))))
                    colnames(mpp) <- colnames(nnk)
                    rownames(mpp)[1] <- c("MLE_p = ")
                }
                else {
                    if (max(nnk) == nn) {
                        psi <- lambda[2]/(1 - exp(-lambda[2]))
                        poissparam <- matrix(c(lambda[2], psi), 1, 2)
                        colnames(poissparam) <- c("lambda", "psi")
                        rownames(poissparam) <- "Fixed = "
                        mpp <- as.matrix(mle_fixed(lambda[2], nnk))
                        logl <- -nn*log(exp(lambda[2])-1) + sum(nnk*log(exp(lambda[2]*as.vector(mpp))-1))
                        logl <- as.matrix(logl)
                        rownames(logl) <- "log likelihood = "
                        colnames(logl) <- ""
                    }
                    else if (sum(nnk) == nn) {
                        psi <- lambda[1]/(1 - exp(-lambda[1]))
                        poissparam <- matrix(c(lambda[1], psi), 1, 2)
                        colnames(poissparam) <- c("lambda", "psi")
                        rownames(poissparam) <- "Fixed = "
                        mpp <- as.matrix(mle_fixed(lambda[1], nnk))
                        logl <- -nn*log(exp(lambda[1])-1) + sum(nnk*log(exp(lambda[1]*as.vector(mpp))-1))
                        logl <- as.matrix(logl)
                        rownames(logl) <- "log likelihood = "
                        colnames(logl) <- ""
                    }
                }
                colnames(mpp) <- colnames(nnk)
                rownames(mpp) <- "MLE_p = "
                set_nnk[[markerlabel]] <- list("Sample Size" = nn,
                                               "Allele Prevalence Counts" = nnk,
                                               "Observed Prevalences" = obsperv,
                                               "Log likelihood at MLE" = logl,
                                               "MOI Parameter MLEstimate and Average MOI" = poissparam,
                                               "Lineage Frequencies MLEstimates" = mpp)
            }
        }
        else {
            for (i in 1:nummarker){
                markerlabel <- markerlabels[i]
                datmarker <- as.vector(set_d[, i])
                markerstat <- moi_nk(datmarker, samorder)
                nn <- markerstat[[1]]
                nnk <- markerstat[[2]]
                obsperv <- markerstat[[3]]
                if (sum(nnk) == nn && max(nnk) == nn) {
                    warning("Marker ", shQuote(markerlabel, "sh")," contains only one lineage.", call. = FALSE)
                    logl <- NA
                    poissparam <- t(as.matrix(c(NA, NA)))
                    colnames(poissparam) <- c("lambda", "psi")
                    rownames(poissparam) <- "MLE = "
                    p <- t(as.matrix(rep(1, length(nnk))))
                    colnames(p) <- colnames(nnk)
                    rownames(p)[1] <- c("MLE_p = ")
                }
                else if (sum(nnk) == nn || max(nnk) == nn) {
                    if (max(nnk) == nn){
                        warning("At marker ", shQuote(markerlabel, "sh")," one lineage occurs in all samples. Users can assign bounds to the Poisson parameter (via option bounds) to derive the MLE for lineage frequencies.", call. = FALSE)
                        logl <- NA
                        poissparam <- t(as.matrix(c(NA, NA)))
                        colnames(poissparam) <- c("lambda", "psi")
                        rownames(poissparam) <- "MLE = "
                        p <- t(as.matrix(rep(NA, length(nnk))))
                        colnames(p) <- colnames(nnk)
                        rownames(p)[1] <- c("MLE_p = ")
                    }
                    else if (sum(nnk) == nn){
                        warning("Marker ", shQuote(markerlabel, "sh"),"  has no sign of superinfections. Users can assign bounds to the  Poisson parameter (via option bounds) to derive the MLE for lineage frequencies.",
                                call. = FALSE)
                        logl <- NA
                        poissparam <- t(as.matrix(c(0, 1)))
                        colnames(poissparam) <- c("lambda", "psi")
                        rownames(poissparam) <- "MLE = "
                        p <- t(as.matrix(as.vector(nnk)/as.vector(nn)))
                        colnames(p) <- colnames(nnk)
                        rownames(p)[1] <- c("MLE_p = Nk/N = ")
                    }

                }
                else {
                    mlestimate <- mle(as.vector(nnk), as.vector(nn))
                    logl <- as.matrix(mlestimate[[1]][1])
                    rownames(logl) <- "log likelihood = "
                    colnames(logl) <- ""
                    poissparam <- matrix(mlestimate[[1]][-1], 1, 2)
                    colnames(poissparam) <- c("lambda", "psi")
                    rownames(poissparam) <- c ("MLE = ")
                    p <- t(as.matrix(mlestimate[[2]]))
                    colnames(p) <- colnames(nnk)
                    rownames(p) <- c("MLE_p = ")
                }
                set_nnk[[markerlabel]] <- list("Sample Size" = nn,
                                               "Allele Prevalence Counts" = nnk,
                                               "Observed Prevalences" = obsperv,
                                               "Log likelihood at MLE" = logl,
                                               "MOI Parameter MLEstimate and Average MOI" = poissparam,
                                               "Lineage Frequencies MLEstimates" = p)
            }
        }
        set_nnk
    }








