#' Merges two molecular datasets.
#'
#' @description The function is designed to merge two
#'   datasets from separate Excel files. The data in
#'   each Excel file is placed in the first
#'   worksheet.
#'
#' @param file1 string; specifying the path of the first
#'   dataset.
#' @param file2  string; specifying the path of the second
#'   dataset.
#' @param nummtd1 numeric; number of metadata columns (see
#'   \code{\link{moimport}()}) in the first file (default as
#'   \code{0}).
#' @param nummtd2 numeric; number of metadata columns (see
#'   \code{\link{moimport}()}) in the second file (default as
#'   \code{0}).
#' @param keepmtd logical; determining whether metadata
#'   (e.g., date) should be retained (default as
#'   \code{TRUE}).
#' @param export string; the path where the data is stored.
#'
#' @param keepwarnings string; the path where the warnings
#' are stored.
#'
#' @return The output is a dataset in standard format which
#'   constitutes of an assembly of the input datasets.
#'
#' @details The two datasets should be already in standard
#'   format (see \code{\link{moimport}()}). The datasets
#'   are placed in the first worksheet of the two different
#'   Excel files. Notice that marker labels (=column
#'   labels) need to be unique.
#'
#' @section Warnings: Warnings are generated if potential
#'   inconsistencies are detected. E.g., if the same sample
#'   occurs in both datasets and have contradicting metadata
#'   entries. The function only prints the first 50 warnings.
#'   If the number of warnings are more than 50, the user is
#'   recommended to set the argument \code{keepwarnings},
#'   in order to save the warnings in an Excel file.
#'
#' @seealso To import and transform data into standard
#'   format, please see the function \code{\link{moimport}()}.
#'
#' @export
#'
#' @examples
#' #The datasets 'testDatamerge1.xlsx' and 'testDatamerge1.xlsx' are already in standard format:
#'
#' infile1 <- system.file("extdata", "testDatamerge1.xlsx", package = "MLMOI")
#' infile2 <- system.file("extdata", "testDatamerge2.xlsx", package = "MLMOI")
#' outfile <- moimerge(infile1, infile2, nummtd1 = 1, nummtd2 = 2, keepmtd = TRUE)
#'
moimerge <-
    function(file1, file2, nummtd1, nummtd2, keepmtd = FALSE, export = NULL, keepwarnings = NULL)
    {
        oldops <- options(stringsAsFactors = FALSE)
        on.exit(options(oldops))
        #KS# rJava::.jpackage("MLMOI")
        nummtd1 <- nummtd1 + 2
        nummtd2 <- nummtd2 + 2
        metadata_warnings <- list()
        "First dataset"
        if (file.exists(file1) == FALSE) {
            stop("File 'file1' cannot be found. Check the path 'file'.", call. = FALSE)
        }
        if (file.exists(file2) == FALSE) {
            stop("File 'file2' cannot be found. Check the path 'file'.", call. = FALSE)
        }
        if (keepmtd*1 != 1 && keepmtd*1 != 0) {
            stop("'keepmtd' needs to be boolean.", call. = FALSE)
        }
        else if (length(keepmtd) > 1) {
            stop("'keepmtd' needs to be boolean.", call. = FALSE)
        }
        if (floor(prod(nummtd1)) != prod(nummtd1)) {
            stop("'nummtd1' needs to take integer values.", call. = FALSE)
        }
        if (floor(prod(nummtd2)) != prod(nummtd2)) {
            stop("'nummtd2' needs to take integer values.", call. = FALSE)
        }
        #KS# wb_1 <- XLConnect::loadWorkbook(file1)
        #KS# data_1 <- XLConnect::readWorksheet(wb_1,1)

        data_1 <- openxlsx::read.xlsx(file1,1)
        colnames(data_1)[1] <- "Sample IDs"
        nc_1 <- ncol(data_1)
        mlabels_1 <- colnames(data_1)[nummtd1:nc_1]
        "Second dataset"
        #wb_2 <- XLConnect::loadWorkbook(file2)
        #data_2 <- XLConnect::readWorksheet(wb_2,1)
        data_2 <- openxlsx::read.xlsx(file2,1)
        colnames(data_2)[1] <- "Sample IDs"
        nc_2 <- ncol(data_2)
        mlabels_2 <- colnames(data_2)[nummtd2:nc_2]
        "checking if there are common marker labels"
        mmll <- match(mlabels_2,mlabels_1)
        mmll <- mmll[!is.na(mmll)]
        if (length(mmll) > 0) {
            warning("Marker labels need to be unique.", call. = FALSE)
        }
        "merging data"
        set_d <- merge(data_1,data_2, all = TRUE)
        if (nummtd1 == 2){
            set_d <- set_d[,c("Sample IDs",colnames(data_2)[2:(nummtd2 - 1)], mlabels_1, mlabels_2)]
        }
        else if (nummtd2 == 2){
            set_d <- set_d[,c("Sample IDs",colnames(data_1)[2:(nummtd1 - 1)], mlabels_1, mlabels_2)]
        }
        else if (nummtd1 == 2 && nummtd2 == 2){
            set_d <- set_d[,c("Sample IDs", mlabels_1, mlabels_2)]
        }
        else {
            set_d <- set_d[,c("Sample IDs",colnames(data_1)[2:(nummtd1 - 1)],
                              colnames(data_2)[2:(nummtd2 - 1)],mlabels_1,mlabels_2)]
        }
        "rearranging data"
        nummtd <- nummtd1 + nummtd2 - 2
        nc <- nc_1 + nc_2 - 1
        markerlabels <- colnames(set_d)[nummtd:ncol(set_d)]
        sam_id <- moi_labels(set_d[,1])
        sam <- sam_id[[1]]
        samall <- sam_id[[2]]
        lsam <- length(sam) - 1
        if (keepmtd == TRUE) {
            tempmtd <- set_d[2:(nummtd - 1)]
            mtdall <- c(colnames(data_1[2:(nummtd1 - 1)]),colnames(data_2[2:(nummtd2 - 1)]))
            dupmtd <- which(duplicated(mtdall) == TRUE)
            if (length(dupmtd) > 0) {
                tempmtd <- as.matrix(tempmtd[,-dupmtd])
            }
        }
        exporting <- list()
        ll <- matrix(0, nc - nummtd + 1, lsam)
        for (i in nummtd:nc){
            col_j <- as.matrix(set_d[,i])
            marker <- list()
            for(j in 1:lsam){
                sam_i <- col_j[(sam[j]):(sam[j + 1] - 1), ]
                sam_i <- sam_i[!is.na(sam_i)]
                sam_i <- sam_i[!duplicated(sam_i)]
                if(length(sam_i) == 0){
                    sam_i <- NA
                }
                marker[[j]] <- sam_i
            }
            exporting[[(i - nummtd + 1)]] <- marker
            ll[(i - nummtd + 1), ] <- unlist(lapply(marker, length))
        }
        l <- apply(ll, 2, max)
        for (k in 1:length(exporting)) {
            for (j in 1:lsam) {
                l2 <- length(exporting[[k]][[j]])
                if (l2 < l[j]) {
                    exporting[[k]][[j]] <- c(exporting[[k]][[j]], rep(NA, (l[j] - l2)))
                }
            }
        }
        samall <- samall[-(lsam + 1)]
        esam <- rep(samall, l)
        samorder <- moi_labels(set_d[,1])[[1]]
        if (keepmtd == TRUE && length(dupmtd) > 0) {
            mtdall <- mtdall[-dupmtd]
            tempmtd <- withWarnings(moi_metadata(tempmtd, mtdall, length(mtdall), samorder, samall, lsam, 0, FALSE, ""))
            metadata_warnings <- tempmtd[[2]]
            tempmtd <- tempmtd[[1]][[1]]
            metadata <- matrix(0, length(esam), ncol(tempmtd))
            for (i in 1:ncol(metadata)) {
                metadata[, i] <- rep(as.vector(tempmtd[, i]), l)
            }
        }
        if (keepmtd == TRUE ) {
            pdata <- matrix(as.character(unlist(exporting)), sum(l), nc - nummtd + 1)
            final <- cbind(esam, metadata, pdata)
            colnames(final) <- c("Sample IDs", mtdall, markerlabels)
        }
        else {
            pdata <- matrix(as.character(unlist(exporting)), sum(l), nc - nummtd + 1)
            final <- cbind(esam, pdata)
            colnames(final) <- c("Sample IDs", markerlabels)
        }
        "Storing warnings"
        allwarnings <- metadata_warnings
        if (is.null(keepwarnings) == FALSE) {
            if (length(allwarnings) == 0) {
                warning("There are no warnings!", call. = FALSE)
            }
            else {
                warndic <- moi_warning(keepwarnings, list(), metadata_warnings, list(), markerlabels)
                message(paste("The warnings are in this directory: ", normalizePath(warndic, winslash = "/")))
            }
        }
        else {
            if (length(allwarnings) == 0) {
                warning("There are no warnings!", call. = FALSE)
            }
            else if (length(allwarnings) > 49) {
                allwarnings <- unlist(allwarnings)
                for (k in 1:49) {
                    warning(allwarnings[k], call. = FALSE)
                }
                warning("There were more than 50 warnings. The argument 'keepwarnings' needs to be set to see all the warnings."
                        , call. = FALSE)
            }
            else {
                allwarnings <- unlist(allwarnings)
                for (k in 1:length(allwarnings)) {
                    warning(allwarnings[k], call. = FALSE)
                }
            }
        }
        rownames(final) <- c(1:nrow(final))
        if (is.null(export) == FALSE) {
            moi_export(export, final)
        }
        final
    }



