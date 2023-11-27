#' Reports and deletes empty rows/columns
#'
#' @description Reports and deletes empty rows/columns.
#'
#' @param set_d data frame; imported dataset.
#' @param nummtd numeric; number of metadata columns plus 2.
#' @param setnoempty data frame; imported dataset.
#' @param rw_col string vector; variable used to switch
#'   between row and column in case of transposed data.
#'   Namely, \code{c("rows ", "row ", "column ", "columns
#'   ")}.
#' @param multsheets logical; indicating whether data is
#'   contained in a single or multiple worksheets. The
#'   default value is \code{ multsheets = FALSE},
#'   corresponding to data contained in a single worksheet.
#' @param alllabels all the marker labels.
#' @param molecular molecular argument.
#' @param molid identifier. It is greater than zero when
#'   molecular argument is just a single value.
#'
#' @return A list of 4 elements: 1) dataset without empty
#'   rows/columns; 2) dataset with empty rows/columns; 3)
#'   coorected labels; 4)corrected number of metadata
#'   columns.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{moimport}.
#'
#'
moi_empty <-
    function (set_d, setnoempty, nummtd, rw_col, multsheets, alllabels, molecular, molid, n)
    {
        out <- list()
        set_dd <- as.matrix(set_d)
        w <- v <- 0
        for (j in 1:nrow(set_dd)) {
            if (prod(is.element(set_dd[j, ], NA)) == 1) {
                w <- append(w, j)
            }
        }
        for (k in 1:ncol(set_dd)) {
            if (prod(is.element(set_dd[, k], NA)) == 1) {
                v <- append(v, k)
            }
        }
        w <- w[-1]
        v <- v[-1]
        wv <- set_dd[-w,]
        if (multsheets == 0) {
            if (length(w) > 0) {
                rc <- rw_col[2]
                verb <- "is"
                if(length(w) > 1){
                    rc <- rw_col[1]
                    verb <- "are"
                }
                warning("The following empty ", rc, verb," deleted: ", paste(w + 1, collapse = ", "), ".",
                        call. = FALSE, noBreaks. = TRUE)
                setnoempty <- wv
            }
            if (length(v) > 0) {
                rc <- rw_col[3]
                verb <- "is"
                if(length(v) > 1){
                    rc <- rw_col[4]
                    verb <- "are"
                }
                warning("The following empty ", rc, verb, " deleted: ", paste(v, collapse = " and "), ".",
                        call. = FALSE, noBreaks. = TRUE)
                alllabels <- alllabels[-(v - 1)]
                set_d <- set_d[,-v]
                nummtd <- nummtd - sum((v < nummtd)*1)
                if (length(alllabels) != length(molecular) + nummtd - 2 && molid == 0) {
                    warning("The arguments 'coding' and 'molecular' need to be set only for non-empty columns.",
                            call. = FALSE)
                }
            }
        }
        else {
            if (length(w) > 0) {
                rc <- rw_col[2]
                verb <- "is"
                if(length(w) > 1){
                    rc <- rw_col[1]
                    verb <- "are"
                }
                warning("In worksheet ", n, " the following empty ", rc, verb, " deleted: ", paste(w + 1, collapse = ", "), ".",
                        call. = FALSE, noBreaks. = TRUE)
                setnoempty <- wv
            }
            if (length(v) > 0) {
                rc <- rw_col[3]
                verb <- "is"
                if(length(v) > 1){
                    rc <- rw_col[4]
                    verb <- "are"
                }
                warning("In worksheet ", n, " the following empty ", rc, verb," deleted: ", paste(v, collapse = " and "), ".",
                        call. = FALSE, noBreaks. = TRUE)
                alllabels <- alllabels[-(v - 1)]
                set_d <- set_d[,-v]
                nummtd <- nummtd - sum((v < nummtd)*1)
            }
        }
        list(set_d, setnoempty, alllabels, nummtd)
    }



#############################################################################################
#############################################################################################


#' Finds sample IDs contained in a dataset
#'
#' @description Each dataset consists of several number of
#'   samples. Samples are specified by their sample ID which
#'   are placed in the first column of the excel worksheet.
#'   The function \code{moi_labels}, finds sample IDs and
#'   the row in which they start.
#'
#' @param s_total sample IDs.
#'
#' @return a list which the first element is a numeric
#'   vector specifying the number of rows in which a new
#'   sample starts. The second element is the name of sample
#'   IDs.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{moimport}
#'   and \code{\link{moi_duplicatefinder}}.
#'
moi_labels <-
    function (s_total)
    {
        sam <- which(is.na(s_total) == FALSE & duplicated(s_total) == FALSE)
        sam[length(sam) + 1] <- length(s_total) + 1
        s_set <- s_total[sam]
        list(sam, s_set)
    }



#############################################################################################
#############################################################################################


#' Administrator function
#'
#' @param set_d data frame; imported dataset.
#' @param s_total string vector; vector of sample IDs.
#' @param m_total string vector;vector of marker labels.
#' @param nummtd numeric; number of metadata columns plus 2.
#' @param cha string vector; vector of punctuation
#'   characters. See \code{\link{moi_prerequisite}}.
#' @param rw_col string vector; variable used to switch
#'   between row and column in case of transposed data.
#'   Namely, \code{c("rows ", "row ", "column ", "columns
#'   ")}.
#' @param nwsh numeric; worksheet number in multiple
#'   worksheet dataset.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a list of following elements: 1) rows in which
#'   new samples start, 2) all sample IDs in the worksheet,
#'   3) number of samples in the worksheet, 3) multiple row
#'   per sample identifier, 4) another multiple row per
#'   sample identifier, 5) columns in which new markers
#'   start, 6) all marker labels in the worksheet, 7) number
#'   of markers in the worksheet, 8) multiple column per
#'   marker identifier, 9) another multiple column per
#'   marker identifier, 10) an identifier whose value is 1
#'   if a warning takes place.
#' @keywords internal
#'
#' @seealso For further details see: \code{moimport}.
#'
moi_administrator <-
    function(set_d, s_total, m_total, nummtd, cha, rw_col, nwsh, transposed, multsh)
    {
        warnid <- 0
        sam <- which(is.na(s_total) == FALSE & duplicated(s_total) == FALSE)
        sam[length(sam) + 1] <- length(s_total) + 1
        samall <- s_total[sam]
        lsam <- length(sam) - 1
        s <- sam[-1] - sam[-(lsam + 1)]
        cons <- length(which(s > 1))
        mark <- which(is.na(m_total) == FALSE & duplicated(m_total) == FALSE)
        mark[length(mark) + 1] <- length(m_total) + 1
        mark_name <- m_total[mark]
        lmark <- length(mark) - 1
        m <- mark[-1] - mark[-(lmark + 1)]
        conm <- length(which(m > 1))
        warnsample <- "sample(s) "
        warnmarker <- "marker(s) "
        if (conm >= 1 && cons >= 1) {
            if (transposed == TRUE) {
                warnsample <- "marker(s) "
                warnmarker <- "sample(s) "
            }
            if (conm <= cons) {
                m <- append(m, 1)
                message("While data is of format multiple ", rw_col[1], "per sample, ", warnmarker, paste(shQuote(mark_name[m > 1], 'sh'), collapse = " and "), " use multiple ", rw_col[4],"to enter their information.")
            }
            else if (conm > cons) {
                message("While data is of format multiple ", rw_col[1], "per marker, ", warnsample, paste(shQuote(samall[s > 1], 'sh'), collapse = " and "), " use multiple ", rw_col[4],"to enter their information.")
            }
            warnid <- 1
        }
        else if (conm == 0 && cons == 0) {
            moi_separator(set_d, nummtd, cha, rw_col, nwsh, multsh)
        }
        list(sam, samall, lsam, s, cons, mark, mark_name, lmark, m, conm, warnid)
    }




#############################################################################################
#############################################################################################


#' Finds the most frequent separator
#'
#' @description This function is activated when the data is
#'   of 'One row per sample, one column per marker' format.
#'   In such a dataset, the user enters multiple information
#'   corresponding to a sample on a marker in one cell and
#'   separates the items with a symbol (punctuation
#'   character). It is expected that user be consistent with
#'   usage of symbol. Otherwise this function addresses the
#'   inconsistencies with a warning.
#'
#' @inheritParams moi_empty
#' @param cha string vector; vector of punctuation
#'   characters. See \code{\link{moi_prerequisite}}.
#' @param rw_col string vector; variable used to switch
#'   between row and column in case of transposed data.
#'   Namely, \code{c("rows ", "row ", "column ", "columns
#'   ")}.
#' @param nwsh numeric; worksheet number in multiple
#'   worksheet dataset.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a warning is generated reporting inconsistencies
#'   in usage of separator.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{moimport}.
#'
moi_separator <-
    function (set_d, nummtd, cha, rw_col, nwsh, multsh)
    {
        set_d <- as.matrix(set_d)
        c_l <- list()
        k <- 0
        for (i in nummtd:ncol(set_d)) {
            for (j in 1:nrow(set_d)) {
                a_l <- as.character(set_d[j, i])
                b_l <- match(unlist(strsplit(a_l, "")), cha)
                d_l <- is.na(b_l)*1
                dll <- length(d_l)
                if (dll > 1 && sum(d_l) > 0) {
                    if(prod(d_l) == 0) {
                        endcha <- c(d_l[-1] + d_l[-dll], d_l[dll])
                        startcha <- c(d_l[1], d_l[-1] + d_l[-dll])
                        endll <- which(endcha == 0)
                        startll <- which(startcha == 0)
                        chall <- unique(c(endll,startll))
                        if (length(chall) > 0) {
                            bll <- b_l[-chall]
                        }
                        else {
                            bll <- b_l
                        }
                        ch_l <- cha[bll[is.na(bll) == FALSE]]

                    }
                    else {
                        ch_l <- cha[b_l[is.na(b_l) == FALSE]]
                    }
                    if (length(ch_l) > 0) {
                        k <- k + 1
                        c_l[[k]] <- c(ch_l, j, i)
                    }
                }
            }
        }
        h_l <- unlist(lapply(c_l, function(x) x[1:(length(x) - 2)]))
        st_l <- sort(table(h_l), decreasing = TRUE)
        nst_l <- names(st_l)
        lst_l <- length(st_l)
        if (length(st_l) > 1) {
            ma_l <- lapply(c_l, function(x) {nst_l[2:lst_l] %in% x})
            ma_l <- which(sapply(ma_l, function(x) { sum(x * 1) > 0}) == TRUE)
            f_l <- t(sapply(c_l[ma_l], function(x) x[c(length(x) - 1, length(x))]))
            f_l[, 1] <- as.numeric(f_l[, 1]) + 1
            if (rw_col[1] == "columns"){
                f_l <- t(f_l)
            }
            warning(multsh, " While the most frequent separator is ", paste(shQuote(names(st_l[1]), "sh"), sep = ""), " which is used ", st_l[1], " times, separators (or possibly typos) ", paste(shQuote(nst_l[2:lst_l], "sh"), collapse = " and "), " are in cells ", paste(f_l[, 1], f_l[, 2], sep = "x", collapse = ", "), ".",
                    call. = FALSE, noBreaks. = TRUE)
        }
    }




#############################################################################################
#############################################################################################



#' Finds forbidden sample ID repetitions
#'
#' @description Sample IDs need to be uniquely assigned to
#'   samples. This function checks if a sample ID is
#'   assigned to two or more different samples. Similarly,
#'   the marker labels need to be uniquely defined. This
#'   function is also used to check forbidden marker label
#'   repetitions.
#'
#' @param total string vector; vector of sample IDs.
#' @param sam_mark string; a string which is either "Sample
#'   ID" or "Marker".
#' @param nummtd numeric; number of metadata columns plus 2.
#' @param rw_col string vector; variable used to switch
#'   between row and column in case of transposed data.
#'   Namely, \code{c("rows ", "row ", "column ", "columns
#'   ")}.
#'
#' @return a warning which informs the user of forbidden
#'   repetition of sample ID's or marker labels.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{moimport}
#'   \code{\link{moi_labels}}.
#'
moi_duplicatefinder <-
    function (total, sam_mark, nummtd, rw_col)
    {
        if (is.element(NA, total) == TRUE) {
            whichna <- which(is.na(total) == TRUE)
            consecutivena <- c(2, whichna[-1] - whichna[-length(whichna)])
            samna <- total[whichna[consecutivena > 1] - 1]
            notna <- which(consecutivena > 1)
            repna <- c(notna, length(consecutivena) + 1)
            repna <- repna[-1] - repna[-length(repna)]
            total[whichna] <- rep(samna, repna)
            total[whichna] <- total[whichna - 1]
        }
        dup <- total[duplicated(total)]
        "duplicated samples"
        l <- 0
        if (length(dup) != 0) {
            dup <- dup[!is.na(dup)]
            if (length(dup) != 0) {
                dup_org <- match(total, dup)
                "each duplicated sample and its original are numbered"
                dup_num <- unique(dup_org)
                "number assigned to duplicated/original "
                dup_num <- dup_num[is.na(dup_num) == FALSE]
                for (j in 1:length(dup_num)) {
                    dup_j <- which(dup_org == dup_num[j])
                    "where duplicated samples lie"
                    l_dupj <- dup_j[-1] - rev(rev(dup_j)[-1])
                    dupsam <- which(l_dupj > 1)
                    "samples in between two duplication"
                    if (length(dupsam) >= 1) {
                        warning(sam_mark, dup[dup_num[j]], " in ", rw_col[2], dup_j[1] + 1 + nummtd, " is used also in ", rw_col[1], paste(c(dup_j[dupsam + 1] + 1 + nummtd), collapse = " and "), ".",
                                call. = FALSE, noBreaks. = TRUE)
                        l <- 1
                    }
                }
            }
        }
        l
    }




#############################################################################################
#############################################################################################


#' Checks the metadata entries
#'
#' @description Checks if a sample has unique metadata
#'   entries.
#'
#' @param metadata matrix; matrix of metadata columns.
#' @param mtdlabels string vector; vector of metadata
#'   labels.
#' @param nummtd numeric; number of metadata columns plus 2.
#' @param samorder numeric vector; rows where samples start.
#' @param samall string vector; vector of sample IDs in the
#'   worksheet.
#' @param lsam numeric; number of samples in the worksheet.
#' @param nomtdneeded numeric vector; samples which need no
#'   metadata.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a list of following elements: 1) unique metadata
#'   for different samples, 2) an identifier whose value is
#'   1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{moimport}.
#'
moi_metadata <-
    function(metadata, mtdlabels, nummtd, samorder, samall, lsam, nomtdneeded, nomerge, multsh)
    {

        warind <- 0
        lrow <- samorder[lsam + 1] - 1
        mdsam <- matrix(c(rep(c(1:nummtd), lrow)), lrow, nummtd, byrow = TRUE)
        mdcol <- rep(1:lsam, (samorder[-1] - samorder[-(lsam + 1)]))
        meta <- split(metadata, mdsam)

        meta <- lapply(meta, function(meta) split(meta, mdcol))
        meta <- lapply(meta, function(x) lapply(x, function(x) x[!is.na(x)]))
        meta <- lapply(meta, function(x) lapply(x, unique))
        lmeta <- lapply(meta, function(x) lapply(x, length) )
        lmeta <- matrix(unlist(lmeta), lsam, nummtd)
        extramtd <- which(lmeta == 2, arr.ind = TRUE)
        lessmtd <- which(lmeta == 0, arr.ind = TRUE)
        lextra <- nrow(extramtd)
        word <- "sample is"
        if (lextra > 0) {
            if (lextra > 1) {
                word <- "samples are"
            }
            warning(multsh, " Metadata of the following ", word, " not unique:\n ", paste(shQuote(mtdlabels[extramtd[,2]], "sh"), shQuote(samall[extramtd[,1]], "sh"), sep = " of sample ", collapse = ". "), ".",
                    call. = FALSE)
            warind <- 1
        }
        needmtd <- match(lessmtd[,1], nomtdneeded)
        lessmtd <- lessmtd[is.na(needmtd), ]
        if (is.null(dim(lessmtd)) == TRUE) {
            lessmtd <- t(as.matrix(lessmtd))
        }
        lless <- nrow(lessmtd)
        word <- "sample is"
        if (lless > 0 && nomerge == TRUE) {
            if (lless > 1) {
                word <- "samples are"
            }
            warning(multsh, " Metadata of the following ", word, " not entered:\n ", paste(shQuote(mtdlabels[lessmtd[,2]], "sh"), shQuote(samall[lessmtd[,1]], "sh"), sep = " of sample ", collapse = ". "), ".",
                    call. = FALSE)
            warind <- 1
        }
        meta <- lapply(meta, function(x) lapply(x, function(x) replace(x, length(x) == 0, NA)))
        meta <- lapply(meta, function(x) lapply(x, '[[', 1))
        meta <- matrix(unlist(meta), lsam, nummtd)
        list(meta, warind)
    }




#############################################################################################
#############################################################################################


#' Merges metadata
#'
#' @param tempmtd matrix; matrix of temporary metadata.
#' @param mtdall string vector; vector of all metadata
#'   labels.
#' @param samall string vector; vector of all sample IDs in
#'   the worksheet.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return list of following elements: 1) unique metadata
#'   labels, 2) unique metadata for different samples.
#'
#' @keywords  internal
#'
#' @seealso For further details see: \code{moimport}.
moi_mergemetadata <-
    function (tempmtd, mtdall, samall, multsh) {
        if (is.null(ncol(tempmtd)) == TRUE) {
            tempmtd <- matrix(tempmtd, length(tempmtd), 1)
        }

        lmtd <- nrow(tempmtd)
        mtdall <- unlist(mtdall)
        mtdlabels <- unique(mtdall)
        mtdmatch <- match(mtdall, mtdlabels)
        colnames(tempmtd) <- mtdall
        warnid <- 0
        for (i in 1:length(mtdlabels)) {
            tempmtdmatch <- which(mtdmatch == i)
            mtd <- list()
            if (length(tempmtdmatch) > 1) {
                tempmetadata <- tempmtd[,tempmtdmatch]
                mtdlabelfixed <- colnames(tempmtd)[tempmtdmatch[1]]
                mtd <- lapply(1:lmtd, function(i) mtd[[i]] <- tempmetadata[i,][!is.na(tempmetadata[i,])])
                mtd <- lapply(1:lmtd, function(i) mtd[[i]] <- unique(mtd[[i]]))
                mtdlength <- lengths(mtd)
                notuniquemtd <- which(mtdlength > 1)
                lnotunimtd <- length(notuniquemtd)

                if (lnotunimtd > 0) {
                    words <- c("sample", " is")
                    if (lnotunimtd > 1) {
                        words <- c("samples", " are")
                    }
                    warning(multsh, " Metadata of following ", words[1], " across worksheets ", words[2], " not unique: ", shQuote(mtdlabels[i], "sh"), " of sample ", paste(shQuote(samall[notuniquemtd], "sh"), collapse = ", "),
                            call. = FALSE)
                    warnid <- 1
                }
                mtd <- lapply(mtd, function(x) if(identical(x, character(0))) NA_character_ else x)
                mtd <- as.matrix(unlist(lapply(mtd, '[[', 1)))
                mtdmatch <- mtdmatch[-tempmtdmatch]
                tempmtd <- tempmtd[,-tempmtdmatch]
                tempmtd <- cbind(tempmtd, mtd)

                colnames(tempmtd)[ncol(tempmtd)] <- mtdlabelfixed

            }
        }
        mtdlabels <- colnames(tempmtd)
        metadata <- tempmtd
        list(mtdlabels, metadata, warnid)
    }




#############################################################################################
#############################################################################################


#' Exports the dataset in standard format to a new excel
#' file.
#'
#' @description This function exports the modified dataset
#'   in standard format to a new excel file.
#'
#' @param export string; the path where the imported data is
#'   stored in standardized format.
#' @param final data frame; modified dataset in standard
#'   format.
#'
#' @return An excel file which contains dataset in standard
#'   format.
#'
#' @keywords internal
#'
#' @seealso For further details, please see the following
#'   functions: \code{moimport}.
#'
moi_export <-
    function (export, final)
    {
        filename <- basename(export)
        w_dir <- sub(filename, "", export)
        fname <- strsplit(filename, ".", fixed = TRUE)[[1]][1]
        u <- 0
        if (paste(unlist(strsplit(filename, ".", fixed = TRUE)), collapse = "") == filename) {
            export <- paste(export, ".xlsx", sep = "")
        }
        while (file.exists(export) == TRUE) {
            u <- u + 1
            export <- paste(w_dir, fname, u, ".xlsx", sep = "")
        }
        #KS# XLConnect::writeWorksheetToFile(export, final, sheet = paste("Out", sep = ""))

        output <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(output, "Out")
        openxlsx::writeData(output, sheet ="Out",final)
        openxlsx::saveWorkbook(output,export, overwrite = TRUE)


        #openxlsx::write.xlsx(final, export, sheetName = paste("Out", sep = ""))
        export
    }

#############################################################################################
#############################################################################################
#' Writes warnings into a file
#'
#' @description This function exports the modified
#' dataset in standard format to a new excel file.
#'
#' @param keepwarnings string; the path where the
#' warnings are stored.
#' @param general_warnings list; general warnings.
#' @param metadata_warnings list; metadata warnings.
#' @param marker_warnings list; marker warnings.
#' @param markerlabels string vector; marker labels.
#'
#' @return An excel file which contains warnings.
#' Each worksheet corresponds to a marker
#'
#' @keywords internal
#'
#' @seealso For further details, please see the following
#'   functions: \code{moimport}.
#'
moi_warning <-
    function (keepwarnings, general_warnings, metadata_warnings, marker_warnings, markerlabels)
    {
        filename <- basename(keepwarnings)
        w_dir <- sub(filename, "", keepwarnings)
        fname <- strsplit(filename, ".", fixed = TRUE)[[1]][1]
        u <- 0
        if (paste(unlist(strsplit(filename, ".", fixed = TRUE)), collapse = "") == filename) {
            keepwarnings <- paste(keepwarnings, ".xlsx", sep = "")
        }
        while (file.exists(keepwarnings) == TRUE) {
            u <- u + 1
            keepwarnings <- paste(w_dir, fname, u, ".xlsx", sep = "")
        }
        #KS# wwarnings <- XLConnect::loadWorkbook(keepwarnings, create = TRUE)
        #wwarnings <- openxlsx::loadWorkbook(keepwarnings, create = TRUE)

        if(file.exists(keepwarnings)){
            wwarnings <- openxlsx::loadWorkbook(keepwarnings)
        }else{
            wwarnings <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wwarnings, "general")
            openxlsx::saveWorkbook(wwarnings,keepwarnings, overwrite = TRUE)
        }
        if (length(general_warnings)  > 0) {
            gwarning <- data.frame(unlist(general_warnings))
            colnames(gwarning) <- ""
            #KS# XLConnect::createSheet(wwarnings, "general")
            #KS# XLConnect::setColumnWidth(wwarnings, sheet = "general", 1, width = 4000)
            #KS# XLConnect::writeWorksheet(wwarnings, gwarning, sheet = paste("general", sep = ""))
            openxlsx::addWorksheet(wwarnings, "general")
            openxlsx::setColWidths(wwarnings,"general", 1,widths = 160)
            openxlsx::writeData(wwarnings, sheet ="general",gwarning)
        }
        if (length(metadata_warnings) > 0) {
            mtdwarning <- data.frame(unlist(metadata_warnings))
            colnames(mtdwarning) <- ""
            #XLConnect::createSheet(wwarnings, "metadata")
            #XLConnect::setColumnWidth(wwarnings, "metadata", 1, width = 4000)
            #XLConnect::writeWorksheet(wwarnings, mtdwarning, sheet = paste("metadata", sep = ""))

            openxlsx::addWorksheet(wwarnings, "metadata")
            openxlsx::setColWidths(wwarnings,"metadata", 1,widths = 160)
            openxlsx::writeData(wwarnings, sheet ="metadata",mtdwarning)
        }
        if (length(marker_warnings) != 0) {
            for (i in 1:length(marker_warnings)) {
                if (length(marker_warnings[i]) > 0 && is.null(marker_warnings[[i]]) == FALSE) {
                    mkwarning <- data.frame(unlist(marker_warnings[[i]]))
                    colnames(mkwarning) <- ""
                    #KS# XLConnect::createSheet(wwarnings, markerlabels[i])
                    #KS# XLConnect::setColumnWidth(wwarnings, markerlabels[i], 1, width = 4000)
                    #KS# XLConnect::writeWorksheet(wwarnings, mkwarning, sheet = paste(markerlabels[i], sep = ""))


                    openxlsx::addWorksheet(wwarnings, markerlabels[i])
                    openxlsx::setColWidths(wwarnings,markerlabels[i], 1,widths = 160)
                    openxlsx::writeData(wwarnings, sheet = markerlabels[i], mkwarning)

                }

            }
        }
        #KS# XLConnect::saveWorkbook(wwarnings, keepwarnings)
        openxlsx::saveWorkbook(wwarnings,keepwarnings, overwrite = TRUE)
        keepwarnings
    }


#############################################################################################
#############################################################################################

#' Captures all the warnings
#'
#' @param expr expression to be evaluated.
#'
#' @return list with two elements. First is the value of the
#'   function. Second is the generated functions by the
#'   expression.
#' @keywords internal
#'
#' @seealso For further details, please see the following
#'   functions: \code{moimport}.
#'
withWarnings <- function(expr) {
    myWarnings <- NULL
    wHandler <- function(w) {
        myWarnings <<- c(myWarnings, list(w))
        invokeRestart("muffleWarning")
    }
    val <- withCallingHandlers(expr, warning = wHandler)
    list(value = val, warnings = myWarnings)
}


#############################################################################################
#############################################################################################

#' Contains different character vectors needed for importing
#' molecular dataset
#'
#'
#' @keywords internal
#'
moi_prerequisite <-
  function()
  {
    cha <- c(",", "/", "\\", "-", ":", ";", "'", "|", "+", "*", "`",
             "#", "&", "^", "_", "?", "!", "%", "]", "[", "(", ")",
             "~", ">", "<", "=", "$", "@", "{", "}", " ")
    cha_num <- c(",", "/", "\\", "-", ":", ";", "'", "|", "+", "*", "`",
                 "#", "&", "^", "_", "?", "!", "%", "]", "[", "(", ")",
                 "~", ">", "<", "=", "$", "@", "{", "}", " ",
                 LETTERS, tolower(LETTERS))
    cha_string <- c(",", "/", "\\", "-", ":", ";", "'", "|", "+", "*", "`",
                    "#", "&", "^", "_", "?", "!", "%", "]", "[", "(", ")",
                    "~", ">", "<", "=", "$", "@", "{", "}", " ", ".",
                    as.character(c(0:9)))
    ambeguity_code <- c("A", "C", "G", "T", "W", "S", "M", "K",
                        "R", "Y", "B", "D", "H", "V", "N")
    represented_bases <- list("A", "C", "G", "T", c("A", "T"),
                              c("C", "G"), c("A", "C"), c("G", "T"), c("A", "G"),
                              c("C", "T"), c("C", "G", "T"), c("A", "G", "T"),
                              c("A", "C", "T"), c("A", "C", "G"), c("A", "C", "G", "T"),
                              NA, NA)
    aa_1 <- c("A", "ALA", "ALANINE", "C", "CYS", "CYSTEINE",
              "D", "ASP", "ASPARTIC", "E", "GLU", "GLUTAMIC", "F",
              "PHE", "PHENYLALANINE", "G", "GLY", "GLYCINE", "H", "HIS",
              "HISTIDINE", "I", "ILE", "ISOLEUCINE", "K", "LYS", "LYSINE",
              "L", "LEU", "LEUCINE", "M", "MET", "METHIONINE", "N",
              "ASN", "ASPARAGINE", "P", "PRO", "PROLINE", "Q", "GLN",
              "GLUTAMINE", "R", "ARG", "ARGININE", "S", "SER", "SERINE",
              "T", "THR", "THREONINE", "V", "VAL", "VALINE", "W", "TRP",
              "TRYPTOPHAN", "Y", "TYR", "TYROSINE")
    aa_2 <- c("GCT", "GCC", "GCA", "GCG", "GCN", "TGT", "TGC",
              "TGY", "GAT", "GAC", "GAY", "GAA", "GAG", "GAR", "TTT",
              "TTC", "TTY", "GGT", "GGC", "GGA", "GGG", "GGN", "CAT",
              "CAC", "CAY", "ATT", "ATC", "ATA", "ATH", "AAA", "AAG",
              "AAR", "TTA", "TTG", "CTT", "CTC", "CTA", "CTG", "YTR",
              "CTN", "ATG", "AAT", "AAC", "AAY", "CCT", "CCC", "CCA",
              "CCG", "CCN", "CAA", "CAG", "CAR", "CGT", "CGC", "CGA",
              "CGG", "AGA", "AGG", "CGN", "MGR", "TCT", "TCC", "TCA",
              "TCG", "AGT", "AGC", "TCN", "AGY", "ACT", "ACC", "ACA",
              "ACG", "ACN", "GTT", "GTC", "GTA", "GTG", "GTN", "TGG",
              "TAT", "TAC", "TAY")
    let_3 <- c("ALA", "CYS", "ASP", "GLU", "PHE", "GLY", "HIS",
               "ILE", "LYS", "LEU", "MET", "ASN", "PRO", "GLN", "ARG",
               "SER", "THR", "VAL", "TRP", "TYR")
    amino_acid <- c("ALANINE", "CYSTEINE", "ASPARTIC", "GLUTAMIC",
                    "PHENYLALANINE", "GLYCINE", "HISTIDINE", "ISOLEUCINE",
                    "LYSINE", "LEUCINE", "METHIONINE", "ASPARAGINE", "PROLINE",
                    "GLUTAMINE", "ARGININE", "SERINE", "THREONINE", "VALINE",
                    "TRYPTOPHAN", "TYROSINE")
    aa_symbol <- c("A", "C", "D", "E", "F", "G", "H", "I", "K",
                   "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")
    compact <- c("GCN", "TGY", "GAY", "GAR", "TTY", "GGN",
                 "CAY", "ATH", "AAR", "YTR", "CTN", "AAY", "CCN", "CAR",
                 "CGN", "MGR", "TCN", "AGY", "ACN", "GTN", "TAY")
    codon_s <- list(c("GCT", "GCC", "GCA", "GCG"), c("TGT", "TGC"),
                    c("GAT", "GAC"), c("GAA", "GAG"), c("TTT", "TTC"), c("GGT",
                    "GGC", "GGA", "GGG"), c("CAT", "CAC"), c("ATT", "ATC","ATA"),
                    c("AAA", "AAG"), c("CTA", "TTG"), c("CTT", "CTC", "CTA", "CTG"),
                    c("AAT", "AAC"), c("CCT", "CCC", "CCA", "CCG"), c("CAA", "CAG"),
                    c("CGT", "CGC", "CGA", "CGG"), c("AGA", "AGG"), c("TCT", "TCC", "TCA", "TCG"),
                    c("AGT", "AGC"), c("ACT", "ACC", "ACA", "ACG"), c("GTT", "GTC", "GTA", "GTG"),
                    c("TAT", "TAC"))
    list(cha, cha_num, cha_string, ambeguity_code, represented_bases,
         aa_1, aa_2, let_3, amino_acid, aa_symbol, compact, codon_s)
  }


