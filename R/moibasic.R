#' Reports and deletes empty rows/columns
#'
#' @description Reports and deletes empty rows/columns.
#'
#' @param set_d data frame; imported dataset.
#' @param nummtd numeric; number of metadata coumns plus 2.
#'
#' @return A list of three elements: 1) list of empty rows,
#'   2) list of empty columns, 3) contains the dataset with
#'   deleted empty rows/columns.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}}.
#'
#'
moi_empty <-
    function (set_d, nummtd)
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
        out[[1]] <- w
        out[[2]] <- v
        out[[3]] <- set_dd[-w,]
        out
    }





#' Finds sample IDs contained in a dataset
#'
#' @description Each dataset consists of several number of
#'   samples. Samples are specified by their sample ID which
#'   are placed in the first column of the excel worksheet.
#'   The function \code{moi_labels}, finds sample IDs and
#'   the row in which they start.
#'
#' @inheritParams moi_empty
#' @param multsheets logical; indicating whether data is
#'   contained in a single or multiple worksheets. The
#'   default value is \code{ multsheets = F}, corresponding
#'   to data contained in a single worksheet.
#'
#' @return a list which the first element is a numeric
#'   vector specifying the number of rows in which a new
#'   sample starts. The second element is the name of sample
#'   IDs.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}}
#'   and \code{\link{moi_duplicatefinder}}.
#'
moi_labels <-
    function (s_total)
    {
        "rows in which samples start"
        sam <- which(is.na(s_total) == F & duplicated(s_total) == F)
        sam[length(sam) + 1] <- length(s_total) + 1
        "sample names"
        s_set <- s_total[sam]
        list(sam, s_set)
    }





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
#' @seealso For further details see: \code{\link{moimport}}.
#'
moi_administrator <-
    function(set_d, s_total, m_total, nummtd, cha, rw_col, nwsh)
    {
        warnid <- 0
        sam <- which(is.na(s_total) == F & duplicated(s_total) == F)
        sam[length(sam) + 1] <- length(s_total) + 1
        samall <- s_total[sam]
        lsam <- length(sam) - 1
        s <- sam[-1] - sam[-(lsam + 1)]
        cons <- length(which(s > 1))
        mark <- which(is.na(m_total) == F & duplicated(m_total) == F)
        mark[length(mark) + 1] <- length(m_total) + 1
        mark_name <- m_total[mark]
        lmark <- length(mark) - 1
        m <- mark[-1] - mark[-(lmark + 1)]
        conm <- length(which(m > 1))
        if (conm >= 1 && cons >= 1) {
            if (conm <= cons) {
                warning("While data is of format multiple ", rw_col[1]," per sample format, marker(s) ",
                        paste(shQuote(rev(rev(mark_name[mark[m > 1]])), 'sh'), collapse = " and "),
                        " use multiple ", rw_col[4],"to enter their information."
                        , call. = F)
            }
            else if (conm > cons) {
                warning("While data is of format multiple ", rw_col[1]," per marker format, samples(s) ",
                        paste(rev(rev(samall[sam[s > 1]])[-1]), collapse = " and "),
                        " use multiple ", rw_col[4]," to enter their information.",
                        call. = F)
            }
            warnid <- 1
        }
        else if (conm == 0 && cons == 0) {
            moi_separator(set_d, nummtd, cha, rw_col, nwsh)
        }
        list(sam, samall, lsam, s, cons, mark, mark_name, lmark, m, conm, warnid)
    }




#' Finds the most frequent sparator
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
#'
#' @return a warning is generated reporting inconsistencies
#'   in usage of separator.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}}.
#'
moi_separator <-
    function (set_d, nummtd, cha, rw_col, nwsh)
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
                        ch_l <- cha[bll[is.na(bll) == F]]

                    }
                    else {
                        ch_l <- cha[b_l[is.na(b_l) == F]]
                    }
                    if (length(ch_l) > 0) {
                        k <- k + 1
                        c_l[[k]] <- c(ch_l, j, i)
                    }
                }
            }
        }
        h_l <- unlist(lapply(c_l, function(x) x[1:(length(x) - 2)]))
        st_l <- sort(table(h_l), decreasing = T)
        nst_l <- names(st_l)
        lst_l <- length(st_l)
        if (length(st_l) > 1) {
            ma_l <- lapply(c_l, function(x) {nst_l[2:lst_l] %in% x})
            ma_l <- which(sapply(ma_l, function(x) { sum(x * 1) > 0}) == T)
            f_l <- t(sapply(c_l[ma_l], function(x) x[c(length(x) - 1, length(x))]))
            f_l[, 1] <- as.numeric(f_l[, 1]) + 1
            if (rw_col[1] == "columns"){
                f_l <- t(f_l)
            }
            warning("While the most frequent separator is ", paste(shQuote(names(st_l[1]), "sh"), sep = ""),
                    " which is used ", st_l[1], " times, separators (or possibly typos) ",
                    paste(shQuote(nst_l[2:lst_l], "sh"), collapse = " and "), " are in cells ",
                    paste(f_l[, 1], f_l[, 2], sep = "x", collapse = ", "), ".",
                    call. = F, noBreaks. = T)
        }
    }





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
#'   repitition of sample ID's or marker labels.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}}
#'   \code{\link{moi_labels}}.
#'
moi_duplicatefinder <-
    function (total, sam_mark, nummtd, rw_col)
    {
        if (is.element(NA, total) == T) {
            whichna <- which(is.na(total) == T)
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
                dup_num <- dup_num[is.na(dup_num) == F]
                for (j in 1:length(dup_num)) {
                    dup_j <- which(dup_org == dup_num[j])
                    "where duplicated samples lie"
                    l_dupj <- dup_j[-1] - rev(rev(dup_j)[-1])
                    dupsam <- which(l_dupj > 1)
                    "samples in between two duplication"
                    if (length(dupsam) >= 1) {
                        warning(sam_mark, dup[dup_num[j]], " in ", rw_col[2],
                                dup_j[1] + 1 + nummtd, " is used also in ", rw_col[1],
                                paste(c(dup_j[dupsam + 1] + 1 + nummtd), collapse = " and "),
                                ".", call. = F, noBreaks. = T)
                        l <- 1
                    }
                }
            }
        }
        l
    }




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
#'
#' @return a list of following elements: 1) unique metadata
#'   for different samples, 2) an identifier whose value is
#'   1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}}.
#'
moi_metadata <-
    function(metadata, mtdlabels, nummtd, samorder, samall, lsam, nomtdneeded)
    {
        #lsam <- length(sam) - 1
        warind <- 0
        lrow <- samorder[lsam + 1] - 1
        mdsam <- matrix(c(rep(c(1:nummtd), lrow)), lrow, nummtd, byrow = T)
        mdcol <- rep(1:lsam, (samorder[-1] - samorder[-(lsam + 1)]))
        meta <- split(metadata, mdsam)
        meta <- lapply(meta, function(meta) split(meta, mdcol))
        meta <- lapply(meta, function(x) lapply(x, function(x) x[!is.na(x)]))
        meta <- lapply(meta, function(x) lapply(x, unique))
        lmeta <- lapply(meta, function(x) lapply(x, length) )
        lmeta <- matrix(unlist(lmeta), lsam, nummtd)
        extramtd <- which(lmeta == 2, arr.ind = T)
        lessmtd <- which(lmeta == 0, arr.ind = T)
        lextra <- nrow(extramtd)
        word <- "is"
        if (lextra > 0) {
            if (lextra > 1) {
                word <- " are"
            }
            warning(paste(shQuote(mtdlabels[extramtd[,2]], "sh"), shQuote(samall[extramtd[,1]], "sh"),
                          sep = " of sample ", collapse = " and "), word, " not unique.", call. = F)
            warind <- 1
        }
        lessmtd <- lessmtd[lessmtd[,1] != nomtdneeded, ]
        if (is.null(dim(lessmtd)) == T) {
            lessmtd <- t(as.matrix(lessmtd))
        }
        lless <- length(lessmtd)
        if (lless > 0) {
            if (lless > 1) {
                word <- " are"
            }
            warning(paste(shQuote(mtdlabels[lessmtd[,2]], "sh"), shQuote(samall[lessmtd[,1]], "sh"),
                          sep = " of sample ", collapse = " and "), word, " not entered.", call. = F)
            warind <- 1
        }
        meta <- lapply(meta, function(x) lapply(x, function(x) replace(x, length(x) == 0, NA)))
        meta <- lapply(meta, function(x) lapply(x, '[[', 1))
        meta <- matrix(unlist(meta), lsam, nummtd)
        list(meta, warind)
    }




#' Merges metadata
#'
#' @param tempmtd matrix; matrix of temporary metadata.
#' @param mtdall string vector; vector of all metadata
#'   labels.
#' @param samall string vector; vector of all sample IDs in
#'   the worksheet.
#'
#' @return list of following elements: 1) unique metadata
#'   labels, 2) unique metadata for different samples.
#'
#' @keywords  internal
#'
#' @seealso For further details see: \code{\link{moimport}}.
moi_mergemetadata <-
    function (tempmtd, mtdall, samall) {
        if (is.null(ncol(tempmtd)) == T) {
            tempmtd <- matrix(tempmtd, length(tempmtd), 1)
        }
        lmtd <- nrow(tempmtd)
        mtdall <- unlist(mtdall)
        mtdlabels <- unique(mtdall)
        mtdmatch <- match(mtdall, mtdlabels)
        colnames(tempmtd) <- mtdall
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
                    warning("Comparing the metadata across worksheets shows ",
                            shQuote(mtdlabels[i], "sh"), " of ", words[1],
                            paste(shQuote(samall[notuniquemtd], "sh"), collapse = " and "),
                            words[2], " not unique.", call. = F)
                }
                mtd <- as.matrix(unlist(lapply(mtd, '[[', 1)))
                mtdmatch <- mtdmatch[-tempmtdmatch]
                tempmtd <- tempmtd[,-tempmtdmatch]
                tempmtd <- cbind(tempmtd, mtd)
                colnames(tempmtd)[ncol(tempmtd)] <- mtdlabelfixed
            }
        }
        mtdlabels <- colnames(tempmtd)
        metadata <- tempmtd
        list(mtdlabels, metadata)
    }




#' Exports the dataset in standarad format to a new excel
#' file.
#'
#' @description This function exports the modified dataset
#'   in standard format to a new excel file.
#'
#' @param export export string; the path where the imported data is
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
#'   functions: \code{\link{moimport}}.
#'
moi_export <-
    function (export, final)
    {
        f_name <- utils::tail(strsplit(export, "/")[[1]], n = 1)
        " file name"
        w_dir <- sub(f_name, "", export)
        " working directory"
        f_name <- strsplit(f_name, ".", fixed = T)[[1]][1]
        f_new <- paste(w_dir, "/", f_name, "Out", ".xlsx",
                       sep = "")
        " new file to be exported"
        u <- 0
        while (file.exists(f_new) == T) {
            u <- u + 1
            f_new <- paste(w_dir, "/", f_name, "Out", u, ".xlsx",
                           sep = "")
        }
        XLConnect::writeWorksheetToFile(f_new, final, sheet = paste("Out", sep = ""))
    }




#' Contains different character vectors needed for importing
#' dataset
#'
#'
#' @keywords internal
#'
moi_prerequisite <-
  function()
  {
    cha <- c(",", "/", "\\", "-", ":", ";", "'", "|", "+", "*", "`",
             "#", "&", "^", "_", "?", "!", "%", "]", "[", "(", ")",
             "~", ">", "<", "=", "$", "@", "{", "}", "?", " ", "?")
    cha_num <- c(",", "/", "\\", "-", ":", ";", "'", "|", "+", "*", "`",
                 "#", "&", "^", "_", "?", "!", "%", "]", "[", "(", ")",
                 "~", ">", "<", "=", "$", "@", "{", "}", "?", " ", "?",
                 LETTERS, tolower(LETTERS))
    cha_string <- c(",", "/", "\\", "-", ":", ";", "'", "|", "+", "*", "`",
                    "#", "&", "^", "_", "?", "!", "%", "]", "[", "(", ")",
                    "~", ">", "<", "=", "$", "@", "{", "}", "?", " ", ".", "?",
                    as.character(c(0:9)))
    ambeguity_code <- c("A", "C", "G", "T", "W", "S", "M", "K",
                        "R", "Y", "B", "D", "H", "V", "N")
    represented_bases <- list("A", "C", "G", "T", c("A", "T"),
                              c("C", "G"), c("A", "C"), c("G", "T"), c("A", "G"),
                              c("C", "T"), c("C", "G", "T"), c("A", "G", "T"),
                              c("A","C", "T"), c("A", "C", "G"), c("A", "C", "G", "T"),
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
              "ACG", "ACN", "GTN", "GTT", "GTC", "GTA", "GTG", "TGG",
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




