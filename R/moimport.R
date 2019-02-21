#' Imports molecular data in various formats and transforms
#' them into a standard format.
#'
#' @description \code{moimport} imports molecular data from
#'   Excel workbooks. The function handles various types of
#'   molecular data (e.g. STRs, SNPs), codings (e.g.
#'   4-letter vs. IUPAC format for SNPs), and detects
#'   inconsistencies (e.g. typos, incorrect entries).
#'   \code{moimport} allows users to import data from single
#'   or multiple worksheets.
#'
#' @param file string; specifying the path to the file to be
#'   imported.
#' @param multsheets logical; indicating whether data is
#'   contained in a single or multiple worksheets. The
#'   default value is \code{ multsheets = F}, corresponding
#'   to data contained in a single worksheet.
#' @param nummtd numeric number or vector; number of
#'   metadata columns (e.g. date, sample location, etc.) in
#'   the worksheet(s) to be imported (default value
#'   \code{nummtd = 0}). In case of multiple worksheet
#'   dataset, if all worksheets have the same number of
#'   metadata columns an integer value is sufficient. If the
#'   numbers differ, they have to be specified by an integer
#'   vector.
#' @param molecular string vector or list; specifies the
#'   type of molecular data to be imported. STR, SNP, amino
#'   acid and codon markers are specified with 'STR', 'SNP',
#'   'amino' and 'codon' values, respectively (default value
#'   \code{molecular = 'STR'}). For importing single
#'   worksheets, \code{molecular} is a single string or
#'   string vector. When importing multiple worksheets,
#'   \code{molecular} is a string in case the data contains
#'   only one type of molecular data. Else it is a list,
#'   with the kth element being a string value or a vector
#'   describing the data types of the kth worksheet.
#' @param coding string vector or list; specifies the coding
#'   of each data variable (marker) depending on their type.
#'   Admissible values for \code{coding} depend on molecular
#'   data types are: 'integer', 'nearest', 'ceil' and
#'   'floor' for STRs; SNPs with '4let' and 'iupac' for
#'   SNPs; '3let', '1let' and 'full' amino acids and
#'   'triplet' and 'compact' for codons.
#' @param keepmtd logical; determines whether metadata
#'   (e.g., date) should be retained during import (default
#'   value \code{keepmtd = T}).
#' @param transposed logical or logical vector; if markers
#'   are entered in rows and samples in columns, set
#'   \code{transposed = T} (default value \code{transposed =
#'   F}). When importing multiple worksheets,
#'   \code{transposed} can be logical vector specifying for
#'   each worksheet whether it is in transposed format.
#'
#' @param export string; the path where the imported data is
#'   stored in standardized format. Data is not stored if no
#'   path is specified (default value \code{export = NULL})
#'
#' @return \code{moimport} imports heterogeneous data
#'   formats and converts them into a standard format which
#'   are free from typos (e.g. incompatible and unidentified
#'   entries) appropriate for further analyses. Metadata is
#'   retained (if \code{keepmtd = T}) and, in case of data
#'   from multiple worksheets, unified if metadata variables
#'   have the same labels across two or more worksheets. If
#'   the argument \code{export} is set, then the result is
#'   saved in first worksheet of the workbook of the file
#'   specified by \code{export}.
#'
#' @export
#'
#' @details Each worksheet of the data to be imported must
#'   have one of the following formats: i) one row per
#'   sample and one column per marker. Here cells can have
#'   multiple entries, separated by a special character
#'   (separator), e.g. a punctuation character. ii) one
#'   column per marker and multiple rows per sample
#'   (standard format). iii) one row per sample and multiple
#'   columns per marker. Importantly, within one worksheet
#'   formats ii) and iii) cannot be combined (see section
#'   Warnings and Errors). Combinations of other formats are
#'   permitted but might result in warnings. Additionally,
#'   Occurrence  of different separators are reported (see
#'   section Warnings and Errors).
#'
#'   Users should check the following before data import:
#'   \itemize{ \item the dataset is placed in the first
#'   worksheet of the workbook; \item in case of multiple
#'   worksheets, all worksheets contain data (additional
#'   worksheets need to be removed); \item sample IDs are
#'   placed in the first column (first row in case of
#'   transposed data; see section Exceptions); \item marker
#'   labels are placed in the first row (first column in
#'   case of transposed data; see section Exceptions); \item
#'   sample IDs and as well the marker labels are unique
#'   (the duplication of ID/labels are allowed when
#'   sample/marker contains data in consecutive
#'   rows/columns); \item entries such as sentences (e.g.
#'   comments in the worksheet) or meaningless words (e.g.
#'   'missing' for missing data) are removed from data;
#'   \item metadata columns (rows in case of transposed
#'   data) are placed between sample IDs and
#'   molecular-marker columns.}
#'
#'   If data is contained in multiple worksheets, above
#'   requirements need to be fulfilled for every worksheet
#'   in the Excel workbook. Not all sample IDs must occur in
#'   every worksheet.
#'
#'   The option \code{molecular} needs to be specified as a
#'   vector, for single-worksheet data (\code{multsheets =
#'   F}) containing different types of molecular markers. A
#'   list is specified, if data spread across multiple
#'   worksheets with different types of molecular across the
#'   worksheets. List elements are vectors or single values,
#'   referring to the types of molecular data of the
#'   corresponding worksheets. Users do not need to set a
#'   vector if all markers are of the same molecular type
#'   (single or multiple worksheet dataset).
#'
#'   Setting the option \code{coding} as vector or list is
#'   similar to setting molecular type by \code{molecular}.
#'   Every molecular data type has a pre-specified coding
#'   class as default which users do not need to specify.
#'   Namely, 'integer' for STRs, '4let' for SNPs, '3let' for
#'   amino acids and 'triplet' for codons.
#'
#' @section Warnings and Errors: Usually warnings are
#'   generated if data is corrected pointing to suspicious
#'   entries in the original data. Users should read
#'   warnings carefully and check respective entries and
#'   apply manual corrections if necessary. In case of
#'   issues an error occurs and the function is stopped.
#'
#'   Usually, if arguments are not set properly, errors
#'   occur. Other cases of errors are: i) if sample IDs in a
#'   worksheet are not uniquely defined, i.e., two samples
#'   in non-consecutive rows have the same sample ID; ii) if
#'   formats 'one column per marker and multiple rows per
#'   sample' and 'one row per sample and multiple columns
#'   per marker' are mixed.
#'
#'   Warnings are issued in several cases. Above all, when
#'   typos (e.g., punctuation characters) are found. Entries
#'   which cannot be identified as a molecular type/coding
#'   class specified by the user are also reported (e.g.,
#'   '9' is reported when marker is of type SNPs, or 'L' is
#'   reported when coding class of an amino-acid marker is
#'   '3let').
#'
#'   Empty rows and columns are deleted and eventually
#'   reported. Samples with ambiguous metadata (in a
#'   worksheet or across worksheets in case of multiple
#'   worksheet dataset), or missing are also reported.
#'
#' @section Rarities: Transposed data: usually data is
#'   entered with samples in rows and markers in columns.
#'   However, on the contrary some users might enter data
#'   the opposite way. That is the case of transposed data.
#'   If so, the argument \code{transposed = T} is set, or a
#'   vector in case of multiple worksheets with at least one
#'   worksheet being transposed.
#' @examples
#' ##datasets can be found in the package
#'
#' ##Basic data import
#' infile <- system.file("/extdata/", "testDatabasic.xlsx", package = "tempMLMOI")
#' moimport(file = infile)
#'
#' ##Importing different types of molecular markers
#' infile <- system.file("/extdata/", "testDatamt.xlsx", package = "tempMLMOI")
#' moimport(infile, nummtd = 1, molecular = c('STR','amino','SNP','SNP','amino','codon'))
#'
#' ##Different molecular types with different coding classes
#' infile <- system.file("/extdata/", "testDatamtc.xlsx", package = "tempMLMOI")
#' moimport(infile, nummtd = 1,
#' molecular = c('SNP','STR','STR','amino','SNP','codon'),
#' coding = c('iupac','integer','nearest','full','4let','triplet'))
#'
moimport <-
    function (file, multsheets = F, nummtd = 0, molecular = 'str', coding = 'integer',
              transposed = F, keepmtd = F, export = NULL)
    {
        prerequisite <- moi_prerequisite()
        cha <- prerequisite[[1]]
        cha_num <- prerequisite[[2]]
        cha_string <- prerequisite[[3]]
        ambeguity_code <- prerequisite[[4]]
        represented_bases <- prerequisite[[5]]
        aa_1 <- prerequisite[[6]]
        aa_2 <- prerequisite[[7]]
        let_3 <- prerequisite[[8]]
        amino_acid <- prerequisite[[9]]
        aa_symbol <- prerequisite[[10]]
        compact <- prerequisite[[11]]
        codon_s <- prerequisite[[12]]
        ###
        keepmtd <- keepmtd * 1
        multsheets <- multsheets * 1
        nummtd <- nummtd + 2
        molecular <- lapply(molecular, toupper)
        molecular <- lapply(molecular, function(x) replace(x, is.na(x),'STR'))
        checkmolecular <- unlist(molecular)
        molerror <- match(checkmolecular, c('STR', 'SNP', 'AMINO', 'CODON'))
        if (is.element(NA, molerror) == T) {
            stop(paste(checkmolecular[is.na(molerror)], collapse = " and "), " are not molecular data type.",
                 call. = F)
        }
        coding <- lapply(coding, tolower)
        coding <- lapply(coding, function(x) replace(x, is.na(x),'integer'))
        checkcoding <- unlist(coding)
        coderror <- match(checkcoding, c('integer', 'nearest', 'ceil', 'floor', '4let', 'iupac',
                                         '1let', '3let', 'full', 'triplet', 'compact', NA))
        if (is.element(NA, coderror) == T) {
            stop(paste(checkcoding[is.na(coderror)], collapse = " and "), " are not coding coding class.",
                 call. = F)
        }
        if (length(molecular) == 1) {
            if (molecular == 'SNP'){
                coderror <- is.na(match(coding, c('4let', 'iupac')))*1
                if (length(coding) == 1 && coding == 'integer') {
                    coding <- '4let'
                }
                else if (sum(coderror) > 0) {
                    stop("SNP data needs to be of coding classes '4let' and 'iupac'.", call. = F)
                }
            }
            else if (molecular == 'AMINO'){
                coderror <- is.na(match(coding, c('3let', '1let', 'full')))*1
                if (length(coding) == 1 && coding == 'integer') {
                    coding <- '3let'
                }
                else if (sum(coderror) > 0) {
                    stop("SNP data needs to be of coding classes '3let', '1let' and 'full'.", call. = F)
                }
            }
            else if (molecular == 'CODON'){
                coderror <- is.na(match(coding, c('triplet', 'compact')))*1
                if (length(coding) == 1 && coding == 'integer') {
                    coding <- 'triplet'
                }
                else if (sum(coderror) > 0) {
                    stop("SNP data needs to be of coding classes 'triplet' and 'compact'.", call. = F)
                }
            }
        }
        else if (coding == 'integer' && length(coding) == 1) {
            molec <- match(molecular, c('STR','SNP','AMINO','CODON'))
            cod <- c('integer', '4let', '3let', 'triplet')
            coding <- cod[molec]
            if (multsheets == 1) {
                coding <- as.list(coding)
            }
        }
        else if (length(molecular) != length(coding)) {
            stop("The options 'molecular' and 'coding' must have the same length.", call. = F)
        }
        options(java.parameters = "-Xmx2048m")
        options(nwarnings = 10000)
        if (multsheets == 0) {
            if (keepmtd == 1 && nummtd == 2) {
                stop("For retaining metadata, the option 'nummtd' needs to be set as the
                     number of metadata columns.", call. = F)
            }
            w_b <- XLConnect::loadWorkbook(file)
            if (transposed == T) {
                set_d <- as.matrix(XLConnect::readWorksheet(w_b, sheet = 1, header = F))
                set_d <- t(set_d)
                colnames(set_d) <- set_d[1,]
                alllabels <- set_d[1,][-1]
                set_d <- set_d[-1,]
                rownames(set_d) <- 1:nrow(set_d)
                rw_col <- c("columns ", "column ", "row ", "rows ")
            }
            else if (transposed == F) {
                set_d <- as.matrix(XLConnect::readWorksheet(w_b, sheet = 1))
                alllabels <- XLConnect::readWorksheet(w_b, sheet = 1, startCol = 2,
                                                      endCol = ncol(set_d), startRow = 1, endRow = 1, header = F)
                alllabels <- as.vector(unlist(alllabels))
                rw_col <- c("rows ", "row ", "column ", "columns ")
            }
            print("data is imported")
            if ((nummtd - 1) > ncol(set_d)) {
              stop("Number of metadata columns are bigger than number of dataset columns.",
                   call. = F)
            }
            if (prod(is.element(alllabels[(nummtd - 1):(ncol(set_d) - 1)], NA)) == 1) {
                stop("Marker labels should be specified in the first column.",
                     call. = F)
            }
            else if (prod(is.element(set_d[,1][-1], NA) == 1)) {
                stop("Sample IDs should be specified in the first column.",
                     call. = F)
            }
            emptychecker <- moi_empty(set_d, nummtd)
            setnoempty <- set_d
            verb <- "is"
            if (length(emptychecker[[1]]) > 0) {
                rc <- rw_col[2]
                if(length(emptychecker[[1]]) > 1){
                    rc <- rw_col[1]
                    verb <- "are"
                }
                warning("The following empty ", rc, verb," deleted: ", paste(emptychecker[[1]] + 1, collapse = ", "), ".",
                        call. = F, noBreaks. = T)
                setnoempty <- emptychecker[[3]]
            }
            if (length(emptychecker[[2]]) > 0) {
                rc <- rw_col[3]
                if(length(emptychecker[[2]]) > 1){
                    rc <- rw_col[4]
                    verb <- "are"
                }
                warning("The following empty ", rc, verb, " deleted: ", paste(emptychecker[[2]], collapse = " and "), ".",
                        call. = F, noBreaks. = T)
                alllabels <- alllabels[-(emptychecker[[2]] - 1)]
                set_d <- set_d[,-emptychecker[[2]]]
                nummtd <- nummtd - sum((emptychecker[[2]] < nummtd)*1)
            }
            markerlabels <- alllabels[(nummtd - 1):(ncol(set_d) - 1)]
            markerlabels <- markerlabels[!is.na(markerlabels)]
            set_dinfo <- moi_administrator(set_d, set_d[,1], markerlabels, nummtd, cha, rw_col, 1)

            if(set_dinfo[[11]] == 1) {
                stop("Dataset cannot be simulataneously of two formats 'multiple columns per marker' and 'multiple rows per sample'.", call. = F)
            }

            s_l <- moi_duplicatefinder(setnoempty[,1], "Sample ID ", 0, rw_col)
            if (s_l == 1) {
                stop("Sample IDs need to be defined uniquely.", call. = F)
            }
            m_l <- moi_duplicatefinder(markerlabels, nummtd - 2, rev(rw_col))
            if (m_l == 1) {
                stop("Marker labels need to be defined uniquely.", call. = F)
            }
            samorder <- set_dinfo[[1]]
            samall <- set_dinfo[[2]]
            lsam <- set_dinfo[[3]]
            s <- set_dinfo[[4]]
            cons <- set_dinfo[[5]]
            m <- set_dinfo[[9]]
            conm <- set_dinfo[[10]]
            if (keepmtd == 1) {
                metadata <- as.matrix(set_d[, c(2:(nummtd - 1))])
                mtdlabels <- alllabels[1:(nummtd - 2)]
                metadata <- moi_metadata(metadata, mtdlabels, nummtd - 2, samorder, samall, lsam, 0)[[1]]
            }
            markerlabels <- markerlabels[!duplicated(markerlabels)]

            if (length(molecular) == 1) {
                molecular <- rep(molecular, length(m))
            }
            else if (length(molecular) != length(m)) {
                stop("Length of option 'molecular' must be the same as the number of markers.",
                     call. = F)
            }
            if (length(coding) == 1) {
                coding <- rep(coding, length(m))
            }
            else if (length(coding) != length(m)) {
                stop("Length of option 'coding' must be the same as the number of markers.",
                     call. = F)
            }
            exporting <- list()
            ll <- matrix(0, length(m), lsam)
            sb <- nummtd
            if (nummtd <= ncol(set_d)) {
                for (j in 1:length(m)) {
                    col_j <- set_d[, sb:(sb + m[j] - 1)]
                    sb <- sb + m[j]
                    result <- moi_marker(col_j, markerlabels[j], samorder, samorder, conm, cons, molecular[[j]], coding[[j]],
                                         cha_num, cha_string, ambeguity_code, represented_bases,
                                         aa_1, aa_2, let_3, amino_acid, aa_symbol, compact,
                                         codon_s, rw_col)
                    exporting[[j]] <- result[[1]]
                    ll[j, ] <- unlist(lapply(result[[1]], length))
                }
            }else{
                markerlabels <- NA
                exporting[[1]] <- NA
                ll<- matrix(1, 1, lsam)
                warning("Dataset does not contain molecular marker columns.", call. = F)
            }
        }
        else if (multsheets == 1) {
            "Multiple worksheet dataset"
            w_b <- XLConnect::loadWorkbook(file)
            sh_names <- XLConnect::getSheets(w_b)
            nsheet <- length(sh_names)
            if (length(nummtd) == 1) {
                nummtd <- rep(nummtd, nsheet)
            }
            else if (length(nummtd) != nsheet){
                stop("Length of the argument 'nummtd' does not match the number of
                     marker worksheets.", call. = F)
            }
            if (length(transposed) == 1) {
                transposed <- rep(transposed, nsheet)
            }
            else if (length(transposed) != nsheet) {
                stop("Length of the argument 'transposed' does not match the number of
                     marker worksheets.", call. = F)
            }
            "checking worksheets for sample IDs"
            samall <- 0
            mtdall <- list()
            alldata <- list()
            markerlabels <- list()
            mdnames <- list()
            mark <- 0
            extrainfo <- list()
            deletelab <- 0
            numarkwsh <- 1:nsheet
            for (n in 1:nsheet) {
                if (transposed[n] == T) {
                    set_d <- as.matrix(XLConnect::readWorksheet(w_b, sheet = n, header = F))
                    set_d <- t(set_d)
                    colnames(set_d) <- set_d[1,]
                    alllabels <- set_d[1,][-1]
                    set_d <- set_d[-1,]
                    rownames(set_d) <- 1:nrow(set_d)
                    rw_col <- c("columns ", "column ", "row ", "rows ")
                }
                else if (transposed[n] == F) {
                    set_d <- as.matrix(XLConnect::readWorksheet(w_b, sheet = n))
                    alllabels <- XLConnect::readWorksheet(w_b, sheet = n, startCol = 2,
                                                          endCol = ncol(set_d), startRow = 1,
                                                          endRow = 1, header = F)
                    alllabels <- unlist(alllabels)
                    rw_col <- c("rows ", "row ", "column ", "columns ")
                }
                if ((nummtd[n] - 1) > ncol(set_d)) {
                  stop("Number of metadata columns are bigger than number of dataset columns in worksheet ", n,".",
                       call. = F)
                }
                emptychecker <- moi_empty(set_d, nummtd)
                setnoempty <- set_d
                if (length(emptychecker[[1]]) > 0) {
                    warning("In worksheet ", n, " the following empty ", rw_col[1]," are deleted: ",
                            paste(emptychecker[[1]] + 1, collapse = ", "), ".",
                            call. = F, noBreaks. = T)
                    setnoempty <- emptychecker[[3]]
                }
                if (length(emptychecker[[2]]) > 0) {
                    warning("In worksheet ", n, " te following empty ", rw_col[4]," are deleted: ",
                            paste(emptychecker[[2]], collapse = " and "), ".",
                            call. = F, noBreaks. = T)
                    alllabels <- alllabels[-(emptychecker[[2]] - 1)]
                    set_d <- set_d[,-emptychecker[[2]]]
                    nummtd[n] <- nummtd[n] - sum((emptychecker[[2]] < nummtd[n])*1)
                }
                if (keepmtd == 1 && nummtd[n] > 2){
                  mtdall[[n]] <- alllabels[1:(nummtd[n] - 2)]
                }
                mlabels <- as.vector(alllabels[(nummtd[n] - 1):(ncol(set_d) - 1)])
                lab <- 0
                if (length(alllabels) == (nummtd[n] - 2)) {
                    lab <- lab + 1 + mark
                    deletelab <- append(deletelab, lab)
                    mlabels <- paste("emptymarker7-5m529", lab)
                    set_d <- cbind(set_d, rep(NA, nrow(set_d)))
                }
                s_total_mark <- as.vector(set_d[, 1])
                samall <- append(samall, s_total_mark)
                set_dinfo <- moi_administrator(set_d, s_total_mark, mlabels, nummtd[n], cha, rw_col, n)
                if(set_dinfo[[11]] == 1) {
                    stop("In worksheet ", n," (", sh_names[n], ") dataset is simulataneously of two formats 'multiple columns per marker' and 'multiple rows per sample'.", call. = F)
                }
                samorder <- set_dinfo[[1]]
                samnames <- set_dinfo[[2]]
                cons <- set_dinfo[[5]]
                conm <- set_dinfo[[10]]
                m <- set_dinfo[[9]]
                sb <- nummtd[n]
                mlabels <- mlabels[!is.na(mlabels)]
                mlabels <- mlabels[!duplicated(mlabels)]
                markerlabels <- append(markerlabels, mlabels)
                numarkwsh[n] <- length(m)
                for (j in 1:length(m)) {
                    mark <- mark + 1
                    alldata[[mark]] <- set_d[,c(1:(nummtd[n] - 1), sb:(sb + m[j] - 1))]
                    extrainfo[[mark]] <- list(samorder, samnames, rw_col, n, cons, conm)
                    sb <- sb + m[j]
                }
            }
            if (length(molecular) == 1) {
                molecular <- rep(molecular, nsheet)
                molecular <- rep(molecular, numarkwsh)
            }
            else if (is.list(molecular) == F) {
                stop("The argument 'molecular' needs to be a list.", call. = F)
            }
            else {
                argmol <- lengths(molecular) == numarkwsh
                if (length((molecular)) != nsheet) {
                    stop("The elements of argument 'molecular' needs to match the number of worksheets.", call. = F)
                }
                else if (prod(argmol) != 1) {
                    argmol <- which(argmol == F)
                    stop("The elements of argument 'molecular' are not chosen properly. Worksheets ", paste(argmol, collapse = " and "), " contain respectively ",
                         paste(numarkwsh[argmol], collapse = " and "), "markers.", call. = F)
                }
            }
            if (length(coding) == 1) {
                coding <- rep(coding, nsheet)
                coding <- rep(coding, numarkwsh)
            }
            else if (is.list(coding) == F) {
                stop("The argument 'coding' needs to be a list.", call. = F)
            }
            else {
                argcod <- lengths(coding) == numarkwsh
                if (length((coding)) != nsheet) {
                    stop("The elements of argument 'coding' needs to match the number
                         of worksheets.", call. = F)
                }
                else if (prod(argcod) != 1) {
                    argcod <- which(argcod == F)
                    stop("The elements of argument 'coding' are not chosen properly.
                         Worksheets ", paste(argcod, collapse = " and "), " contain respectively ",
                         paste(numarkwsh[argcod], collapse = " and "), " markers.", call. = F)
                }
            }
            markerlabels <- unlist(markerlabels)
            if (anyDuplicated(markerlabels) > 0) {
                stop("Marker labels need to be unique.", call. = F)
            }
            molecular <- unlist(molecular)
            coding <- unlist(coding)
            samall <- samall[-1]
            samall <- samall[!is.na(samall)]
            samall <- unique(samall)
            lsam <- length(samall)
            ll <- matrix(0, mark, lsam)
            exporting <- list()
            tempmtd <- matrix(0, lsam, 1)
            prensh <- 0
            for (mk in 1:mark) {
                datamark <- as.matrix(alldata[[mk]])
                samidsmark <- extrainfo[[mk]][[1]]
                samnamesmark <- extrainfo[[mk]][[2]]
                rw_col <- extrainfo[[mk]][[3]]
                nsh <- extrainfo[[mk]][[4]]
                cons <- extrainfo[[mk]][[5]]
                conm <- extrainfo[[mk]][[6]]
                warnid <- 0
                roworders <- 0
                samidsmarksort <- match(samall,samnamesmark)
                samidsmarkna <- which(is.na(samidsmarksort) == T)
                lsmna <- length(samidsmarkna)
                if(lsmna > 0){
                    naextramatrix <- matrix(c(samall[samidsmarkna],rep(NA, lsmna*(ncol(datamark) - 1))),
                                            lsmna, ncol(datamark))
                    datamark <- rbind(datamark, naextramatrix)
                    naextra <- max(samidsmarksort[!is.na(samidsmarksort)]) + 1:lsmna
                    samidsmarksort[samidsmarkna] <- naextra
                    samidsmark <- append(samidsmark, max(samidsmark) + 1:lsmna)
                }
                samidsmarkorder <- samidsmark[samidsmarksort]
                plus1order <- samidsmark[samidsmarksort + 1]
                samidsmarkrows <- matrix(c(samidsmarkorder, plus1order), 2, length(samidsmarkorder), byrow = T)
                roworders <- lapply(1:ncol(samidsmarkrows), function(x) append(roworders, samidsmarkrows[1,x]:(samidsmarkrows[2,x] - 1)))
                roworders <- lapply(roworders, function(x) x[-1])
                roworders <- unlist(roworders)
                datamark <- datamark[roworders,]
                samidsmark <- moi_labels(datamark[,1])[[1]]
                if (prensh != nsh) {
                    warning("In worksheet ", nsh," (", sh_names[nsh], "): ", call. = F)
                    if (keepmtd == 1 && nummtd[nsh] > 2) {
                        mtdnsh <- as.matrix(datamark[,2:(nummtd[nsh] - 1)])
                        mtdnsh <- moi_metadata(mtdnsh, mtdall[[nsh]], nummtd[nsh] - 2, samidsmark,
                                               samall, lsam, samidsmarkna)
                        warnid <- mtdnsh[[2]]
                        mtdnsh <- mtdnsh[[1]]
                        tempmtd <- cbind(tempmtd, mtdnsh)
                    }
                }
                col_j <- as.matrix(datamark[, nummtd[nsh]:ncol(datamark)])
                result <- moi_marker(col_j, markerlabels[mk], samidsmark, samidsmarkorder, conm, cons,
                                     molecular[mk], coding[mk], cha_num, cha_string, ambeguity_code,
                                     represented_bases, aa_1, aa_2, let_3, amino_acid, aa_symbol,
                                     compact, codon_s, rw_col)
                warnid <- result[[2]] + warnid
                exporting[[mk]] <- result[[1]]
                ll[mk, ] <- unlist(lapply(result[[1]], length))
                if (warnid == 0 && prensh != nsh) {
                    warning("---There were no warnings!---", call. = F)
                }
                prensh <- nsh
            }
            tempmtd <- tempmtd[,-1]
            if (keepmtd == 1) {
                metadata <- moi_mergemetadata(tempmtd, mtdall, samall)
                mtdlabels <- metadata[[1]]
                metadata <- metadata[[2]]
            }
        }
        "Finding the number of rows needed for each sample"
        l <- apply(ll, 2, max)
        for (k in 1:length(exporting)) {
            for (j in 1:lsam) {
                l2 <- ll[k,j]
                if (l2 < l[j]) {
                    exporting[[k]][[j]] <- c(exporting[[k]][[j]], rep(NA, (l[j] - l2)))
                }
            }
        }
        "Building up metadata"
        samall <- samall[-(lsam + 1)]
        esam <- rep(samall, l)
        if (keepmtd == 1) {
            finalmtd <- matrix(0, length(esam), ncol(metadata))
            for (i in 1:ncol(metadata)) {
                finalmtd[, i] <- rep(as.vector(metadata[, i]), l)
            }
        }
        "Building up markers"
        if (multsheets == 0) {
            pdata <- matrix(as.character(unlist(exporting)), sum(l), length(markerlabels))
        }
        else if (multsheets == 1) {
            pdata <- matrix(as.character(unlist(exporting)), sum(l), length(markerlabels))
            pdata <- pdata[,-deletelab]
            markerlabels <- markerlabels[-deletelab]
        }
        "Finalzing stage. Preparing data to export"
        if (keepmtd == 1) {
            final <- cbind(esam, finalmtd, pdata)
            colnames(final) <- c("Sample IDs", mtdlabels, markerlabels)
        }
        else {
            final <- cbind(esam, pdata)
            colnames(final) <- c("Sample IDs", markerlabels)
        }
        rownames(final) <- c(1:nrow(final))
        if (is.null(export) == F) {
            moi_export(export, final)
        }
        on.exit()
        final
        }






