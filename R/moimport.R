#'Imports molecular data in various formats and transforms
#'them into a standard format.
#'
#'@description \code{moimport()} imports molecular data from
#'  Excel workbooks. The function handles various types of
#'  molecular data (e.g. STRs, SNPs), codings (e.g. 4-letter
#'  vs. IUPAC format for SNPs), and detects inconsistencies
#'  (e.g. typos, incorrect entries). \code{moimport()}
#'  allows users to import data from single or multiple
#'  worksheets.
#'
#'@param file string; specifying the path to the file to be
#'  imported.
#'@param multsheets logical; indicating whether data is
#'  contained in a single or multiple worksheets. The
#'  default value is \code{multsheets = FALSE},
#'  corresponding to data contained in a single worksheet.
#'@param nummtd numeric number or vector; number of metadata
#'  columns (e.g. date, sample location, etc.) in the
#'  worksheet(s) to be imported (default value \code{nummtd
#'  = 0}). In case of multiple worksheet dataset, if all
#'  worksheets have the same number of metadata columns an
#'  integer value is sufficient. If the numbers differ, they
#'  have to be specified by an integer vector.
#'@param molecular string vector or list; specifies the type
#'  of molecular data to be imported. STR, SNP, amino acid
#'  and codon markers are specified with 'STR', 'SNP',
#'  'amino' and 'codon' values, respectively (default value
#'  \code{molecular = 'str'}). For importing single
#'  worksheets, \code{molecular} is a single string or
#'  string vector. When importing multiple worksheets,
#'  \code{molecular} is a string in case the data contains
#'  only one type of molecular data. Else it is a list, with
#'  the k-th element being a string value or a vector
#'  describing the data types of the k-th worksheet.
#'@param coding string vector or list; specifies the coding
#'  of each data variable (marker) depending on their type.
#'  Admissible values for \code{coding} depend on molecular
#'  data types are: 'integer', 'nearest', 'ceil' and 'floor'
#'  for STRs; SNPs with '4let' and 'iupac' for SNPs; '3let',
#'  '1let' and 'full' amino acids and 'triplet' and
#'  'compact' for codons.
#'@param keepmtd logical; determines whether metadata (e.g.,
#'  date) should be retained during import (default value
#'  \code{keepmtd = TRUE}).
#'@param transposed logical or logical vector; if markers
#'  are entered in rows and samples in columns, set
#'  \code{transposed = TRUE} (default value \code{transposed
#'  = FALSE}). When importing multiple worksheets,
#'  \code{transposed} can be logical vector specifying for
#'  each worksheet whether it is in transposed format.
#'@param export string; the path where the imported data is
#'  stored in standardized format. Data is not stored if no
#'  path is specified (default value \code{export = NULL}).
#'@param keepwarnings string; the path where the warnings
#'  are stored. Warnings are not stored if no path is
#'  specified (default value \code{keepwarnings = NULL}).
#'
#'@return returns a data frame. \code{moimport()} imports
#'  heterogeneous data formats and converts them into a
#'  standard format which are free from typos (e.g.
#'  incompatible and unidentified entries) appropriate for
#'  further analyses. Metadata is retained (if \code{keepmtd
#'  = TRUE}) and, in case of data from multiple worksheets,
#'  unified if metadata variables have the same labels
#'  across two or more worksheets. If the argument
#'  \code{export} is set, then the result is saved in the
#'  first worksheet of the workbook of the file specified by
#'  \code{export}. The imported/exported dataset will be
#'  appropriate for other functions of the package.
#'
#'@details Each worksheet of the data to be imported must
#'  have one of the following formats: i) one row per sample
#'  and one column per marker. Here cells can have multiple
#'  entries, separated by a special character (separator),
#'  e.g. a punctuation character. ii) one column per marker
#'  and multiple rows per sample (standard format). iii) one
#'  row per sample and multiple columns per marker.
#'  Importantly, within one worksheet formats ii) and iii)
#'  cannot be combined (see section Warnings and Errors).
#'  Combinations of other formats are permitted but might
#'  result in warnings. Additionally, Occurrence  of
#'  different separators are reported (see section Warnings
#'  and Errors).
#'
#'  Users should check the following before data import:
#'  \itemize{ \item the dataset is placed in the first
#'  worksheet of the workbook; \item in case of multiple
#'  worksheets, all worksheets contain data (additional
#'  worksheets need to be removed); \item sample IDs are
#'  placed in the first column (first row in case of
#'  transposed data; see section Exceptions); \item marker
#'  labels are placed in the first row (first column in case
#'  of transposed data; see section Exceptions); \item
#'  sample IDs and as well the marker labels are unique (the
#'  duplication of ID/labels are allowed when sample/marker
#'  contains data in consecutive rows/columns); \item
#'  entries such as sentences (e.g. comments in the
#'  worksheet) or meaningless words (e.g. 'missing' for
#'  missing data) are removed from data; \item metadata
#'  columns (rows in case of transposed data) are placed
#'  between sample IDs and molecular-marker columns.}
#'
#'  If data is contained in multiple worksheets, above
#'  requirements need to be fulfilled for every worksheet in
#'  the Excel workbook. Not all sample IDs must occur in
#'  every worksheet. The sample ID must not be confused with
#'  the patient's ID, the former refers to a particular
#'  sample taken from a patient, the latter to a unique
#'  patient. Several sample IDs can have the same patient's
#'  ID. In case of multiple-worksheet datasets, all marker
#'  labels across all worksheets need to be unique.
#'
#'  The option \code{molecular} needs to be specified as a
#'  vector, for single-worksheet data (\code{multsheets =
#'  FALSE}) containing different types of molecular markers.
#'  A list is specified, if data spread across multiple
#'  worksheets with different types of molecular across the
#'  worksheets. List elements are vectors or single values,
#'  referring to the types of molecular data of the
#'  corresponding worksheets. Users do not need to set a
#'  vector if all markers are of the same molecular type
#'  (single or multiple worksheet dataset).
#'
#'  Setting the option \code{coding} as vector or list is
#'  similar to setting molecular type by \code{molecular}.
#'  Every molecular data type has a pre-specified coding
#'  class as default which users do not need to specify.
#'  Namely, 'integer' for STRs, '4let' for SNPs, '3let' for
#'  amino acids and 'triplet' for codons.
#'
#'@section Warnings and Errors: Usually warnings are
#'  generated if data is corrected pointing to suspicious
#'  entries in the original data. Users should read warnings
#'  carefully and check respective entries and apply manual
#'  corrections if necessary. In case of issues an error
#'  occurs and the function is stopped.
#'
#'  Usually, if arguments are not set properly, errors
#'  occur. Other cases of errors are: i) if sample IDs in a
#'  worksheet are not uniquely defined, i.e., two samples in
#'  non-consecutive rows have the same sample ID; ii) if
#'  formats 'one column per marker and multiple rows per
#'  sample' and 'one row per sample and multiple columns per
#'  marker' are mixed.
#'
#'  Warnings are issued in several cases. Above all, when
#'  typos (e.g., punctuation characters) are found. Entries
#'  which cannot be identified as a molecular type/coding
#'  class specified by the user are also reported (e.g., '9'
#'  is reported when marker is of type SNPs, or 'L' is
#'  reported when coding class of an amino-acid marker is
#'  '3let').
#'
#'  Empty rows and columns are deleted and eventually
#'  reported. Samples with ambiguous metadata (in a
#'  worksheet or across worksheets in case of multiple
#'  worksheet dataset), or missing are also reported.
#'
#'  The function only prints the first 50 warnings.
#'  If the number of warnings are more than 50, the user is
#'  recommended to set the argument \code{keepwarnings},
#'  in order to save the warnings in an Excel file.
#'
#'@section Exceptions: Transposed data: usually data is
#'  entered with samples in rows and markers in columns.
#'  However, on the contrary some users might enter data the
#'  opposite way. That is the case of transposed data. If
#'  so, the argument \code{transposed = TRUE} is set, or a
#'  vector in case of multiple worksheets with at least one
#'  worksheet being transposed.
#'
#'@seealso For further details, see the following vignettes:
#'
#'  \code{vignette("dataimportcheck-list", package =
#'  "MLMOI")}
#'
#'  \code{vignette("StandardAmbiguityCodes", package =
#'  "MLMOI")}
#'
#'  \code{vignette("moimport-arguments", package = "MLMOI")}
#'
#'@export
#'
#' @examples
#' #datasets are provided by the package
#'
#' #importing dataset with metadata variables:
#' infile <- system.file("extdata", "testDatametadata.xlsx", package = "MLMOI")
#' moimport(infile, nummtd = 3, keepmtd = TRUE)
#'
#'
#' ##more examples are included in 'examples' vignette:
#'
#' #vignette("examples", package = "MLMOI")
#'
moimport <-
    function (file, multsheets = FALSE, nummtd = 0, molecular = 'str', coding = 'integer',
              transposed = FALSE, keepmtd = FALSE, export = NULL, keepwarnings = NULL)
    {

        oldops <- options(stringsAsFactors = FALSE)
        on.exit(options(oldops))
        ##rJava::.jpackage("MLMOI")
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
        general_warnings <- list()
        metadata_warnings <- list()
        marker_warnings <- list()
        allmolecular <- c('STR', 'SNP', 'AMINO', 'CODON','PLASMODIUM')   ## KS - cor
        allcoding <- list(c('integer', 'nearest', 'floor', 'ceil'), c('4let', 'iupac'),
                          c('3let', '1let', 'full'), c('triplet', 'compact'))
        ###
        if (file.exists(file) == FALSE) {
            stop("File cannot be found. Check the path in 'file'.", call. = FALSE)
        }

        if (length(multsheets) > 1) {
            stop("The argument 'multsheets' needs to be a single value boolean.", call. = FALSE)
        }
        else if (is.logical(multsheets) == FALSE) {
            stop("The argument 'multsheets' needs to be a single value boolean.", call. = FALSE)
        }
        if (length(keepmtd) > 1) {
            stop("The argument 'keepmtd' needs to be a single value boolean.", call. = FALSE)
        }
        else if (is.logical(keepmtd) == FALSE) {
            stop("The argument 'keepmtd' needs to be a single value boolean.", call. = FALSE)
        }

        if (multsheets == FALSE) {
            if (length(nummtd) > 1){
                stop("'For single worksheet dataset 'nummtd' is a single value.", call. = FALSE)
            }
            if (length(transposed) > 1) {
                stop("'For single worksheet dataset 'transposed' is a single value.", call. = FALSE)
            }
        }
        else {
            if (is.list(molecular) == FALSE && length(molecular) != 1) {
                stop("The argument 'molecular' needs to be a list for multiple worksheet datasets.", call. = FALSE)
            }
            if (is.list(coding) == FALSE && length(coding) != 1) {
                stop("The argument 'coding' needs to be set as a list for multiple worksheet datasets.", call. = FALSE)
            }
        }
        keepmtd <- keepmtd * 1
        multsheets <- multsheets * 1
        if (is.logical(transposed) == FALSE) {
            stop("The argument 'transposed' needs to take boolean values.", call. = FALSE)
        }
        if (floor(prod(nummtd)) != prod(nummtd)) {
            stop("The argument 'nummtd' needs to take integer values.", call. = FALSE)
        }
        nummtd <- nummtd + 2
        checkmolecular <- lapply(molecular, toupper)
        checkmolecular <- unlist(checkmolecular)
        molerror <- match(checkmolecular, allmolecular)
        if (is.element(NA, molerror) == TRUE) {
            if (length(checkmolecular[is.na(molerror)]) > 1) {
                mler <- c(" are", " types", " ")
            }
            else {
                mler <- c(" is", " type", " a ")
            }
            stop(paste(shQuote(checkmolecular[is.na(molerror)], "sh"), collapse = " and "), mler[1], " not", mler[3], "molecular data", mler[2], ".",
                 call. = FALSE)
        }
        checkcoding <- lapply(coding, tolower)
        checkcoding <- unlist(checkcoding)
        coderror <- match(checkcoding, unlist(allcoding))
        if (is.element(NA, coderror) == TRUE) {
            if (length(checkcoding[is.na(coderror)]) > 1) {
                coder <- c(" are", " classes", " ")
            }
            else {
                coder <- c(" is", " class", " a ")
            }
            stop(paste(shQuote(checkcoding[is.na(coderror)], "sh"), collapse = " and "), coder[1], " not", coder[3], "coding", coder[2], ".",
                 call. = FALSE)
        }
        molid <- 0
        if (length(molecular) == 1) {
            molid <- 1
            if (checkmolecular == 'STR'){
                coderror <- is.na(match(checkcoding, c('integer', 'nearest', 'floor', 'ceil')))*1
                if (length(coding) == 1 && checkcoding[1] == 'integer') {
                    coding <- 'integer'
                }
                else if (sum(coderror) > 0) {
                    stop("STR data needs to be of coding classes 'integer', 'nearest', 'floor' and 'ceil'.", call. = FALSE)
                }
            }
            else if (checkmolecular == 'SNP'){
                coderror <- is.na(match(checkcoding, c('4let', 'iupac')))*1
                if (length(coding) == 1 && checkcoding[1] == 'integer') {
                    coding <- '4let'
                }
                else if (sum(coderror) > 0) {
                    stop("SNP data needs to be of coding classes '4let' and 'iupac'.", call. = FALSE)
                }
            }
            else if (checkmolecular == 'AMINO'){
                coderror <- is.na(match(checkcoding, c('3let', '1let', 'full')))*1
                if (length(coding) == 1 && checkcoding[1] == 'integer') {
                    coding <- '3let'
                }
                else if (sum(coderror) > 0) {
                    stop("Amino-acid data needs to be of coding classes '3let', '1let' and 'full'.", call. = FALSE)
                }
            }
            else if (checkmolecular == 'CODON'){
                coderror <- is.na(match(checkcoding, c('triplet', 'compact')))*1
                if (length(coding) == 1 && checkcoding[1] == 'integer') {
                    coding <- 'triplet'
                }
                else if (sum(coderror) > 0) {
                    stop("Codon data needs to be of coding classes 'triplet' and 'compact'.", call. = FALSE)
                }
            }
            else if (checkmolecular == 'PLASMODIUM'){
              coderror <- is.na(match(checkcoding, c('triplet', 'compact')))*1
              if (length(coding) == 1 && checkcoding[1] == 'integer') {
                coding <- 'human plasmodium'
              }
              else if (sum(coderror) > 0) {
                stop("Codon data needs to be of coding classes 'triplet' and 'compact'.", call. = FALSE)
              }
            }
        }
        else if (checkcoding[1] == 'integer' && length(coding) == 1) {
            cod <- c('integer', '4let', '3let', 'triplet')
            mol <- c('STR','SNP','AMINO','CODON','PLASMODIUM') ##KS-cor
            if (multsheets == 0) {
                molec <- match(checkmolecular, mol)
                coding <- cod[molec]
            }
            else if (multsheets == 1) {
                molec <- list()
                coding <- list()
                molecular <- lapply(molecular, toupper)
                molka <- lapply(1:length(molecular), function(x) molec[[x]] <- match(molecular[[x]], mol))
                coding <- lapply(1:length(molecular), function(x) coding[[x]] <- cod[molka[[x]]])
            }
        }
        else if (length(molecular) != length(coding) && length(coding) != 1 && length(molecular) != 1) {
            stop("The arguments 'molecular' and 'coding' must have the same length.", call. = FALSE)
        }

        if (multsheets == 0) {
            if (keepmtd == 1 && nummtd == 2) {
                stop("The argument 'keepmtd' is set as 'TRUE'. For retaining metadata, the argument 'nummtd' needs to be set as the number of metadata columns.", call. = FALSE)
            }
            #KS# w_b <- XLConnect::loadWorkbook(file)
            w_b <- openxlsx::read.xlsx(file,1)
            if (transposed == TRUE) {
                #KS# set_d <- t(as.matrix(XLConnect::readWorksheet(w_b, sheet = 1, header = FALSE)))
                set_d <- t(as.matrix(openxlsx::read.xlsx(file, sheet = 1,colNames=FALSE)))
                colnames(set_d) <- set_d[1,]
                alllabels <- set_d[1,][-1]
                set_d <- set_d[-1,]
                rownames(set_d) <- 1:nrow(set_d)
                rw_col <- c("columns ", "column ", "row ", "rows ")
            }
            else if (transposed == FALSE) {
                #KS# set_d <- as.matrix(XLConnect::readWorksheet(w_b, sheet = 1))
                set_d <- as.matrix(openxlsx::read.xlsx(file, sheet = 1))
                alllabels <- as.vector(colnames(set_d)[-1])
                #KS# alllabels <- XLConnect::readWorksheet(w_b, sheet = 1, startCol = 2, endCol = ncol(set_d),
                #KS#                                       startRow = 1, endRow = 1, header = FALSE,
                #KS#                                       autofitCol = FALSE, autofitRow = FALSE)
                #KS# alllabels <- as.vector(unlist(alllabels))
                rw_col <- c("rows ", "row ", "column ", "columns ")
            }
            set_d[,1] <- trimws(set_d[,1])
            alllabels <- trimws(alllabels)
            if ((nummtd - 1) > ncol(set_d)) {
                stop("The number of metadata columns exceeds the number of columns in the dataset.",
                     call. = FALSE)
            }
            if (prod(is.element(alllabels[(nummtd - 1):(ncol(set_d) - 1)], NA)) == 1) {
                stop("Marker labels should be specified in the first row",
                     call. = FALSE)
            }
            else if (prod(is.element(set_d[,1], NA) == 1)) {
                stop("Sample IDs should be specified in the first column.",
                     call. = FALSE)
            }
            setnoempty <- set_d
            emptylabels <- alllabels[(nummtd - 1):(ncol(set_d) - 1)]
            emptychecker <- withWarnings(moi_empty(set_d, setnoempty, nummtd, rw_col, multsheets, alllabels, molecular, molid,1))
            set_d <- emptychecker[[1]][[1]]
            setnoempty <- emptychecker[[1]][[2]]
            alllabels <- emptychecker[[1]][[3]]
            nummtd <- emptychecker[[1]][[4]]
            general_warnings <- emptychecker[[2]]
            markerlabels <- alllabels[(nummtd - 1):(ncol(set_d) - 1)]
            if (is.na(set_d[1,1]) == TRUE || length(set_d[1,1]) == 0) {
                stop("No sample ID is given in the first row." , call. = FALSE)
            }
            s_l <- moi_duplicatefinder(set_d[,1], "Sample ID ", 0, rw_col)
            if (s_l == 1) {
                stop("Sample IDs need to be defined uniquely.", call. = FALSE)
            }
            if (is.na(emptylabels[1]) == TRUE || length(emptylabels[1]) == 0) {
                stop("No marker label is given in the first column.", call. = FALSE)
            }
            m_l <- moi_duplicatefinder(emptylabels, "Marker label ", nummtd - 2, rev(rw_col))
            if (m_l == 1) {
                stop("Marker labels need to be defined uniquely.", call. = FALSE)
            }
            set_dinfo <- withWarnings(moi_administrator(set_d, set_d[,1], markerlabels, nummtd, cha, rw_col, 1, transposed, ""))
            general_warnings <- append(general_warnings, set_dinfo[[2]])
            set_dinfo <- set_dinfo[[1]]
            if(set_dinfo[[11]] == 1) {
                stop("The dataset cannot be simultaneously of two formats 'multiple columns per marker' and 'multiple rows per sample'.", call. = FALSE)
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
                metadata <- withWarnings(moi_metadata(metadata, mtdlabels, nummtd - 2, samorder, samall, lsam, 0, TRUE, ""))
                metadata_warnings <- metadata[[2]]
                metadata <- metadata[[1]][[1]]
            }
            markerlabels <- markerlabels[!is.na(markerlabels)]
            markerlabels <- markerlabels[!duplicated(markerlabels)]

            if (length(molecular) == 1) {
                molecular <- rep(molecular, length(m))
            }
            else if (length(molecular) != length(m)) {
                stop("Length of the argument 'molecular' must be the same as the number of markers.",
                     call. = FALSE)
            }
            if (length(coding) == 1) {
                coding <- rep(coding, length(m))
            }
            else if (length(coding) != length(m)) {
                stop("Length of the argument 'coding' must be the same as the number of markers.",
                     call. = FALSE)
            }
            tempmolecular <- unlist(lapply(molecular, toupper))
            tempcoding <- unlist(lapply(coding, tolower))
            matchallmol <- match(tempmolecular, allmolecular)
            matchallcod <- unlist(lapply(1:length(tempcoding), function(x) is.element(tempcoding[x],allcoding[[matchallmol[x]]])))
            if (prod(matchallcod) == 0) {
                warning("The coding classes ", paste(shQuote(tempcoding[!matchallcod], "sh"), collapse = " and "), " do not correspond to molecular data types ", paste(shQuote(tempmolecular[!matchallcod], "sh"), collapse = " and "), ", respectively.", call. = FALSE)
                stop("The argument 'coding' is not set properly.")
            }
            coding <- tempcoding
            molecular <- tempmolecular
            exporting <- list()
            ll <- matrix(0, length(m), lsam)
            sb <- nummtd
            marker_warnings <- list()
            if (nummtd <= ncol(set_d)) {
                for (j in 1:length(m)) {
                    col_j <- set_d[, sb:(sb + m[j] - 1)]
                    sb <- sb + m[j]
                    result <- withWarnings(moi_marker(col_j, markerlabels[j], samorder, samorder, conm, cons, molecular[[j]],
                                                      coding[[j]], cha_num, cha_string, ambeguity_code, represented_bases,
                                                      aa_1, aa_2, let_3, amino_acid, aa_symbol, compact,
                                                      codon_s, rw_col, ""))
                    marker_warnings[[j]] <- result[[2]]
                    result <- result[[1]]
                    exporting[[j]] <- result[[1]]
                    ll[j, ] <- unlist(lapply(result[[1]], length))

                }
            }else{
                markerlabels <- NA
                exporting[[1]] <- NA
                ll<- matrix(1, 1, lsam)
                warning("Dataset does not contain molecular marker columns.", call. = FALSE)
            }
        }
        else if (multsheets == 1) {
            "Multiple worksheet dataset"
            #KS#  w_b <- XLConnect::loadWorkbook(file)
            #KS# sh_names <- XLConnect::getSheets(w_b)
            sh_names <- openxlsx::getSheetNames(file)
            nsheet <- length(sh_names)
            if (length(nummtd) == 1) {
                nummtd <- rep(nummtd, nsheet)
            }
            else if (length(nummtd) != nsheet){
                stop("Length of the argument 'nummtd' does not match the number of marker worksheets.", call. = FALSE)
            }
            if (length(transposed) == 1) {
                transposed <- rep(transposed, nsheet)
            }
            else if (length(transposed) != nsheet) {
                stop("Length of the argument 'transposed' does not match the number of marker worksheets.", call. = FALSE)
            }
            if (length(molecular) == 1) {
                molecular <- rep(molecular, nsheet)
                molecular <- as.list(molecular)
            }
            else if (length(molecular) != nsheet) {
                stop("The number of list elements in argument 'molecular' needs to be equal to the number of worksheets.", call. = FALSE)
            }
            if (length(coding) == 1){
                coding <- rep(coding, nsheet)
                coding <- as.list(coding)
            }
            else if (length(coding) != nsheet) {
                stop("The number of list elements in argument 'coding' needs to be equal to the number of worksheets.", call. = FALSE)
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
                if (transposed[n] == TRUE) {
                    #KS#  set_d <- as.matrix(XLConnect::readWorksheet(w_b, sheet = n, header = FALSE))
                    #KS#  set_d <- t(set_d)
                    set_d <- as.matrix(openxlsx::read.xlsx(file, sheet = n, colNames = FALSE))
                    set_d <- t(set_d)
                    colnames(set_d) <- set_d[1,]
                    alllabels <- set_d[1,][-1]
                    set_d <- set_d[-1,]
                    rownames(set_d) <- 1:nrow(set_d)
                    rw_col <- c("columns ", "column ", "row ", "rows ")
                }
                else if (transposed[n] == FALSE) {
                    #KS#  set_d <- as.matrix(XLConnect::readWorksheet(w_b, sheet = n))
                    #KS#  alllabels <- XLConnect::readWorksheet(w_b, sheet = n, startCol = 2, endCol = ncol(set_d),
                    #KS#                                        startRow = 1, endRow = 1, header = FALSE,
                    #KS#                                        autofitRow = FALSE, autofitCol = FALSE)
                    #KS#  alllabels <- unlist(alllabels)
                    set_d <- as.matrix(openxlsx::read.xlsx(file, sheet = n))
                    alllabels <- as.vector(colnames(set_d)[-1])
                    rw_col <- c("rows ", "row ", "column ", "columns ")
                }
                if ((nummtd[n] - 1) > ncol(set_d)) {
                    stop("Number of the metadata columns are bigger than the number of dataset columns in worksheet ", n, ".",
                         call. = FALSE)
                }
                if (prod(is.element(alllabels[(nummtd[n] - 1):(ncol(set_d) - 1)], NA)) == 1) {
                    stop("Marker labels should be specified in the first row",
                         call. = FALSE)
                }
                else if (prod(is.element(set_d[,1], NA) == 1)) {
                    stop("Sample IDs should be specified in the first column.",
                         call. = FALSE)
                }
                set_d[,1] <- trimws(set_d[,1])
                alllabels <- trimws(alllabels)
                setnoempty <- set_d
                emptylabels <- as.vector(alllabels[(nummtd[n] - 1):(ncol(set_d) - 1)])
                emptychecker <- withWarnings(moi_empty(set_d, setnoempty, nummtd[n], rw_col, multsheets, alllabels, molecular, molid, n))
                set_d <- emptychecker[[1]][[1]]
                setnoempty <- emptychecker[[1]][[2]]
                alllabels <- emptychecker[[1]][[3]]
                nummtd[n] <- emptychecker[[1]][[4]]
                general_warnings <- append(general_warnings, emptychecker[[2]])
                if (is.null(ncol(set_d)) == TRUE) {
                    set_d <- t(matrix(c(names(set_d), set_d), ncol = 2, nrow = length(set_d)))
                }
                else if (nrow(set_d) == 1) {
                    set_d <- t(matrix(c(colnames(set_d), set_d[1,]), ncol = 2, nrow = length(set_d)))
                }

                if (keepmtd == 1 && nummtd[n] > 2){
                    mtdall[[n]] <- alllabels[1:(nummtd[n] - 2)]
                }
                mlabels <- as.vector(alllabels[(nummtd[n] - 1):(ncol(set_d) - 1)])
                lab <- 0
                if (length(alllabels) == (nummtd[n] - 2)) {
                    lab <- lab + 1 + mark
                    deletelab <- append(deletelab, lab)
                    mlabels <- paste("emptymarker7-5m*529", lab)
                    set_d <- cbind(set_d, rep(NA, nrow(set_d)))
                }
                s_total_mark <- as.vector(set_d[, 1])
                samall <- append(samall, s_total_mark)
                if (is.na(set_d[1,1]) == TRUE || length(set_d[1,1]) == 0) {
                    stop("No sample ID is given in the first row in worksheet ", n, "." , call. = FALSE)
                }
                s_l <- moi_duplicatefinder(set_d[,1], "Sample ID ", 0, rw_col)
                if (s_l == 1) {
                    stop("Sample IDs need to be defined uniquely in worksheet ", n, ".", call. = FALSE)
                }
                if (is.na(alllabels[1]) == TRUE || length(alllabels[1]) == 0) {
                    stop("No marker label is given in the first column in worksheet ", n, ".", call. = FALSE)
                }
                m_l <- moi_duplicatefinder(mlabels, "Marker label ", nummtd[n] - 2, rev(rw_col))
                if (m_l == 1) {
                    stop("Marker labels need to be defined uniquely in worksheet ", n, ".", call. = FALSE)
                }
                multsh <- paste("In worksheet ", n, ":", sep = "")
                set_dinfo <- withWarnings(moi_administrator(set_d, s_total_mark, mlabels, nummtd[n], cha, rw_col, n,
                                                            transposed[n], multsh))
                general_warnings <- append(general_warnings, set_dinfo[[2]])
                set_dinfo <- set_dinfo[[1]]
                if(set_dinfo[[11]] == 1) {
                    stop("In worksheet ", n," (", sh_names[n], ") the dataset is simulataneously of two formats 'multiple columns per marker' and 'multiple rows per sample'.", call. = FALSE)
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
                lmoln <- length(molecular[[n]])
                lcodn <- length(coding[[n]])
                if (numarkwsh[n] > 1) {
                    if (lmoln == 1) {
                        molecular[[n]] <- rep(molecular[[n]], numarkwsh[n])
                    }
                    else if (lmoln != numarkwsh[n]) {
                        stop("The worksheet ", n, " (", sh_names[n], ") contains ", numarkwsh[n], " markers. ", "The corresponding 'molecular' argument element is not set properly.", call. = FALSE)
                    }
                    if (lcodn == 1) {
                        coding[[n]] <- rep(coding[[n]], numarkwsh[n])
                    }
                    else if (lcodn != numarkwsh[n]) {
                        if (lmoln != lcodn) {
                            warning("The arguments 'molecular' and 'coding' do not have the same size for worksheet ", n, ".", call. = FALSE)
                        }
                        stop("The worksheet ", n, " (", sh_names[n], ") contains ", numarkwsh[n], " markers. ", "The corresponding 'coding' argument element is not set properly.", call. = FALSE)
                    }
                }
                else if (lmoln > 1 && numarkwsh[n] == 1) {
                    stop("The worksheet ", n, " (", sh_names[n], ") contains only one marker. The corresponding 'molecular' argument element is not set properly.", call. = FALSE)
                }
                else if (lcodn > 1 && numarkwsh[n] == 1) {
                    stop("The worksheet ", n, " (", sh_names[n], ") contains only one marker. The corresponding 'coding' argument element is not set properly.", call. = FALSE)
                }
                for (j in 1:length(m)) {
                    mark <- mark + 1
                    alldata[[mark]] <- set_d[,c(1:(nummtd[n] - 1), sb:(sb + m[j] - 1))]
                    extrainfo[[mark]] <- list(samorder, samnames, rw_col, n, cons, conm)
                    sb <- sb + m[j]
                }
            }
            deletelab <- deletelab[-1]
            tempmolecular <- unlist(lapply(molecular, toupper))
            tempcoding <- unlist(lapply(coding, tolower))
            matchallmol <- match(tempmolecular, allmolecular)
            matchallcod <- unlist(lapply(1:length(tempcoding), function(x) is.element(tempcoding[x],allcoding[[matchallmol[x]]])))
            if (prod(matchallcod) == 0 && length(matchallcod) > 0) {
                stop("The argument 'coding' is not set properly. The coding classes ", paste(shQuote(tempcoding[!matchallcod], "sh"), collapse = " and "), " do not correspond to molecular data types ", paste(shQuote(tempmolecular[!matchallcod], "sh"), collapse = " and "), ", respectively.", call. = FALSE)
            }
            molecular <- tempmolecular
            coding <- tempcoding
            markerlabels <- unlist(markerlabels)
            if (anyDuplicated(markerlabels) > 0) {
                #stop("Marker labels need to be unique.", call. = FALSE)
            }
            samall <- samall[-1]
            samall <- samall[!is.na(samall)]
            samall <- unique(samall)
            lsam <- length(samall)
            ll <- matrix(0, mark, lsam)
            exporting <- list()
            tempmtd <- matrix(0, lsam, 1)
            prensh <- 0
            postnsh <- 1
            warnid <- 1
            for (mk in 1:mark) {
                datamark <- as.matrix(alldata[[mk]])
                samidsmark <- extrainfo[[mk]][[1]]
                samnamesmark <- extrainfo[[mk]][[2]]
                rw_col <- extrainfo[[mk]][[3]]
                nsh <- extrainfo[[mk]][[4]]
                cons <- extrainfo[[mk]][[5]]
                conm <- extrainfo[[mk]][[6]]
                roworders <- 0
                samidsmarksort <- match(samall,samnamesmark)
                samidsmarkna <- which(is.na(samidsmarksort) == TRUE)
                lsmna <- length(samidsmarkna)
                multsh <- paste("In worksheet ", nsh, ":", sep = "")
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
                samidsmarkrows <- matrix(c(samidsmarkorder, plus1order), 2, length(samidsmarkorder), byrow = TRUE)
                roworders <- lapply(1:ncol(samidsmarkrows), function(x) append(roworders, samidsmarkrows[1,x]:(samidsmarkrows[2,x] - 1)))
                roworders <- lapply(roworders, function(x) x[-1])
                roworders <- unlist(roworders)
                datamark <- datamark[roworders,]
                samidsmark <- moi_labels(datamark[,1])[[1]]
                if (prensh != nsh) {
                    warnid <- 0
                    if (keepmtd == 1 && nummtd[nsh] > 2) {
                        mtdnsh <- as.matrix(datamark[,2:(nummtd[nsh] - 1)])
                        mtdnsh <- withWarnings(moi_metadata(mtdnsh, mtdall[[nsh]], nummtd[nsh] - 2, samidsmark, samall, lsam, samidsmarkna, TRUE, multsh))
                        metadata_warnings <- append(metadata_warnings, mtdnsh[[2]])
                        mtdnsh <- mtdnsh[[1]]
                        warnid <- mtdnsh[[2]]
                        mtdnsh <- mtdnsh[[1]]
                        tempmtd <- cbind(tempmtd, mtdnsh)
                    }
                    prensh <- nsh
                }
                col_j <- as.matrix(datamark[, nummtd[nsh]:ncol(datamark)])
                result <- withWarnings(moi_marker(col_j, markerlabels[mk], samidsmark, samidsmarkorder, conm, cons,
                                                  molecular[mk], coding[mk], cha_num, cha_string, ambeguity_code,
                                                  represented_bases, aa_1, aa_2, let_3, amino_acid, aa_symbol,
                                                  compact, codon_s, rw_col, multsh))
                marker_warnings[[mk]] <- result[[2]]
                result <- result[[1]]
                warnid <- result[[2]] + warnid
                exporting[[mk]] <- result[[1]]
                ll[mk, ] <- unlist(lapply(result[[1]], length))

            }

            tempmtd <- tempmtd[,-1]
            if (keepmtd == 1) {
                metadata <- withWarnings(moi_mergemetadata(tempmtd, mtdall, samall, multsh))
                metadata_warnings <- append(metadata_warnings, metadata[[2]])
                metadata <- metadata[[1]]
                mtdlabels <- metadata[[1]]
                warnid <- metadata[[3]]
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
            if (length(deletelab) > 0) {
                pdata <- pdata[,-deletelab]
                markerlabels <- markerlabels[-deletelab]
            }
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
        "Storing warnings"
        allwarnings <- general_warnings
        allwarnings <- append(allwarnings, metadata_warnings)
        allwarnings <- append(allwarnings, marker_warnings)
        allwarnings <- unlist(allwarnings)
        if (is.null(keepwarnings) == FALSE) {
            if (length(allwarnings) == 0) {
                warning("There are no warnings!", call. = FALSE)
            }
            else {
                warndic <- moi_warning(keepwarnings, general_warnings, metadata_warnings, marker_warnings, markerlabels)
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
                warning("***NOTE: There were more than 50 warnings. Set argument 'keepwarnings = <PATH>' to export ALL warnings into an Excel file!***"
                        , call. = FALSE)

            }
            else {
                allwarnings <- unlist(allwarnings)
                for (k in 1:length(allwarnings)) {
                    warning(allwarnings[k], call. = FALSE)
                }
            }
        }
        if (is.null(export) == FALSE) {
            export <- moi_export(export, final)
            message(paste("The output dataset is in this directory: ", normalizePath(export, winslash = "/")))
        }
        final <- as.data.frame(final)
        final
    }

