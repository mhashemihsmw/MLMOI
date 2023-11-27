#' Removes punctuation characters and typos from data
#' entries
#'
#' @description This function is designed to find the
#'   lineages (STRs) present on a microsatellite marker in a
#'   single cell.
#'
#' @param y string; a cell entry.
#' @param c_l string; marker label.
#' @param r_w numeric; sample ID's row number in the excel
#'   file.
#' @param conm numeric; the multiple column per marker
#'   identifier. For the data of format multiple columns
#'   conm > 1.
#' @param cons numeric; the multiple row per sample
#'   identifier. For the data of format multiple rows cons >
#'   1.
#' @param cha_num string vector; the vector of punctuation
#'   characters plus alphabets. See
#'   \code{\link{moi_prerequisite}}.
#' @param rw_col string vector; variable used to switch
#'   between row and column in case of transposed data.
#'   Namely, \code{c("rows ", "row ", "column ", "columns
#'   ")}.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a list of following elements: 1) a vector of
#'   lineages found on a microsatellite marker in a single
#'   cell. Each element corresponds to one and only one
#'   lineage and it is free from any punctuation character.
#'   2) an identifier whose value is 1 if a warning takes
#'   place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}}.
#'
#'
corrector_numeric <-
    function (y, c_l, r_w, conm, cons, cha_num, rw_col, multsh)
    {
        warnid <- 0
        y <- as.character(y)
        y_1 <- unlist(strsplit(y, ""))
        z_1 <- match(y_1, cha_num)
        z <- which(is.na(z_1) == FALSE)
        l_z <- length(z)
        if (l_z == 0) {
            z_2 <- y
        }
        else {
            y_2 <- y_1
            y_2[z] <- " "
            y_2 <- paste(y_2, collapse = "")
            y_2 <- unlist(strsplit(y_2, " "))
            y_3 <- nchar(y_2)
            z_2 <- as.character(y_2[which(y_3 > 0)])
            z_5 <- which(z_1 > 32)
            if (conm == 0 && cons == 0 && (l_z + length(z_5)) >= length(z_2)) {
                z_3 <- match(c(1, length(y_1)), z)
                z_3 <- z_3[!duplicated(z_3)]
                z_3 <- y_1[z[z_3[!is.na(z_3)]]]
                z_4 <- z[-1] - z[-l_z]
                z_4 <- y_1[z[which(z_4 == 1)]]
                z_3 <- c(z_3, z_4)
                if(length(z_5)) {
                    z_5 <- cha_num[z_1[z_5]]
                    z_3 <- append(z_3, z_5)
                    z_3 <- z_3[!duplicated(z_3)]
                }
                if (length(z_3) > 1) {
                    tch <- "characters "
                }
                else {
                    tch <- "character "
                }
                warning(multsh, " The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(z_3, "sh"), collapse = " and "),
                        ".", call. = FALSE, noBreaks. = TRUE)
                warnid <- 1
            }
            if ((conm >= 1 || cons >= 1)) {
                if (l_z > 1) {
                    tch <- "characters "
                }
                else {
                    tch <- "character "
                }
                warning(multsh, " The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(y_1[z], "sh"), collapse = " and "),
                        ".", call. = FALSE, noBreaks. = TRUE)
                warnid <- 1
                if (length(z_2) > 1) {
                    warning(multsh, " The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains multiple entries. In case of multiple rows (or multiple columns) data, inserting several entries in one cell is not recommended.",
                            call. = FALSE, noBreaks. = FALSE)
                }
                }
            }
        if (length(z_2) == 0 ) {
            z_2 <- NA
        }
        else {
            if (is.na(z_2[1]) == FALSE) {
                if (nchar(z_2[1]) == 0) {
                    z_2 <- NA
                }
                else {
                    z_num_2 <- unlist(lapply(z_2, function(x) strsplit(x, "[.]")[[1]][1]))
                    z_num <- floor(as.numeric(z_2))
                    z_real <- as.numeric(z_2)
                    zerostarter <- which(nchar(z_num_2) != nchar(z_num))
                    n_z <- nchar(z_num)
                    n_z_2 <- nchar(z_2)
                    n0 <- z_2[zerostarter]
                    n1 <- z_2[n_z == 1]
                    n4 <- z_2[n_z >= 4]
                    if (length(n1) > 0 || length(n4) > 0 || length(zerostarter) > 0) {
                        khata <- c(n0, n1, n4)
                        khata <- khata[!duplicated(khata)]
                        warning(multsh, " The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an unusual entry ", paste(shQuote(khata, "sh"), collapse = " and "), ".",
                                call. = FALSE, noBreaks. = TRUE)
                        warnid <- 1
                        z_2 <- as.character(z_real)
                    }
                }
            }
        }
        z_2 <- z_2[!duplicated(z_2)]
        list(z_2, warnid)
        }




#############################################################################################
#############################################################################################



#' Removes punctuation characters and typos from data
#' entries
#'
#' @description This function is designed to find the
#'   lineages present on a SNP, amino-acid and codon marker
#'   in a single cell.
#'
#' @param y string; entry of a cell.
#' @param c_l string; marker label.
#' @param r_w numeric; sample ID's row number in the excel
#'   file.
#' @param conm numeric; the multiple column per marker
#'   identifier. For the data of format multiple columns
#'   conm > 1.
#' @param cons numeric; the multiple row per sample
#'   identifier. For the data of format multiple rows cons >
#'   1.
#' @param cha_string string vector; the vector of
#'   punctuation characters plus numerics form 1 to 9. See
#'   \code{\link{moi_prerequisite}}.
#' @param rw_col string vector; variable used to switch
#'   between row and column in case of transposed data.
#'   Namely, \code{c("rows ", "row ", "column ", "columns
#'   ")}.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a list of following elements: 1) a vector of
#'   lineages found on a marker (SNP, amino-acid or codon)
#'   in a single cell. Each element corresponds to one and
#'   only one lineage and it is free from typos, 2) an
#'   identifier whose value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}}.
#'
corrector_string <-
    function (y, c_l, r_w, conm, cons, cha_string, rw_col, coding, multsh)
    {
        warnid <- 0
        y <- as.character(y)
        y_1 <- unlist(strsplit(y, ""))
        z_1 <- match(y_1, cha_string)
        z <- which(is.na(z_1) == FALSE)
        l_z <- length(z)
        if (l_z == 0) {
            z_2 <- y
        }
        else if (length(y) == 0) {
            z_2 <- NA
        }
        else {
            y_2 <- y_1
            y_2[z] <- " "
            y_2 <- paste(y_2, collapse = "")
            y_2 <- unlist(strsplit(y_2, " "))
            y_3 <- nchar(y_2)
            z_2 <- as.character(y_2[which(y_3 > 0)])
            z_2 <- as.character(z_2[z_2 > 0])
            z_5 <- which(z_1 > 32)
            if ((l_z + length(z_5)) >= length(z_2) && conm == 0 && cons == 0) {
                z_3 <- match(c(1, length(y_1)), z)
                z_3 <- z_3[!duplicated(z_3)]
                z_3 <- y_1[z[z_3[!is.na(z_3)]]]
                z_4 <- z[-1] - z[-l_z]
                z_4 <- y_1[z[which(z_4 == 1)]]
                z_3 <- c(z_3, z_4)
                if(length(z_5)) {
                    z_5 <- cha_string[z_1[z_5]]
                    z_3 <- append(z_3, z_5)
                    z_3 <- z_3[!duplicated(z_3)]
                }
                if (length(z_3) > 1) {
                    tch <- "characters "
                }
                else {
                    tch <- "character "
                }
                warning(multsh, " The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(z_3, "sh"), collapse = " and "),
                        ".", call. = FALSE, noBreaks. = TRUE)
                warnid <- 1
            }
            if ((conm >= 1 || cons >= 1)) {
                if (l_z > 1) {
                    tch <- "characters "
                }
                else {
                    tch <- "character "
                }
                warning(multsh, " The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(y_1[z], "sh"), collapse = " and "),
                        ".", call. = FALSE, noBreaks. = TRUE)
                warnid <- 1
                if (length(z_2) > 1) {
                    warning(multsh, " The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains multiple entries. In case of multiple rows (or multiple columns) data, inserting several entries in one cell is not recommended.",
                            call. = FALSE, noBreaks. = TRUE)
                    warnid <- 1
                }
            }
        }
        z_2 <- z_2[!duplicated(z_2)]
        if (length(z_2) == 0) {
            z_2 <- NA
        }
        else if (length(z_2) == 1) {
            if (is.na(z_2) == FALSE) {
                if (nchar(z_2) == 0) {
                    z_2 <- NA
                }
            }
        }
        if (is.element(coding, c('1let', 'iupac', '4let')) == T) {
            lz <- length(z_2)
            z_2 <- unlist(strsplit(as.character(z_2), ""))
            if (length(z_2) > lz) {
                if (coding == '1let') {
                    mol <- "amino acids: "
                }
                else {
                    mol <- "SNPs: "
                }
                warning(multsh, " The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains consecutive ", mol, shQuote(y, "sh"), ".",
                        call. = FALSE, noBreaks. = TRUE)
            }
        }
        list(z_2, warnid)
    }
