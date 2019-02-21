#' Finds STRs in a single cell
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
#'
#' @return a list of following elements: 1) a vector of
#'   lineages found on a microsatellite marker in a single
#'   cell. Each element corresponds to one and only one
#'   lineage and it is free from any special symbol. 2) an
#'   identifier whose value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}}.
#'
#'
corrector_str <-
    function (y, c_l, r_w, conm, cons, cha_num, rw_col)
    {
        warnid <- 0
        y <- as.character(y)
        y_1 <- unlist(strsplit(y, ""))
        z_1 <- match(y_1, cha_num)
        z <- which(is.na(z_1) == F)
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
            if (conm == 0 && cons == 0 && l_z >= length(z_2)) {
                z_3 <- match(c(1, length(y_1)), z)
                z_3 <- z_3[!duplicated(z_3)]
                z_3 <- y_1[z[z_3[!is.na(z_3)]]]
                z_4 <- z[-1] - z[-l_z]
                z_4 <- y_1[z[which(z_4 == 1)]]
                z_3 <- c(z_3, z_4)
                if (length(z_3) > 1) {
                    tch <- "characters "
                }
                else {
                    tch <- "character "
                }
                warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(z_3, "sh"), collapse = " and "),
                        ".", call. = F, noBreaks. = T)
                warnid <- 1
            }
            if ((conm >= 1 || cons >= 1)) {
                if (l_z > 1) {
                    tch <- "characters "
                }
                else {
                    tch <- "character "
                }
                warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(y_1[z], "sh"), collapse = " and "),
                        ".", call. = F, noBreaks. = T)
                warnid <- 1
                if (length(z_2) > 1) {
                    warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains multiple entries. In case of multiple rows (or multiple columns) data, inserting several
   entries in one cell is not recommended.", call. = F, noBreaks. = F)
                }
            }
        }
        if (length(z_2) == 0) {
            z_2 <- NA
        }
        else {
          if (is.na(z_2) == F) {
            if (nchar(z_2) == 0) {
              z_2 <- NA
            }
          }
        }
        if (is.na(z_2[1]) == F){
          z_2 <- z_2[!duplicated(z_2)]
          z_num <- as.numeric(z_2)
          n_z <- nchar(z_2)
          zerostarter <- n_z - nchar(z_num)
          n1 <- n_z[n_z == 1]
          n4 <- n_z[n_z >= 4]
          if ((length(n1) > 0 || length(n4) > 0 || sum(zerostarter) > 0) && is.na(z_2) == F && ceiling(z_num) == z_num) {
            n1 <- z_2[n_z == n1]
            n4 <- z_2[n_z == n4]
            n0 <- z_2[zerostarter > 0]
            warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"),
                    " contains an unusual entry ", paste(shQuote(c(n1, n4, n0), "sh"), collapse = " and "),
                    ".", call. = F, noBreaks. = T)
            warnid <- 1
          }
        }
        list(z_2, warnid)
  }





#' Finds SNPs in a single cell
#'
#' @description This function is designed to find the
#'   lineages present on a SNP marker in a single cell.
#'   The function is equiped with several warnings which are
#'   properly set for different situations.
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
#'
#' @return a list of following elements: 1) a vector of
#'   lineages found on a SNP marker in a single cell. Each
#'   element corresponds to one and only one lineage and it
#'   is free from typos, 2) an identifier whose
#'   value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}}.
#'
corrector_snp <-
    function (y, c_l, r_w, conm, cons, cha_string, rw_col)
    {
        warnid <- 0
        y <- as.character(y)
        y_1 <- unlist(strsplit(y, ""))
        y_num <- which(is.numeric(y_1) == T)
        z_1 <- match(y_1, cha_string)
        b <- which(is.na(z_1) == T)
        d <- integer(0)
        lb <- length(b)
        z_2 <- NA
        if (lb > 0) {
            b <- matrix(c(b[-1], b[-lb]), (lb - 1), 2)
            d <- which(b[, 1] - b[, 2] == 1)
            if (length(d) > 0) {
                warning("The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains consecutive SNPs:", shQuote(y, "sh"), ".",
                        call. = F, noBreaks. = T)
                z_2 <- NA
                warnid <- 1
            }
            else {
                z <- which(is.na(z_1) == F)
                l_z <- length(z)
                if (l_z == 0) {
                    z_2 <- y_1
                }
                else {
                    z_2 <- y_1[is.na(z_1) == T]
                    if (length(z_2) == 0 || is.na(z_2) == T) {
                        z_2 <- NA
                    }
                    if (l_z >= length(z_2) - length(d) && conm == 0 && cons == 0) {
                        z_3 <- match(c(1, length(y_1)), z)
                        z_3 <- z_3[!duplicated(z_3)]
                        z_3 <- y_1[z[z_3[!is.na(z_3)]]]
                        z_4 <- z[-1] - z[-l_z]
                        z_4 <- y_1[z[which(z_4 == 1)]]
                        z_3 <- c(z_3, z_4)
                        if (length(z_3) > 1) {
                            tch <- "characters "
                        }
                        else {
                            tch <- "character "
                        }
                        warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(z_3, "sh"), collapse = " and "), ".",
                                call. = F, noBreaks. = T)
                        warnid <- 1
                    }
                    if ((conm >= 1 || cons >= 1)) {
                        if (l_z > 1) {
                            tch <- "characters "
                        }
                        else {
                            tch <- "character "
                        }
                        warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(y_1[z], "sh"), collapse = " and "),
                                ".", call. = F, noBreaks. = T)
                        warnid <- 1
                        if (length(z_2) > 1) {
                            warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains multiple entries. In case of multiple rows (or multiple columns) data, inserting several
   entries in one cell is not recommended.", call. = F, noBreaks. = T)
                            warnid <- 1
                        }
                    }
                }
            }
        }
        if (length(z_2) == 0) {
            z_2 <- NA
        }
        else {
          if (is.na(z_2) == F) {
            if (nchar(z_2) == 0) {
              z_2 <- NA
            }
          }
        }
        z_2 <- z_2[!duplicated(z_2)]
        list(z_2, warnid)
        }



#' Finds amino acids or codons in a single cell
#'
#' @description This function is designed to find the
#'   lineages present on a amino acid or codon marker in a
#'   single cell. The function is equiped with several
#'   warnings which are properly set for different
#'   situations.
#'
#' @inheritParams corrector_snp
#'
#' @return a list of following elements: 1) a vector of
#'   lineages found on a amino acid or codon marker in a
#'   single cell. Each element corresponds to one and only
#'   one lineage and it is free from typos. 2) an identifier
#'   whose value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}}.
#'
corrector_aminoacid <-
    function (y, c_l, r_w, conm, cons, cha_string, rw_col, coding)
    {
        warnid <- 0
        y <- as.character(y)
        y_1 <- unlist(strsplit(as.character(y), ""))
        z_1 <- match(y_1, cha_string)
        z <- which(is.na(z_1) == F)
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
            if (l_z >= length(z_2) && conm == 0 && cons == 0) {
                z_3 <- match(c(1, length(y_1)), z)
                z_3 <- z_3[!duplicated(z_3)]
                z_3 <- y_1[z[z_3[!is.na(z_3)]]]
                z_4 <- z[-1] - z[-l_z]
                z_4 <- y_1[z[which(z_4 == 1)]]
                z_3 <- c(z_3, z_4)
                if (length(z_3) > 1) {
                    tch <- "characters "
                }
                else {
                    tch <- "character "
                }
                warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(z_3, "sh"), collapse = " and "),
                        ".", call. = F, noBreaks. = T)
                warnid <- 1
            }
            if ((conm >= 1 || cons >= 1)) {
                if (l_z > 1) {
                    tch <- "characters "
                }
                else {
                    tch <- "character "
                }
                warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains the unexpected ", tch, paste(shQuote(y_1[z], "sh"), collapse = " and "),
                        ".", call. = F, noBreaks. = T)
                warnid <- 1
                if (length(z_2) > 1) {
                    warning("The cell in ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains multiple entries. In case of multiple rows (or multiple columns) data, inserting several
   entries in one cell is not recommended.", call. = F, noBreaks. = T)
                    warnid <- 1
                }
            }
        }
        z_2 <- z_2[!duplicated(z_2)]
        if (length(z_2) == 0) {
            z_2 <- NA
        }
        else {
          if (is.na(z_2) == F) {
            if (nchar(z_2) == 0) {
              z_2 <- NA
            }
          }
        }
        if (coding == '1let') {
           lz <- length(z_2)
           z_2 <- unlist(strsplit(as.character(z_2), ""))
           if (length(z_2) > lz) {
             warning("The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains consecutive amino acids:", shQuote(y, "sh"), ".",
                     call. = F, noBreaks. = T)
           }

        }
        list(z_2, warnid)
    }



