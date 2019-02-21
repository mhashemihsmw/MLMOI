#' Extracts lineages of samples at a specific marker
#'
#' @description For a specific marker, the function goes
#'   from one sample to another and finds lineages with the
#'   help of the following functions:
#'   \code{\link{corrector_str}} along with
#'   \code{\link{decoder_str}},
#'   \code{\link{corrector_aminoacid}} along with
#'   \code{\link{decoder_aminoacid}} and
#'   \code{\link{corrector_snp}} along with
#'   \code{\link{decoder_snp}}. Each of these functions are
#'   suitable for a particular type of molecular data.
#'
#' @param col_j vector; column vector of a specific marker.
#' @param c_l string; marker label.
#' @param sam numeric vector; vector which its elements
#'   specify where a new sample starts.
#' @param conm numeric; the multiple column identifier. For
#'   the data of format multiple columns conm > 1.
#' @param cons numeric; the multiple row identifier. For the
#'   data of format multiple rows conm > 1.
#' @param molecular string; type of molecular data.
#' @param cha_num string vector; vector of symbols (used for
#'   microsatellite data).
#' @param cha_string string vector; vector of symbols (used
#'   for snp and amino acid).
#' @param ambeguity_code string vector; ambeguity codes for
#'   snp data.
#' @param represented_bases string vector; represented bases
#'   for those ambeguity codes.
#' @param aa_1 string vector; vector of different amino
#'   acids.
#' @param aa_2 string vector; vector of different codons.
#' @param let_3 string vector; vector of amino acids in
#'   3-letter designation.
#' @param amino_acid string vector; vector of amino acids in
#'   full name.
#' @param aa_symbol string vector; vector of amino acids in
#'   one letter designation.
#' @param compact string vector; vector of different codons
#'   in compact form.
#' @param codon_s string vector; vector of different codons.
#' @param samorder a vector which its elements specify where
#'   a new sample starts.
#' @param coding string; coding class of the molecular
#'   marker.
#' @param rw_col string vector; variable used to switch
#'   between row and column in case of transposed data.
#'   Namely, \code{c("rows ", "row ", "column ", "columns
#'   ")}.
#'
#' @return a list with the following elements: 1) a list
#'   with elements containing lineages for a specific sample
#'   on a specific marker. The order in which samples are
#'   entered in dataset is preserved in the list. The
#'   lineages are free from typos and are transformed to
#'   pre-specified coding class, 2) an identifier whose
#'   value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details, please see the following
#'   functions: \code{\link{moimport}}
#'
#'
moi_marker <-
    function (col_j, c_l, sam, samorder, conm, cons, molecular, coding, cha_num, cha_string,
              ambeguity_code, represented_bases, aa_1, aa_2, let_3, amino_acid,
              aa_symbol, compact, codon_s, rw_col)
    {
        warnid <- 0
        lsam <- length(sam)
        col_j <- as.matrix(col_j)
        out <- list()
        for (j in 1:(lsam - 1)) {
            sam_i <- col_j[(sam[j]):(sam[j + 1] - 1),]
            host <- 0
            r_w <- samorder[j] + 1
            for (k in 1:length(sam_i)) {
                if (is.na(sam_i[k]) == F) {
                    if (molecular == 'STR') {
                        x <- corrector_str(sam_i[k], c_l, r_w, conm, cons, cha_num, rw_col)
                        warnid <- x[[2]] + warnid
                        x <- decoder_str(x[[1]], c_l, r_w, coding, rw_col)
                        warnid <- x[[2]] + warnid
                    }
                    else if (molecular == 'SNP') {
                        x <- corrector_snp(sam_i[k], c_l, r_w, conm, cons, cha_string, rw_col)
                        warnid <- x[[2]] + warnid
                        x <- decoder_snp(x[[1]], c_l, r_w, ambeguity_code, represented_bases, coding, rw_col)
                        warnid <- x[[2]] + warnid

                    }
                    else if (molecular == 'AMINO') {
                        x <- corrector_aminoacid(sam_i[k], c_l, r_w, conm, cons, cha_string, rw_col, coding)
                        warnid <- x[[2]] + warnid
                        x <- decoder_aminoacid(x[[1]], c_l, r_w, aa_1, aa_2, let_3, amino_acid, aa_symbol, coding, rw_col)
                        warnid <- x[[2]] + warnid
                    }
                    else if (molecular == 'CODON') {
                        x <- corrector_aminoacid(sam_i[k], c_l, r_w, conm, cons, cha_string, rw_col, coding)
                        warnid <- x[[2]] + warnid
                        x <- decoder_codon(x[[1]], c_l, r_w, aa_1, aa_2, compact, codon_s, coding, rw_col)
                        warnid <- x[[2]] + warnid
                    }
                    host <- append(host, x[[1]])
                }
                else {
                    host <- append(host, NA)
                }
                if (cons > 1) {
                    r_w <- r_w + 1
                }
            }
            host <- host[-1]
            host <- host[!is.na(host)]
            host <- host[!duplicated(host)]
            if(length(host) == 0) {
              host <- NA
            }
            out[[j]] <- host
            r_w <- r_w + 1
        }
        list(out, warnid)
    }






