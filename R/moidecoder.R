#' Transforms entries to the desired coding class
#'
#' @description Transforms the data entries in a cell to a
#'   pre-specified coding class.
#'
#' @param y numeric vector; entries in a cell corresponding
#'   to a specific sample and a specific marker.
#' @param c_l string; marker label.
#' @param r_w numeric; sample ID's row number in the excel
#'   file.
#' @param coding string; coding class of the molecular
#'   marker.
#' @param rw_col string vector; variable used to switch
#'   between row and column in case of transposed data.
#'   Namely, \code{c("rows ", "row ", "column ", "columns
#'   ")}.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a list of two elements: 1) a vector of STR
#'   entries in pre-specified coding class. 2) an identifier
#'   whose value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}}.
#'
decoder_str <- function(y, c_l, r_w, coding, rw_col, multsh)
{
    warnid <- 0
    real_y <- 0
    for (i in 1:length(y)) {
        if (is.na(y[i]) == FALSE) {
            x <- as.numeric(y[i])
            if (coding == 'nearest'){
                x <- floor(x) + ((x - floor(x)) > 0.5) * 1
                real_y <- append(real_y, x)
            }
            else if (coding == 'floor') {
                x <- floor(x)
                real_y <- append(real_y, x)
            }
            else if (coding == 'ceil') {
                x <- ceiling(x)
                real_y <- append(real_y, x)
            }
            else if(coding == 'integer') {
                if (floor(x) != x) {
                    warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains a real-valued entry: ", shQuote(x, "sh"), ". The coding is set as 'integer'. ",
                            call. = FALSE)
                    warnid <- 1
                }
                real_y <- append(real_y, x)
            }
        }
        else {
          real_y <- append(real_y, NA)
        }
    }
    real_y <- real_y[-1]
    list(real_y, warnid)
  }




#############################################################################################
#############################################################################################


#' Converts ambeguity codes to represented bases
#'
#' @description Translates the nucleotide ambiguity codes as
#'   defined in DNA Sequence Assembler from a pre-specified
#'   coding class to 4-letter codes.
#'
#' @inheritParams decoder_str
#' @param ambeguity_code string vector; ambeguity codes for
#'   snp data.
#' @param represented_bases string vector; represented bases
#'   for those ambeguity codes.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a list of two elements: 1) a vector of
#'   represented bases on a marker corresponding to a sample
#'   in pre-specified coding class. 2) an identifier whose
#'   value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}} and
#'   \code{\link{corrector_string}}. See also the vignette
#'   'StandardAmbiguityCodes'.
#'
decoder_snp <-
    function (y, c_l, r_w, ambeguity_code, represented_bases, coding, rw_col, multsh)
    {
        warnid <- 0
        y <- toupper(y)
        real_y <- 0
        for (i in 1:length(y)) {
            if (is.na(y[i]) == FALSE) {
                p_real <- match(y[i], ambeguity_code)
                if (is.na(p_real) == FALSE) {
                    if (coding == 'iupac') {
                        real_y <- append(real_y, represented_bases[[p_real]])
                    }
                    else if (coding == '4let') {
                        if (is.element(p_real, 1:4) == FALSE) {
                            warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an entry (", shQuote(y[i], "sh"), ") incompatible with SNP data with coding class '4let' (four-letter code). SNP data in coding class '4let' needs to be coded only with four letters 'A', 'C', 'G' and 'T' (either in upper or lowercase).",
                                    call. = FALSE)
                            warnid <- 1
                            real_y <- append(real_y, y[i])
                        }
                        else {
                            real_y <- append(real_y, y[i])
                        }
                    }
                }
                else {
                    warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an unidentified entry: ", shQuote(y[i], "sh"), ".", call. = FALSE)
                    real_y <- append(real_y, NA)
                    warnid <- 1
                }
            }
            else {
                real_y <- append(real_y, NA)
            }
        }
        real_y <- real_y[-1]
        list(real_y, warnid)
        }



#############################################################################################
#############################################################################################


#' Translates the standard ambiguity codes for nucleotides
#' (amino acid decoder)
#'
#' @description Translates the standard ambiguity codes for
#'   nucleotides in amino acid forms from a pre-specified
#'   coding class to 3-letter designation of amino acids.
#'
#' @inheritParams decoder_str
#' @param aa_1 string vector; vector of different amino
#'   acids.
#' @param aa_2 string vector; vector of different codons.
#' @param let_3 string vector; vector of amino acids in 3
#'   letter designation.
#' @param amino_acid string vector; vector of amino acids in
#'   full name.
#' @param aa_symbol string vector; vector of amino acids in
#'   one letter designation.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a list of two elements: 1) a vector of 3-letter
#'   designation of amino acids on a marker corresponding to
#'   a sample in pre-specified coding class. 2) an identifier
#'   whose value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}} and
#'   \code{\link{corrector_string}}. See also the
#'   vignette 'StandardAmbiguityCodes'.
#'
decoder_aminoacid <-
    function (y, c_l, r_w, aa_1, aa_2, let_3, amino_acid, aa_symbol,
              coding, rw_col, multsh)
    {
        warnid <- 0
        y <- toupper(y)
        real_y <- 0
        for (i in 1:length(y)) {
            if (is.na(y[i]) == FALSE) {
                l_y <- nchar(y[i])
                if (is.element(y[i], aa_1) == TRUE) {
                    if (coding == '1let') {
                        if (l_y > 1) {
                            warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an amino entry (",shQuote(y[i], "sh"),") incompatible with user-defined coding class '1let'.",
                                    call. = FALSE)
                            warnid <- 1
                            real_y <- append(real_y, y[i])
                        }
                        else {
                            p_real <- match(y[i], aa_symbol)
                            real_y <- append(real_y, let_3[p_real])
                        }
                    }
                    else if (coding == 'full') {
                        if (l_y < 6) {
                            warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an amino entry (",shQuote(y[i], "sh"),") incompatible with user-defined coding class 'full'.",
                                    call. = FALSE)
                            warnid <- 1
                            real_y <- append(real_y, y[i])
                        }
                        else {
                            p_real <- match(y[i], amino_acid)
                            real_y <- append(real_y, let_3[p_real])
                        }
                    }
                    else if (coding == '3let') {
                        if (l_y != 3) {
                            warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an amino entry (",shQuote(y[i], "sh"),") incompatible with user-defined coding class '3let'.",
                                    call. = FALSE)
                            warnid <- 1
                            real_y <- append(real_y, y[i])
                        }
                        else {
                            real_y <- append(real_y, y[i])
                        }
                    }
                }
                else if (is.element(y[i], aa_2) == TRUE) {
                    warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains a codon entry: ", shQuote(y[i], "sh"), ". The argument molecular for this marker is set as 'amino' by user.",
                            call. = FALSE)
                    warnid <- 1
                    real_y <- append(real_y, y[i])
                }
                else {
                    real_y <- append(real_y, NA)
                    warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an unidentified entry: ", shQuote(y[i], "sh"), ".",
                            call. = FALSE)
                    warnid <- 1
                }
            }
            else {
                real_y <- append(real_y, NA)
            }
        }
        real_y <- real_y[-1]
        list(real_y, warnid)
    }



#############################################################################################
#############################################################################################


#' Translates the standard ambiguity codes for nucleotides
#' (codon decoder)
#'
#' @description Translates the standard ambiguity codes for
#'   nucleotides in codon form from a pre-specified coding
#'   class to triplet designation of codons.
#'
#' @inheritParams decoder_str
#' @param aa_1 string vector; vector of different amino
#'   acids.
#' @param aa_2 string vector; vector of different codons.
#' @param compact string vector; vector of different codons
#'   in compact form.
#' @param codon_s string vector; vector of different codons.
#' @param multsh string; reports warnings for multiple
#' worksheet datasets.
#'
#' @return a list of two elements: 1) a vector of codons in
#'   triplet designation on a marker corresponding to a
#'   sample in pre-specified coding class. 2) an identifier
#'   whose value is 1 if a warning takes place.
#'
#' @keywords internal
#'
#' @seealso For further details see: \code{\link{moimport}},
#'   \code{\link{moi_marker}} and
#'   \code{\link{corrector_string}}.
#'
decoder_codon <-
    function (y, c_l, r_w, aa_1, aa_2, compact, codon_s, coding, rw_col, multsh)
    {
        warnid <- 0
        y <- toupper(y)
        real_y <- 0
        for (i in 1:length(y)) {
            if (is.na(y[i]) == FALSE) {
                l_y <- nchar(y[i])
                if (is.element(y[i], aa_1) == TRUE) {
                    warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an amino entry: ", shQuote(y[i], "sh"), ". The argument molecular for this marker is set as 'codon' by user.",
                            call. = FALSE)
                    warnid <- 1
                    real_y <- append(real_y, y[i])
                }
                else if (is.element(y[i], aa_2) == TRUE) {
                    p_real <- match(y[i], compact)
                    if (coding == 'compact') {
                        if (is.na(p_real) == FALSE) {
                            real_y <- append(real_y, codon_s[[p_real]])
                        }
                        else {
                            warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains a codon entry (",shQuote(y[i], "sh"),") incompatible with user-defined coding class 'compact'.",
                                    call. = FALSE)
                            warnid <- 1
                            real_y <- append(real_y, y[i])
                        }
                    }
                    else if (coding == 'triplet') {
                        if (is.na(p_real) == TRUE) {
                            real_y <- append(real_y, y[i])
                        }
                        else {
                            warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains a codon entry (",shQuote(y[i], "sh"),") incompatible with user-defined coding class 'triplet'.",
                                    call. = FALSE)
                            warnid <- 1
                            real_y <- append(real_y, y[i])
                        }
                    }
                }
                else {
                    warning(multsh, " The cell on ", rw_col[2], r_w, " and marker ", shQuote(c_l, "sh"), " contains an unidentified entry: ", shQuote(y[i], "sh"), ".",
                            call. = FALSE)
                    warnid <- 1
                    real_y <- append(real_y, NA)
                }
            }
            else{
                real_y <- append(real_y, NA)
            }
        }
        real_y <- real_y[-1]
        list(real_y, warnid)
    }

