#'MLMOI: An R Package to preprocess molecular data and
#'derive prevalences, frequencies and multiplicity of
#'infection (MOI)
#'
#'The MLMOI package provides three functions: \itemize{
#'\item moimport(); \item moimle(); \item moimerge().}
#'
#'The package reaches out to scientists that seek to
#'estimate MOI and lineage frequencies at molecular markers
#'using the maximum-likelihood method described in
#'\insertCite{Schneider2018}{MLMOI},
#'\insertCite{SchneiderEscalante2018}{MLMOI} and
#'\insertCite{SchneiderEscalante2014}{MLMOI}. Users can
#'import data from Excel files in various formats, and
#'perform maximum-likelihood estimation on the imported data
#'by the package's \code{moimle()} function.
#'
#'@section Types of molecular data: Molecular data can be of
#'  types: \itemize{ \item microsatellite repeats (STRs);
#'  \item single nucleotide polymorphisms (SNPs); \item
#'  amino acids; \item codons (base triplets). }
#'
#'
#'@section Import function: The function \code{moimport()}, is
#'  designed to import molecular data. It imports molecular
#'  data in various formats and transforms them into a
#'  standard format.
#'
#'@section Merging Datasets: Two datasets in standard format
#'  can be merged with the function \code{moimerge()}.
#'
#'@section Estimation MOI and frequencies: The function
#'  \code{moimle()} is designed to derive MLE from molecular
#'  data in standard format.
#'
#'@references \insertRef{Schneider2018}{MLMOI}
#'
#'  \insertRef{SchneiderEscalante2018}{MLMOI}
#'
#'  \insertRef{SchneiderEscalante2014}{MLMOI}
#'
#'@importFrom Rdpack reprompt
#'
#'@docType package
#'@name MLMOI
NULL
