## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- out.width='70%', include=TRUE, fig.align='center', echo = F--------
library(MLMOI)
fig <- system.file("extdata/fig", "basic.png", package = "MLMOI")
knitr::include_graphics(fig, auto_pdf = TRUE)

## ------------------------------------------------------------------------
infile <- system.file("extdata", "testDatabasic.xlsx", package = "MLMOI")
Outfile <- moimport(file = infile)

## ------------------------------------------------------------------------
head(Outfile)

## ---- eval = FALSE-------------------------------------------------------
#  Outfile <- moimport(infile, export = "testDatabasicOut.xlsx")

## ---- out.width='80%', include=TRUE, fig.align='center', echo = FALSE----
library(MLMOI)
fig <- system.file("extdata/fig", "metadata.png", package = "MLMOI")
knitr::include_graphics(fig, auto_pdf = TRUE)

## ------------------------------------------------------------------------
infile <- system.file("extdata", "testDatametadata.xlsx", package = "MLMOI")
Outfile <- moimport(infile, nummtd = 3, keepmtd = TRUE)

## ------------------------------------------------------------------------
head(Outfile)

## ---- out.width='80%',include=TRUE, fig.align='center', echo = FALSE-----
library(MLMOI)
fig <- system.file("extdata/fig", "multi-type.png", package = "MLMOI")
knitr::include_graphics(fig, auto_pdf = TRUE)

## ------------------------------------------------------------------------
infile <- system.file("extdata", "testDatamt.xlsx", package = "MLMOI")
Outfile <- moimport(infile, nummtd = 1, molecular = c('str','amino','snp','snp','amino','codon'))

## ------------------------------------------------------------------------
head(Outfile)

## ---- out.width='80%',include=TRUE, fig.align='center', echo = FALSE-----
library(MLMOI)
fig <- system.file("extdata/fig", "multi-type-coding.png", package = "MLMOI")
knitr::include_graphics(fig, auto_pdf = TRUE)

## ------------------------------------------------------------------------
infile <- system.file("extdata", "testDatamtc.xlsx", package = "MLMOI")
Outfile <- moimport(infile, nummtd = 1,
         molecular = c('snp', 'str', 'str', 'amino', 'snp', 'codon'),
         coding = c('iupac', 'integer', 'nearest', 'full', '4let', 'triplet'))

## ------------------------------------------------------------------------
tail(Outfile)

## ----include=TRUE, out.width = '80%', fig.align='center', fig.show = 'hold', echo = FALSE----
library(MLMOI)
fig <- system.file("extdata/fig", "complex.png", package = "MLMOI")

knitr::include_graphics(fig)


## ------------------------------------------------------------------------
infile <- system.file("extdata", "testDatacomplex.xlsx", package = "MLMOI")
Outfile <- moimport(infile, multsheets = TRUE, keepmtd = TRUE, nummtd = c(0, 2, 2, 0), transposed = c(FALSE, TRUE, FALSE, FALSE),
         molecular = list(c('str', 'str', 'snp', 'snp'),
                          c('amino', 'codon', 'snp', 'amino', 'codon'),
                          c('codon', 'str'),
                          c('str', 'amino', 'snp', 'str')),
         coding = list(rep(c('integer', '4let'), each = 2), 
                       c('1let', 'triplet', 'iupac', '3let', 'triplet'),
                       c('triplet', 'integer'),
                       c('integer', '1let', 'iupac', 'integer')))

## ------------------------------------------------------------------------
head(Outfile, n = 7)

## ----include=TRUE, out.width = '80%', fig.align='center', fig.show = 'hold', echo = FALSE----
library(MLMOI)
fig <- system.file("extdata/fig", "merge.png", package = "MLMOI")
knitr::include_graphics(fig)

## ------------------------------------------------------------------------
infile1 <- system.file("extdata", "testDatamerge1.xlsx", package = "MLMOI")
infile2 <- system.file("extdata", "testDatamerge2.xlsx", package = "MLMOI")
Outfile <- moimerge(infile1, infile2, nummtd1 = 1, nummtd2 = 2, keepmtd = TRUE)


## ------------------------------------------------------------------------
head(Outfile, n = 6)

## ------------------------------------------------------------------------
infile <- system.file("extdata", "testDatametadata.xlsx", package = "MLMOI")
mle <- moimle(file = infile, nummtd = 3)
mle$locus1

## ------------------------------------------------------------------------
infile <- system.file("extdata", "testDatamerge2.xlsx", package = "MLMOI")
moimle(file = infile, nummtd = 2, bounds = c(1.5, 2))

