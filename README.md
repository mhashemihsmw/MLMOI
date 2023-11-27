MLMOI: An R Package to preprocess molecular data and derive prevalences, frequencies and multiplicity of infection (MOI)

Description

The MLMOI package provides three functions:

moimport();

moimle();

moimerge().

Details

The package reaches out to scientists that seek to estimate MOI and lineage frequencies at molecular markers using the maximum-likelihood method described in (Schneider 2018), (Schneider and Escalante 2018) and (Schneider and Escalante 2014). Users can import data from Excel files in various formats, and perform maximum-likelihood estimation on the imported data by the package's moimle() function.

Types of molecular data

Molecular data can be of types:

microsatellite repeats (STRs);

single nucleotide polymorphisms (SNPs);

amino acids;

codons (base triplets).

Import function

The function moimport(), is designed to import molecular data. It imports molecular data in various formats and transforms them into a standard format.

Merging Datasets

Two datasets in standard format can be merged with the function moimerge().

Estimation MOI and frequencies

The function moimle() is designed to derive MLE from molecular data in standard format.

References

Schneider KA (2018). “Large and finite sample properties of a maximum-likelihood estimator for multiplicity of infection.” PLOS ONE, 13(4), 1–21. doi: 10.1371/journal.pone.0194148, https://doi.org/10.1371/journal.pone.0194148.

Schneider KA, Escalante AA (2018). “Correction: A Likelihood Approach to Estimate the Number of Co-Infections.” PLOS ONE, 13(2), 1–3. doi: 10.1371/journal.pone.0192877, https://doi.org/10.1371/journal.pone.0192877.

Schneider KA, Escalante AA (2014). “A Likelihood Approach to Estimate the Number of Co-Infections.” PLoS ONE, 9(7), e97899. doi: 10.1371%2Fjournal.pone.0097899, https://doi.org/10.1371%2Fjournal.pone.0097899.
