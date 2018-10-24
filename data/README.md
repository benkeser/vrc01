# Data for "Prediction of VRC01 neutralization sensitivity by HIV-1 gp160 sequence features"

**Authors:** Craig A. Magaret, David C. Benkeser, Brian D. Williamson, Bhavesh R. Borate, Lindsay N. Carpp, Ivelin S. Georgiev, Ian Setliff, Adam S. Dingens, Noah Simon, Marco Carone, David Montefiori, Galit Alter, Wen-Han Yu, Michal Juraska, Paul T. Edlefsen, Shelly Karuna, Nyaradzo M. Mgodi, Srilatha Edugupanti Peter B. Gilbert 

-----

## Introduction

A secondary objective of the Antibody Mediated Prevention (AMP) prevention efficacy trials (HVTN 704/HPTN 085 and HVTN 703/HPTN 081) assesses how prevention efficacy depends on proteomic characteristics of HIV-1 (i.e., genotypic sieve analysis).  The primary genotypic sieve analysis assesses how prevention efficacy depends on a genotypic resistance score that measures how sensitive an HIV-1 genotype is to neutralization by VRC01.  The purpose of this README file and accompanying data set is to develop this genotypic resistance score (or a few scores) for use in the sieve analysis.  The data set is also used for other purposes including estimation of variable importance of different genotypic features for neutralization sensitivity. Since the analysis data were split into two separate data sets, we have included the [set 1 data](data1.csv), the [set 2 data](data2.csv), and the [full data set](fulldata.csv).

-----

## Data Contents

The data for this analysis are provided by CATNAP [4], which have been compiled from a large number of studies that analyzed the potency of neutralizing antibodies versus various HIV-1 sequences.  CATNAP provides the gp160 viral sequences, the IC50 and IC80 infectivity results derived from TZM-bl assay experiments, and associated annotation.  The data from CATNAP have been cleaned up and restructured for this analysis, and are described in this document.

CATNAP has data pertaining to many different antibodies and many different sequences.  The initial analyses to predict antibody resistance will be based on VRC01, though other antibodies will be considered when time permits (e.g., PGT151).  VRC01 is a well-studied antibody being tested for immunoprophylaxis in the AMP trials, and the version of data queried on 21 March 2017 from CATNAP contains infectivity results and sequence data for 624 viruses from 24 different subtypes, including recombinants.  As such, this data set will have one entry/row per virus.  Other antibodies tend to have fewer sequences available, which may limit statistical power.

For VRC01, all of the studies were performed with the TZM-bl assay.

Since these data are high-dimensional, it will be analyzed in phases, defined by pre-specified regions of the virus, enabling us to focus on regions of specific interest.  The goal is a manuscript based on Phase I data, and then subsequent manuscripts on Phase II data.

* Phase I:  All features specified in a separate analysis plan document, which excludes 5-mer variables, 9-mer variables, and physico-chemical property variables
* Phase II:  TBD, which would include the 5-mer variables, 9-mer variables, and physico-chemical property variables

All sequence positions are labeled in HXB2 coordinates, using the “lettered” naming standard for positions that are insertions against HXB2.  E.g., position “31a” is the position following HXB2 position 31, where a residue was observed in a sequence other than HXB2, resulting in a gap in HXB2 at this position.  Similarly, position “31b” is the second position following HXB2 position 31, where a residue was observed in a sequence other than HXB2, resulting in a gap in HXB2 at this position.  In variable regions with a high degree of insertion content, these letters may surpass “z”, in which case they flip over to “aa” and progress to “ab”, etc.

In all variables, missing data are represented by `NA`.

-----

## Study Annotation

By “annotation” I am referring to the non-sequence data provided by CATNAP, which includes information pertaining to the virus and the infection from which the virus was isolated.

These data are represented as 30 variables, and are defined as follows (when available):

1.	`seqname.lanl`:  the name of the virus as represented in the LANL HIV database (i.e., the information found in the virus's header line when represented in FASTA format).
2.	`seqname`:  the virus's canonical name.
3.	`seqname.aliases`:  a comma-delimited list of aliases for this virus.
4.	`gb.accession`:  the accession number for this virus's GenBank entry.
5.	`subtype`:  the virus's subtype as defined by LANL.
6.	`subtype.reduced`:  same as subtype, except subtypes that were not found in at least ten sequences are redefined as “Other”.  The following subtypes are included in this variable:
	*	CRF01_AE (represented as “01_AE”)
	*	CRF02_AG (represented as “02_AG”)
	*	CRF07_BC (represented as “07_BC”)
	*	A1
	*	A1C (recombinant of A1 and C)
	*	A1D (recombinant of A1 and D)
	*	B
	*	C
	*	D
	*	Group O (represented as “O”)
	*	Other (i.e., none of the above)
7.	`subtype.is.[subtype]` (11 variables):  the subtypes from subtype.reduced rendered as binary indicator variables. (“[subtype]” is replaced with the subtype being indicated.)  There are eleven variables under this category:  ten for each subtype and one for “Other”.
8.	`country.of.origin`:  the country where the virus was isolated.
9.	geographic.region.of.origin:  global geographic groupings based on country.of.origin, with the intention of reducing the number of “countries” in a sensible manner.  The geographic categories and their respective countries are as follows:
	*	“Asia”:  China, India and Thailand.
	*	“N.Africa”:  Cameroon, Cote d'Ivoire, Democratic Republic of the Congo, Ethiopia, Kenya, Rwanda, Tanzania and Uganda.
	*	“S.Africa”:  Botswana, Malawi, South Africa and Zambia.
	*	“Europe.Americas”:  Belgium, Brazil, France, Haiti, Italy, Peru, Spain, Trinidad and Tobago and the United States.
10.	`geographic.region.of.origin.is.[region]` (four variables):  the regions from `geographic.region.of.origin` rendered as binary indicator variables.  (`[region]` is replaced with the geographic region being indicated.)  There are four variables under the category:  one for each region listed under `geographic.region.of.origin`.
11.	`coreceptor`:  a text variable indicating the virus’s coreceptor status.  These data are sparse, with a 70%+ tendency to be missing.  When present, it comes in three forms:
	*	“CCR5”
	*	“CXCR4”
	*	“CCR5.CXCR4” (indicating the capability of using either coreceptor)
12.	`coreceptor.is.[coreceptor]` (two variables):  the coreceptor information from coreceptor rendered as binary indicator variables.  (`[coreceptor]` is replaced with the coreceptor being indicated.)  There are two variables under the category:  one for each individual coreceptor listed under `coreceptor`.  For the case where `coreceptor = “CCR5.CXCR4”`, both variables are indicated with a `1`.
13.	`tier.raw`:  the virus's neutralization sensitivity tier (lower number = more sensitive), when available, exactly as it was found in CATNAP.  This information can be ambiguous (e.g., “1B or 2”) and thus it isn't easily amenable for analysis.  Furthermore, this value has a high (~50%) tendency to be missing.
14.	`tier.ordinal`:   the virus's neutralization sensitivity tier, as provided above, but converted into an ordinal, monotonic form for easy analysis.  This value ranges from 1 to 8, where a lower number indicates a higher sensitivity to neutralization.
	* 1 = Tier 1A
	* 2 = Tier 1
	* 3 = Tier 1B
	* 4 = Tier “1 or 2”
	* 5 = Tier “1B or 2”
	* 6 = Tier 2
	* 7 = Tier “2 or 3”
	* 8 = Tier 3
15.	`infection.stage.raw`:  the stage of the infection from which this virus was isolated, exactly as it was found in CATNAP.  These values include “acute”, “early”, “intermediate”, and “chronic”.  This value is occasionally (~11%) not available.  
16.	`infection.stage.ordinal`:  like “tier.ordinal”, we have converted the raw infection stage data into an ordinal form that is analysis-ready.
	* 1 = “acute”
	* 2 = “early”
	* 3 = “intermediate”
	* 4 = “chronic”

-----

## Infectivity Results

Neutralization studies compared one (or many) viruses with one (or many) antibodies, and some antibody-virus combinations were studied more than once, typically by different investigators.  The results of all of these studies were compiled in CATNAP, and the process of reconciling these different results into something we can use analytically requires some explanation.

The neutralization studies yielded the readouts of IC50 and/or IC80 (in units of μg/ml).  IC50 was recorded for all of the VRC01 studies.  IC80 was captured most (~75%) of the time.

Occasionally, the IC50 or IC80 values were right-censored (only a few concentrations were left-censored, such that in this document “censoring” always refers to right-censoring unless otherwise specified).  These values are concentrations, so the lower the value, the more potent the neutralization.  When the value was above the upper limit of detection, it is represented in the raw CATNAP data as a censored value (e.g., “UD:>50”), and in these cases it should be considered that neutralization was not detected.

Viruses that were assessed with a single study are the simple case, as their IC50/IC80 values come from that single study.  For viruses that were assessed with multiple studies, however, CATNAP summarizes their IC50/IC80 values with the geometric mean, and we have continued the use of this value out of consistency.

CATNAP does not impute censored values when calculating the geometric mean, so this result contains IC50/IC80 values akin to “UD:>1” or even “UD:>10, >50”.

For the sake of having data that are analysis-ready, we have created new variables with imputations for the censored values.  Any virus with right-censored results was imputed with the value of twice the upper limit of detection.  (E.g., the value “UD:>10” was imputed as “20”.)  Similarly, viruses with left-censored results were imputed with the values of half the lower limit of detection.  (E.g., the value “<0.02” was imputed as “0.1”.)  Then the geometric mean using the natural log was taken of all available values, both empirical and imputed.  We have also created log-transformed versions of the geometric means, which have distributions that are more Gaussian than the original IC50/IC80 values.

With that in mind, the IC50 and IC80 results are represented in this data with the following 10 variables:

1.	`ic50.by.study.raw`:   a structured list of IC50 readouts and the citations of the studies from whence they came, direct from CATNAP.  Readout and citation are colon/space-separated, and different readout/citation pairs are semicolon-separated.  E.g., “0.318: Andrabi2015;0.177: Bonsignori2016”.
2.	`ic50.geometric.mean.raw`:  the geometric mean of the readouts available in `ic50.by.study.raw`, as provided by CATNAP (i.e., with no imputation of censored values).
3.	`ic80.by.study.raw`:  same as “ic50.by.study.raw”, but for IC80 values.
4.	`ic80.geometric.mean.raw`:  same as “ic50.geometric.mean.raw”, but for IC80 values.
5.	`ic50.censored`:  a binary value (`TRUE`/`FALSE`) indicating whether a right-censored value was available for this virus. 
6.	`ic50.geometric.mean.imputed`:  the geometric mean of all IC50 readouts for this virus-antibody pair, using imputed values for right-censored cases
7.	`ic50.geometric.mean.imputed.log10`:  log-transformed (base 10) variant of `ic50.geometric.mean.imputed`.
8.	`ic80.censored`:  same as `ic50.censored`, but for IC80 values.
9.	`ic80.geometric.mean.imputed`:  same as `ic50.geometric.mean.imputed`, but for IC80 values.
10.	`ic80.geometric.mean.imputed.log10`:  same as `ic50.geometric.mean.imputed.log10`, but for IC80 values.
11.	`neutralization.slope`:  the slope of the neutralization curve, calculated via equation 6 from Webb et al.[9]
12.	`binding.dichotomous.sens.resis`:  a binary outcome imputed from ic50.censored and `ic50.geometric.mean.imputed.log10`.  This endpoint is a 0 (sensitive) if `ic50.geometric.mean.imputed.log10` < 0 (i.e., if IC50 < 1), and is a 1 (resistant) if `ic50.censored = 1`.  Otherwise, viruses with an IC50 in between these two cutoffs are excluded from the analysis (by having this variable = `NA`).

-----

## Viral Sequences:  Amino Acids, Their Distributions, and Their Associated Physicochemical Properties

As CATNAP is hosted by LANL, all sequences provided by CATNAP are queried directly from the LANL HIV database and retain the same alignment.  Sequence data provided here are as proteins, and are represented by three different means:  (1) amino acid information; (2) binary physicochemical property values from Taylor et al., and (3) continuous physiochemical property values from Wold et al.  The data for (2) and (3) are represented for both individual amino acids and for k-mer peptides of lengths k = 5 and k = 9.  Additionally, (4) a binary variable is provided to indicate a residue’s non-majority status.

For basic amino acid information, the sequences are converted into CSV format with the amino acids represented as binary indicators for each position, for each of the 20 biogenic amino acids by their one-letter IUPAC code, plus the following IUPAC-standard and -nonstandard (but still common in the LANL database) ambiguity codes and other annotation:
	* `B` = `“D or N”`
	* `J` = `“I or L”`
	* `Z` = `“E or Q”`
	* `X` = Unknown/unspecified
	* `gap` = gap in alignment , represented in sequence by dash (“-”) 
	* `stop_asterisk` = stop codon represented in sequence by asterisk (“*”)
	* `stop_dollar` = stop codon represented in sequence by dollar sign (“$”)
	* `question` = Unknown/unspecified, represented in sequence by question mark (“?”)
	* `frameshift` = incomplete codon, represented in sequence by octothorpe (“#”)

Thus, for every position, there are 29 binary variables representing the potential sequence information at that position.  The vast majority (75%+) of these variables are redundant (they're either all “0” or all “1”), but we retained them in this dataset for the sake of being exhaustively inclusive as the raw source data set.

The amino acid variables are labeled using the following format: 

`hxb2.[position].AA.1mer`

where `[position]` = the position of the AA in HXB2 coordinates, and `AA` equals the amino acid, ambiguity code, or other code that the variable is indicating.  For example, the following variable:

`hxb2.322.K.1mer`

represents the presence or absence of a lysine (`K`) at position 322.

For this binary representation, the presence of an AA (or other code) at this position is indicated by a `1`, and the absence of an AA (or other code) at this position is indicated by a `0`.  Thus, every position for each sequence should have only one `1` over all 29 of the position's indicator variables.

-----

## Physicochemical Property Information

Two forms of physicochemical property information are provided in the data set:  (1) the binary properties as defined by Taylor [1] and later expanded by Livingstone and Barton [2], and the quantitative “z-scales” defined by Wold et al. [3] for the purpose of defining quantitative structure-activity relationships (QSAR) between proteins.  All of these properties are provided for individual amino acids, and for k-mers of length k = 5 and k = 9.

The binary properties used are:

•	Aliphatic
•	Aromatic
•	Polar
•	Charged
•	Positive
•	Negative
•	Hydrogen acceptor
•	Hydrogen donor
•	Hydrophobic
•	Small (volume of steric side-chain)
•	Tiny (volume of steric side-chain)

(Technically the Taylor set of properties includes a property to indicate the presence of a proline, to reflect the impact that proline can have on the protein's structure, but since this was redundant with the amino acid description provided above, I excluded it in this dataset to reduce unnecessary variables.)

These properties are represented in the data for individual amino acids by the variable name convention:

`hxb2.[position].property.1mer`

where `[position]` = the position of the AA in HXB2 coordinates, and `property` is any of the above 11 properties.  For example:

`hxb2.225.hydrogen_acceptor.1mer`

is the binary indicator for the amino acid at HXB2 position 225, and its capacity as a hydrogen acceptor.  A `1` indicates the presence of the property at that position, and a `0` indicates the absence of the property at that position.

Taylor's properties have been said to be too simplistic and don't represent real-world variability, and that these nuances are better represented in Wold's z-scales.  The primary drawback to using z-scales, however, is their relative lack of interpretability.  The five z-scales (described as z1 through z5) were determined through a principal components analysis from empirical physicochemical data, so as such, their descriptions are not as cleanly defined as Taylor binary properties.  That said, the z-scales exhibit some correlation with certain properties, and can be generally interpreted thus:

z1:  hydrophobicity
z2:  steric bulk/volume
z3:  polarity
z4:  electronegativity
z5:  electrophilicity

The z-scales are represented in the data for individual amino acids by the variable name convention:

`hxb2.[position].scale.1mer`

where `[position]` = the position of the AA in HXB2 coordinates, and `scale` indicates the appropriate z-scale corresponding to the column's value, as “z1” through “z5”.  For example:

`hxb2.810.z3.1mer`

is the quantitative z3 measure for the amino acid at HXB2 position 810.  All z-scale values are continuous, ranging roughly on the scale from (-5, 5).

In addition to representing the physicochemical properties of individual amino acids, as described above, all of these properties can be summed over peptides to indicate their degree of enrichment within that given peptide.  This was done for all 11 of the Taylor binary physiochemical properties listed above, and all five of Wold's z-scales, for k-mer lengths of k = 5 and k = 9.  To create these values, the property values are summed across all contiguous k amino acids from the origin; i.e., gaps are ignored following the peptide's origin.  These peptide-based variables have the same naming convention as the variables describing individual amino acids, except the “1mer” term is replaced by “5mer” or “9mer”, indicating the proper peptide length.  For example:

`hxb2.328.polar.9mer`

represents the sum of the binary polar property over the 9mer starting at position HXB2 328.  (So if this 9mer contains five polar residues, this value will be “5”.  Similarly,

`hxb2.101.z2.5mer`

represents the sum of the z2 scale values over the 5mer starting at position HXB2 101.

In addition to the above, we will create some variables to represent the potential for steric clashes in VRC01 binding, as steric clashes have been identified as a primary cause of natural VRC01 resistance.  These variables are created by summing the number of sites containing a Taylor “small” indicator property.  These three variables are:
	* `taylor.small.total.v5`:  the total number of “small” residues in the V5 loop.
	* `taylor.small.total.loop.d`:  the total number of “small” residues in Loop D.
	* `taylor.small.total.cd4`:  the total number of “small” residues in the CD4 binding loop.


-----

## Non-Majority Status

A binary indicator variable is provided to indicate whether or not a sequence contains a residue that is not the most-frequently seen residue among the sequence population.  For example, at a specific site, if 65% of sequences have an “A”, 30% have a “C”, and 5% have a “D”, then the 35% of sequences with a “C” or a “D” at this site will be described as “non-majority”..  It is possible for a majority residue to be seen in fewer than 50% of sequences as long as none of the other observed residues surpasses it in frequency.

Sites’ non-majority status is represented in the data for individual amino acids by the variable name convention:

`hxb2.[position].nonmajority.1mer`

where `[position]` = the position of the AA in HXB2 coordinates.  For example:

`hxb2.626.nonmajority.1mer`

is the binary indicator for the amino acid at HXB2 position 626, relating its status as a non-majority residue.  A `1` indicates the presence of a non-majority amino acid at that position, and a `0` indicates the absence of a majority amino acid at that position.

Gaps and alignment artifacts (e.g., “frameshift” (“#”) characters) are not counted as residues, either majority or non-majority.  All other residue information, including ambiguity codes (“B”, “J”, “Z”, “X”) and stop codons, are included in the counts for are majority/non-majority candidates.

(Because of this gap logic, it is important to note that a “0” at any position does not indicate the presence of a majority amino acid.  More accurately, a “0” indicates the presence of a majority amino acid or a gap/frameshift at that position.)

In our current sequence population from CATNAP, two positions are unique in that they have two “co-majority” amino acids (tied at the mode).  Position 240 has co-majority residues of “K” and “N” (found in 158 sequences each), and position 832 has two co-majority residues of “L” and “V” (found in 180 sequences each).  In each of these cases, the two co-majority residues are regarded as “majority”, and the remaining residues are regarded as “non-majority”.

-----

## Glycosylation Information

Glycosylation plays a role in the binding of neutralizing antibodies, so I have represented it directly in the data.  There are two variables for each position to indicate the role they play in glycosylation:

1.	Documented:  the site at this position has been described as commonly having the potential for N-linked glycosylation.  `TRUE` = the site has been documented as a likely sequon starting at this position; `FALSE` = the site was not listed as a likely glycosylation site, though perhaps it may be occasionally, especially given the myriad issue one may encounter with alignment.  This variable has the same value at a given position for all sequences, and I only recommend its use for subsetting sequences by their potential glycosylation status.
2.	Actual:  this site in this sequence is observed to have the N-linked glycosylation motif ([N][!P][S|T]) starting at this position.  `TRUE` = that the 3mer starting at this position matches the N-linked glysocylation motif.  `FALSE` = that the 3mer starting at this position does not match the N-linked glycosylation motif.  When matching the 3mer against the motif, gaps were removed downstream of the origin.  This variable is sequence-specific:  unlike the “documented” data above, this variable will vary at any given position, depending on the content of the actual sequences.

All but one of the “documented” glycosylation sites are observed as “actual” glycosylation sites in these sequences, where many of the “actual” glycosylation sites are not among the “documented” glycosylation sites.

These variables can be identified with the following nomenclature:

`hxb2.[position].sequon_documented.1mer`

`hxb2.[position].sequon_actual.1mer`

where (in both cases) `[position]` = the location in HXB2 coordinates of the first site in the N-linked glycosylation motif.  E.g.:

`hxb2.101.sequon_documented.1mer`

contains the indicator for whether HXB2 position 101 has been documented as frequently being the start position of a sequon, while:

`hxb2.101.sequon_actual.1mer`

contains the binary indicator for whether HXB2 position 101 is actually observed to be  the start position of an actual sequon in the given sequence.

Lastly, there are variables which represent the sum of the total number of sequons in a given sequence, for various subsets of the Env sequence.

* `sequon.total.env`:  the total number of sequons among all positions in Env.
* `sequon.total.gp120`:  the total number of sequons among all positions in gp120.
* `sequon.total.vrc01`:  the total number of sequons among all positions in the VRC01 footprint.
* `sequon.total.cd4`:  the total number of sequons among all CD4 binding sites.
* `sequon.total.v5`:  the total number of sequons among all positions in the V5 region.
* `sequon.total.loop.d`:  the total number of sequons among all positions in Loop D.
* `sequon.total.loop.e`:  the total number of sequons among all positions in Loop E.
* `sequon.total.sj.fence`:  the total number of sequons among the four sites (197, 276, 363 and 460) identified by Stewart-Jones et al. [6] and Crooks et al. [9, 10] as part of a glycan shield.
* `sequon.total.sj.trimer`:  the total number of sequons among the five sites (61, 64, 197, 276, 386) identified by Stewart-Jones et al. [6] where VRC01 interacts with the HIV-1 trimer.
* `sequon.total.subset`:  the total number of sequons within only those sites represented in the current data subset

As mentioned, to determine whether a site is a sequon (i.e., a site with glycosylation potential), I used the standard, three-position N-linked glycosylation motif:  [N][!P][S|T].  Sometimes an extended, four-position motif is used to identify sequons ([N][!P][S|T][!P]); I didn't use this, because I am under the impression that the three-position motif is more widely accepted.  

-----

## Cysteine Counts

Counts of observed cysteines (“C”) within various regions or site sets are provided as follows:
	* `cysteines.total.env`:  the total number of cysteines within the entire Env protein.
	* `cysteines.total.gp120`:  the total number of cysteines within the entire gp120 protein.
	* `cysteines.total.v5`:  the total number of cysteines within of the V5 region.
	* `cysteines.total.loop.d`:  the total number of cysteines within Loop D.
	* `cysteines.total.loop.e`:  the total number of cysteines within Loop E.

-----

## Viral Geometry

Sequence lengths of various regions are provided.  The total sequence length was calculated as the length of the aligned sequence minus all gaps and frameshifts.  Total lengths are provided as follows:
	* `length.env`:  the length of the entire Env protein.
	* `length.gp120`:  the length of the entire gp120 protein.
	* `length.v5`:  the length of the V5 region.
	* `length.loop.d`:  the length of Loop D.
	* `length.loop.e`:  the length of Loop E.

Furthermore, the variables of length.v5, length.loop.d. and length.loop.e will be broken out into a binary indicator variable to show whether the length of this region is represented in the main body of the distribution of length values, or is an outlier.  For v5, the main body of the distribution is a length of 9 or 10 AA.  For Loop D, the main body of the distribution is a length of 9 AA.  For Loop E, the main body of the distribution is a length of 2 or 3 AA.  Any values outside of these ranges (above or below) are regarded as outliers, and are represented by “1” in these binary indicator variables.  These variables will have the same names as the above, except they will be suffixed by `outliers`.  E.g., the indicator variable for the outlier bin for the V5 loop will be called `length.v5.outliers`.


-----

## Housekeeping

While the foundation of these data was provided by CATNAP, most of the derivation, assembly, and documentation were performed by Craig Magaret with the Fred Hutchinson Cancer Research Center, who, being human, is occasionally fallible.  Please report any feedback, questions, or curiosities to him (cmagaret@fredhutch.org).

-----

## References

1.	Taylor, W.T.:  “The Classification of Amino Acid Conservation”, J. Theor. Biol. (1986) 119, 205-218.
2.	Livingstone, C.D. Barton, G.J.:  “Protein Sequence Alignments:  a Strategy for the Hierarchical Analysis of Residue Conservation”, CABIOS (1993) 9:6, 745-756.
3.	Sandberg, M. et al.:  “New Chemical Descriptors Relevant for the Design of Biologically Active Peptides.  A Multivariate Characterization of 87 Amino Acids”, J. Med. Chem. (1998) 41, 2481-2491.
4.	Yoon, H.: “CATNAP: a tool to compile, analyze and tally neutralizing antibody panels”, Nuc. Acid Res. (2015) 43, W213-219.
5.	Zhou, T. et al.:  “Structural Basis for Broad and Potent Neutralization of HIV-1 by Antibody VRC01”, Science (2010) 329(5993): 811-817. 
6.	Stewart-Jones, Guillaume BE, Cinque Soto, Thomas Lemmin, Gwo-Yu Chuang, Aliaksandr Druz, Rui Kong, Paul V. Thomas et al. "Trimeric HIV-1-Env structures define glycan shields from clades A, B, and G", Cell (2016) 165, 813-826.
7.	Stansell, Elizabeth et al. “Gp120 on HIV-1 Virions Lacks O-Linked Carbohydrate.” PLoS ONE 10.4 (2015): e0124784. PMC. Web. 24 Mar. 2017.
8.	Webb, NE, Montefiori DC, Lee B.  “Dose–response curve slope helps predict therapeutic potency and breadth of HIV broadly neutralizing antibodies”, Nature Communications (2015), 6(8443).
9.	Crooks ET et al.  “Vaccine-Elicited Tier 2 HIV-1 Neutralizing Antibodies Bind to Quaternary Epitopes Involving Glycan-Deficient Patches Proximal to the CD4 Binding Site.”, PLOS Pathogens (2015) 11(5): e1004932. doi:10.1371/journal.ppat.1004932
10.	Crooks ET et al.  “Effects of partially dismantling the CD4 binding site glycan fence of HIV-1 Envelope glycoprotein trimers on neutralizing antibody induction”, Virology (2017) 505:193-209 
11.	Kabsch, W. & Sander, C. Dictionary of protein secondary structure: Pattern recognition of hydrogen‐bonded and geometrical features. Biopolymers (1983). doi:10.1002/bip.360221211
12.	Joosten, R. P. et al. A series of PDB related databases for everyday needs. Nucleic Acids Res. (2011). doi:10.1093/nar/gkq1105


