

# ---------------------------------------------------------------------------- #
# STEP 0:  get set up
# ---------------------------------------------------------------------------- #


# refresh our workspace
rm (list=ls ())

# identify our filesystem locations; you will need to set "path.home" to the
# path of the repository on your local filesystem
path.home <- "/repository/home/path"
path.lib <- paste0 (path.home, "/code")
path.data <- paste0 (path.home, "/data")
path.rdata <- paste0 (path.home, "/data")

# load our results
setwd (path.data)
vim <- read.csv ("varimport_omnibus_v4.csv", header=T)

# create the starting data for our tables
table.2 <- vim[, c (1, 2, 9, 21, 41:45)]
table.3 <- vim[, c (1, 4, 11, 23, 51:55)]
table.s1 <- vim[, c (1, 3, 10, 22, 46:50)]
table.s2 <- vim[, c (1, 5, 12, 24, 56:60)]
table.s3 <- vim[, c (1, 6, 13, 25, 61:65)]


rank (-table.2[, 2])
# reduce to multiplicity-corrected entries only
table.2.reduced <- table.2[table.2[, 9] < 0.05 & (rank (-table.2[, 2]) <= 50 | table.2[, 4] <= 50), ]
table.2.reduced <- table.2.reduced[!is.na (table.2.reduced[, 1]), ]
table.3.reduced <- table.3[table.3[, 9] < 0.05 & (rank (-table.3[, 2]) <= 50 | table.3[, 4] <= 50), ]
table.3.reduced <- table.3.reduced[!is.na (table.3.reduced[, 1]), ]
table.s1.reduced <- table.s1[table.s1[, 9] < 0.05 & (rank (-table.s1[, 2]) <= 50 | table.s1[, 4] <= 50), ]
table.s1.reduced <- table.s1.reduced[!is.na (table.s1.reduced[, 1]), ]
table.s2.reduced <- table.s2[table.s2[, 9] < 0.05 & (rank (-table.s2[, 2]) <= 50 | table.s2[, 4] <= 50), ]
table.s2.reduced <- table.s2.reduced[!is.na (table.s2.reduced[, 1]), ]
table.s3.reduced <- table.s3[table.s3[, 9] < 0.05 & (rank (-table.s3[, 2]) <= 50 | table.s3[, 4] <= 50), ]
table.s3.reduced <- table.s3.reduced[!is.na (table.s3.reduced[, 1]), ]


# ---------------------------------------------------------------------------- #
# FXN DEPOT:  define a function to create/format our tables
# ---------------------------------------------------------------------------- #


mk.table <- function (data) {

  # create our sorted table of the 55 top entries
  table <- data[order (data[, 2], decreasing=T), c (1:5, 7:9)]
  table[, 1] <- as.character (table [, 1])
  names (table) <- c ("Feature", "MCCV Composite VIM", 
                      "Ensemble VIM", "Ensemble VIM Rank", "Direction of Effect",
                      "Univariate p-value", "Univariate q-value", 
                      "Univariate FWER p-value")

  # round numbers:  VIMs to three decimal places, p-vals to three sig digits
  table[, 2:3] <- round (table[, 2:3], 3)
  table[, 6:8] <- signif (table[, 6:8], 3)

  # format our direction variable
  direction <- rep ("Null", nrow (table))
  direction[table[, 5] > 0] <- "Positive"
  direction[table[, 5] < 0] <- "Negative"
  table[, 5] <- direction

  # let's organize some information for our renaming endeavor
  feature.names <- table[, 1]
  feature.name.parts <- strsplit (table[, 1], split=".", fixed=T)
  feature.is.length <- unlist (lapply (feature.name.parts, function (x) x[1] == "length"))
  feature.is.subtype <- unlist (lapply (feature.name.parts, function (x) x[1] == "subtype"))
  feature.is.cysteines <- unlist (lapply (feature.name.parts, function (x) x[1] == "cysteines"))
  feature.is.sequons <- unlist (lapply (feature.name.parts, function (x) x[1] == "sequons"))
  feature.is.taylor <- unlist (lapply (feature.name.parts, function (x) x[1] == "taylor"))
  feature.is.aa <- unlist (lapply (feature.name.parts, function (x) x[1] == "hxb2" & nchar (x[3]) == 1))
  feature.is.sequon.actual <- unlist (lapply (feature.name.parts, function (x) x[1] == "hxb2" & x[3] == "sequon_actual"))

  # rename features:  presence/absence of amino acid
  bits <- matrix (unlist (strsplit (table[feature.is.aa, 1], split=".", fixed=T)), ncol=4, byrow=T)
  feature.names[feature.is.aa] <- paste (bits[, 2], "is", bits[, 3])

  # I think that's enough; let er rip
  table[, 1] <- feature.names
  return (table)
}


# ---------------------------------------------------------------------------- #
# STEP 1:  let's actually get to work
# ---------------------------------------------------------------------------- #


# create and write out our tables
setwd (path.data)
table.2.final <- mk.table (table.2.reduced)
write.csv (table.2.final, file="table_2_v5_both.csv", row.names=F)
table.3.final <- mk.table (table.3.reduced)
write.csv (table.3.final, file="table_3_v5_both.csv", row.names=F)
table.s1.final <- mk.table (table.s1.reduced)
write.csv (table.s1.final, file="table_s1_v5_both.csv", row.names=F)
table.s2.final <- mk.table (table.s2.reduced)
write.csv (table.s2.final, file="table_s2_v5_both.csv", row.names=F)
table.s3.final <- mk.table (table.s3.reduced)
write.csv (table.s3.final, file="table_s3_v5_both.csv", row.names=F)


# ---------------------------------------------------------------------------- #
#                                    - 30 -
# ---------------------------------------------------------------------------- #


