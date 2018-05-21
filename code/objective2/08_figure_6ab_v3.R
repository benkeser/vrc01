

# ---------------------------------------------------------------------------- #
# STEP 0:  set up a fresh workspace
# ---------------------------------------------------------------------------- #


# purge!
rm (list=ls ())


# ---------------------------------------------------------------------------- #
# STEP 1:  load up everything we need
# ---------------------------------------------------------------------------- #


# identify our filesystem locations; you will need to set "path.home" to the
# path of the repository on your local filesystem
path.home <- "/repository/home/path"
path.lib <- paste0 (path.home, "/code")
path.data <- paste0 (path.home, "/data")
path.rdata <- paste0 (path.home, "/data")

# load our VIM results
setwd (path.data)
vim <- read.csv ("varimport_omnibus_v4.csv", header=T)

# load our binding site info
setwd (path.data)
binding.sites.vrc01 <- read.csv ("map/vrc01_bsites.csv", header=T)
binding.sites.cd4 <- read.csv ("map/cd4_bsites.dat", header=F)[, 1]


# ---------------------------------------------------------------------------- #
# STEP 2:  process our data accordingly
# ---------------------------------------------------------------------------- #


# filter out results associated with specific residues, and identify their site
# locations
filter <- rep (FALSE, nrow (vim))
position <- NULL
feature.names <- strsplit (as.character (vim [, 1]), fixed=T, split=".")
for (item in 1:length (feature.names)) {
  if (feature.names[[item]][1] == "hxb2" & feature.names[[item]][3] != "sequon_actual") {
    filter[item] <- TRUE
    position <- append (position, feature.names[[item]][2])
  }
}
position <- as.numeric (position)

# subset the VIM data we need for each endpoint
vim.ic50.censored <- vim[filter, c (1, 2, 9, 14, 21, 45)]
vim.ic50 <- vim[filter, c (1, 4, 11, 16, 23, 55)]

# block out the direction of effects for each endpoint
direction.ic50.censored <- rep (24, sum (filter))
direction.ic50.censored[vim[filter, "lm.ic50.censored.tval"] < 0] <- 25
direction.ic50 <- rep (24, sum (filter))
direction.ic50[vim[filter, "lm.ic50.tval"] < 0] <- 25

# stratify our results by three groups:  (1) FWER significant and in Top 50; 
# (2) not FWER significant but in Top 50; (3) neither FWER significant nor in 
# Top 50
vim.ic50.censored.tier.1 <- (vim.ic50.censored[, 4] <= 50 | vim.ic50.censored[, 5] <= 50) & vim.ic50.censored[, 6] < 0.05
vim.ic50.censored.tier.2 <- (vim.ic50.censored[, 4] <= 50 | vim.ic50.censored[, 5] <= 50) & vim.ic50.censored[, 6] >= 0.05
vim.ic50.censored.tier.3 <- vim.ic50.censored[, 4] > 50 & vim.ic50.censored[, 5] > 50
vim.ic50.tier.1 <- (vim.ic50[, 4] <= 50 | vim.ic50[, 5] <= 50) & vim.ic50[, 6] < 0.05
vim.ic50.tier.2 <- (vim.ic50[, 4] <= 50 | vim.ic50[, 5] <= 50) & vim.ic50[, 6] >= 0.05
vim.ic50.tier.3 <- vim.ic50[, 4] > 50 & vim.ic50[, 5] > 50


# ---------------------------------------------------------------------------- #
# STEP 3:  plot-o-rama
# ---------------------------------------------------------------------------- #


# set up our plotting device
setwd (path.data)
pdf (file="vim_results_v4.pdf", height=5, width=8)
par (mfrow=c (2, 1), oma=c (0, 0, 0, 0), xpd=T, cex=0.8)

# PANEL 1:  VIM results for IC50 censored
par (mar=c (1, 4, 6, 12))
plot (position[vim.ic50.censored.tier.3], apply (vim.ic50.censored[vim.ic50.censored.tier.3, 4:5], 1, min), 
      xlab="", ylab="Highest VIM Rank", type="n", xlim=c (0, 500), ylim=c (525, 1), 
      bty="n", xaxt="n", yaxt="n")
points (position[vim.ic50.censored.tier.3], apply (vim.ic50.censored[vim.ic50.censored.tier.3, 4:5], 1, min), 
        col="gray", pch=direction.ic50.censored[vim.ic50.censored.tier.3], bg="gray")
points (position[vim.ic50.censored.tier.2], apply (vim.ic50.censored[vim.ic50.censored.tier.2, 4:5], 1, min), 
        col="blue", pch=direction.ic50.censored[vim.ic50.censored.tier.2], bg="blue")
points (position[vim.ic50.censored.tier.1], apply (vim.ic50.censored[vim.ic50.censored.tier.1, 4:5], 1, min), 
        col="red", pch=direction.ic50.censored[vim.ic50.censored.tier.1], bg="red")
axis (side=2, at=c (1, 75, 150, 225, 300, 375, 450, 525), labels=F)
axis (side=2, at=c (1, 150, 375, 525), tick=F)


# add some location annotation:  viral regions
axis (side=3, at=c (131, 157.5, 196), pos=-225, labels=F)
axis (side=3, at=c (275, 283), pos=-225, labels=F)
axis (side=3, at=c (296, 331), pos=-225, labels=F)
axis (side=3, at=c (353, 357), pos=-225, labels=F)
axis (side=3, at=c (385, 418), pos=-225, labels=F)
axis (side=3, at=c (460, 469), pos=-225, labels=F)
text (144, -300, "V1", cex=0.8)
text (177, -300, "V2", cex=0.8)
text (279, -300, "Loop D", cex=0.8)
text (314, -300, "V3", cex=0.8)
text (355, -300, "Loop E", cex=0.8)
text (402, -300, "V4", cex=0.8)
text (465, -300, "V5", cex=0.8)

# add some location annotation:  binding sites
rect (binding.sites.vrc01[, 1] - 0.6, -125, binding.sites.vrc01[, 1] + 0.6, -175, col="chartreuse4", border=F)
rect (binding.sites.cd4 - 0.6, -50, binding.sites.cd4 + 0.6, -100, col="goldenrod4", border=F)

# label the site sets
rect (535, -125, 547, -175, col="chartreuse4")
rect (535, -50, 547, -100, col="goldenrod4")
text (550, -150, "VRC01 Binding Footprint", pos=4, col="chartreuse4", cex=0.8)
text (550, -75, "CD4 Binding Sites", pos=4, col="goldenrod4", cex=0.8)

# add legend, describing plot characters
#legend (x = 500, y=100, 

# PANEL 2:  VIM results for quantitative IC50
par (mar=c (5, 4, 1, 12))
plot (position[vim.ic50.tier.3], apply (vim.ic50[vim.ic50.tier.3, 4:5], 1, min), 
      xlab="Env Position", ylab="Highest VIM Rank", type="n", xlim=c (1, 500), ylim=c (525, 1), 
      bty="n", xaxt="n", yaxt="n")
points (position[vim.ic50.tier.3], apply (vim.ic50[vim.ic50.tier.3, 4:5], 1, min), 
        col="gray", pch=direction.ic50[vim.ic50.tier.3], bg="gray")
points (position[vim.ic50.tier.2], apply (vim.ic50[vim.ic50.tier.2, 4:5], 1, min), 
        col="blue", pch=direction.ic50[vim.ic50.tier.2], bg="blue")
points (position[vim.ic50.tier.1], apply (vim.ic50[vim.ic50.tier.1, 4:5], 1, min), 
        col="red", pch=direction.ic50[vim.ic50.tier.1], bg="red")
axis (side=2, at=c (1, 75, 150, 225, 300, 375, 450, 525), labels=F)
axis (side=2, at=c (1, 150, 375, 525), tick=F)
axis (side=1, at=c (1, seq (50, 500, 50)))

# close plotting device
dev.off ()



# ---------------------------------------------------------------------------- #
#                                    - 30 -
# ---------------------------------------------------------------------------- #

