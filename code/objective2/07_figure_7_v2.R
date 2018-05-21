

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

# load our binding site info
setwd (path.data)
binding.sites.vrc01 <- read.csv ("vrc01_bsites.csv", header=T)
binding.sites.cd4 <- read.csv ("cd4_bsites.dat", header=F)[, 1]

# load sites provided by Ivelin
setwd (path.data)
dssp.group.a <- read.csv ("dssp_group_a.dat", header=F)[, 1]
dssp.group.b <- read.csv ("dssp_group_b.dat", header=F)[, 1]
dssp.sites <- sort (unique (c (dssp.group.a, dssp.group.b)))

# sites are defined manually
sites.sj.fence <- c (197, 276, 362, 363, 386, 392, 462, 463)
sites.sj.trimer <- c (61, 64, 197, 276, 386)
sites.sj.all <- sort (union (sites.sj.fence, sites.sj.trimer))
sites.gp120.covary <- c (46, 132, 138, 144, 150, 179, 181, 186, 190, 290, 321, 328, 354, 389, 394, 396, 397, 406)
sites.galter.pngs <- c (130, 139, 143, 156, 187, 197, 241, 262, 289, 339, 355, 363, 406, 408, 410, 442, 448, 460, 462)


# ---------------------------------------------------------------------------- #
# STEP 2:  plot-o-rama
# ---------------------------------------------------------------------------- #


# set up our plotting device
setwd (path.data)
pdf (file="sites_selected_v3.pdf", height=4, width=11)
par (oma=c (0, 0, 0, 0), mar=c (5, 4, 0, 12), xpd=T)

# set up plot
plot (1:856, 1:856, xlab="Env Position (HXB2)", ylab="", ylim=c (0, 1000), type="n", bty="n", xaxt="n", yaxt="n")

# add top and bottom axes
axis (side=1, at=c (1, seq (100, 800, 100), 856))
axis (side=3, at=c (1, 30.5, 511.5, 856), pos=650, labels=F)
text (15, 800, labels="Signal\nPeptide")
text (271, 800, labels="gp120")
text (684, 800, labels="gp41")

# add axes for regions
axis (side=3, at=c (131, 157.5, 196), pos=475, labels=F)
axis (side=3, at=c (275, 283), pos=475, labels=F)
axis (side=3, at=c (296, 331), pos=475, labels=F)
axis (side=3, at=c (353, 357), pos=475, labels=F)
axis (side=3, at=c (385, 418), pos=475, labels=F)
axis (side=3, at=c (460, 469), pos=475, labels=F)
text (144, 550, "V1", cex=0.7)
text (177, 550, "V2", cex=0.7)
#text (279, 550, "Loop D", cex=0.7)
text (270, 550, "Loop D", cex=0.7)
text (314, 550, "V3", cex=0.7)
text (355, 550, "Loop E", cex=0.7)
text (402, 550, "V4", cex=0.7)
text (465, 550, "V5", cex=0.7)

# label the site sets
rect (535, 375, 552, 425, col="chartreuse4")
rect (535, 300, 552, 350, col="goldenrod4")
rect (535, 225, 552, 275, col="blue4")
rect (535, 150, 552, 200, col="darkred")
rect (535, 75, 552, 125, col="cadetblue4")
rect (535, 0, 552, 50, col="darkorchid4")
text (550, 400, "VRC01 Binding Footprint", pos=4, col="chartreuse4", cex=0.8)
text (550, 325, "CD4 Binding Sites", pos=4, col="goldenrod4", cex=0.8)
text (550, 250, "Residues That Covary with the VRC01 Binding Footprint", pos=4, col="blue4", cex=0.8)
text (550, 175, "PNG Sites Identified as Important for VRC01 Activity", pos=4, col="darkred", cex=0.8)
text (550, 100, "Sites with Sufficient Exposed Surface Area", pos=4, col="cadetblue4", cex=0.8)
text (550, 25, "Sites Associated with VRC01-Specific PNG Effects", pos=4, col="darkorchid4", cex=0.8)

# mark the sites of interest
rect (binding.sites.vrc01[, 1] - 0.6, 375, binding.sites.vrc01[, 1] + 0.6, 425, col="chartreuse4", border=F)
rect (binding.sites.cd4 - 0.6, 300, binding.sites.cd4 + 0.6, 350, col="goldenrod4", border=F)
rect (sites.gp120.covary - 0.6, 225, sites.gp120.covary + 0.6, 275, col="blue4", border=F)
rect (sites.sj.all - 0.6, 150, sites.sj.all + 0.6, 200, col="darkred", border=F)
rect (dssp.sites - 0.6, 75, dssp.sites + 0.6, 125, col="cadetblue4", border=F)
rect (sites.galter.pngs - 0.6, 0, sites.galter.pngs + 0.6, 50, col="darkorchid4", border=F)

# do a quick count of how many unique sites we selected
unique (sort (c (binding.sites.vrc01[, 1], binding.sites.cd4, sites.gp120.covary, sites.sj.all, dssp.sites, sites.galter.pngs)))

# close plotting device
dev.off ()



# ---------------------------------------------------------------------------- #
#                                    - 30 -
# ---------------------------------------------------------------------------- #

