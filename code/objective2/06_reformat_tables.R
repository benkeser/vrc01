
# kill!
rm (list=ls ())

# where?
# (you will need to set "path.home" to the path of the repository on your local 
# filesystem)
path.home <- "/repository/home/path"
path.lib <- paste0 (path.home, "/code")
path.data <- paste0 (path.home, "/data")
path.rdata <- paste0 (path.home, "/data")

# load!
setwd (path.data)
table.2 <- read.csv ("table_2_v5_both.csv", header=T)
table.3 <- read.csv ("table_3_v5_both.csv", header=T)
table.s1 <- read.csv ("table_s1_v5_both.csv", header=T)
table.s2 <- read.csv ("table_s2_v5_both.csv", header=T)
table.s3 <- read.csv ("table_s3_v5_both.csv", header=T)

# define!
formatify <- function (data) {
  data.new <- as.character (data)
  data.new[data > 0.001] <- format (round (data[data > 0.001], 3), scientific=F)
  data.new[data < 0.001] <- format (data[data < 0.001], scientific=T)
  data.new[data == 0.001] <- "0.001"
  return (data.new)
}

# format!
table.2[, 6] <- formatify (table.2[, 6])
table.2[, 7] <- formatify (table.2[, 7])
table.2[, 8] <- formatify (table.2[, 8])
table.3[, 6] <- formatify (table.3[, 6])
table.3[, 7] <- formatify (table.3[, 7])
table.3[, 8] <- formatify (table.3[, 8])
table.s1[, 6] <- formatify (table.s1[, 6])
table.s1[, 7] <- formatify (table.s1[, 7])
table.s1[, 8] <- formatify (table.s1[, 8])
table.s2[, 6] <- formatify (table.s2[, 6])
table.s2[, 7] <- formatify (table.s2[, 7])
table.s2[, 8] <- formatify (table.s2[, 8])
table.s3[, 6] <- formatify (table.s3[, 6])
table.s3[, 7] <- formatify (table.s3[, 7])
table.s3[, 8] <- formatify (table.s3[, 8])

# save!
setwd (path.data)
write.csv (table.2, file="table_2_v5_both_reformatted.csv", row.names=F)
write.csv (table.3, file="table_3_v5_both_reformatted.csv", row.names=F)
write.csv (table.s1, file="table_s1_v5_both_reformatted.csv", row.names=F)
write.csv (table.s2, file="table_s2_v5_both_reformatted.csv", row.names=F)
write.csv (table.s3, file="table_s3_v5_both_reformatted.csv", row.names=F)

