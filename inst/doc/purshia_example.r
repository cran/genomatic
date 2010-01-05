# ##### ##### #####
#
# Load genomatic library #
#
# ##### ##### #####

library(genomatic)

# ##### ##### #####
#
# 96-Well Maps.   #
#
# ##### ##### #####

purshia <- read.table("purshia_pops.csv", header=TRUE, sep=",")
p.indiv <- pop2indiv(purshia)
p.plates <- plater(p.indiv)
setwd("output")
plate_write(p.plates, "putr_plate")


# ##### ##### #####
#
# ABI Submission. #
#
# ##### ##### #####

fset <- read.table("purshia_loci_fsetD.csv", header=TRUE, sep=",")
p.plate <- read.table("output/putr_plate_1.csv", header=TRUE,
                      sep=",", row.names=1)
setwd("output")
abi_sub(p.plate, fset, "putr")
setwd("..")

# ##### ##### ##### ##### ##### ##### ##### #####
#
# Input genotypic data to the Genomatic.        #
#
# ##### ##### ##### ##### ##### ##### ##### #####

# Initialize genomatic file.
setwd("output")
loci <- read.table("../purshia_loci.csv",
                   header=TRUE, sep=",")
gmtc <- init_gmtc(loci)
write.table(gmtc, "gmtc.csv", 
            sep=",", row.names=FALSE)

# Read in files.
setwd("..")
purshia.mp <- list()
fnames <- c("plate_1_I.txt", "plate_1_II.txt",
            "plate_5_I.txt", "plate_5_II.txt")
for(i in 1:length(fnames))
  {
  purshia.mp[[i]]<-read.table(fnames[i], header=TRUE,
                              sep="\t")
  }

# Read in master file
gmtc <- read.table("output/gmtc.csv", header=TRUE, sep=",")
#

putr_data <- genotyper2genomatic(gmtc, purshia.mp)
setwd("output")
putr_data <- condense(putr_data)

write.table(putr_data,
            paste("putr", "_gmtc.csv", sep=""),
            sep=",", row.names=FALSE)

# ##### ##### #####
#
# Managing Reruns. #
#
# ##### ##### #####

putr_data <- read.table("putr_gmtc.csv", header=TRUE, sep=",")
putr_dups <- dup_checker(putr_data)

if(is.null(putr_dups)!=TRUE)
  {
  setwd(out.dir)
  write.table(putr_dups, "duplicates_gmtc.csv", sep=",",
              row.names=FALSE)
  }

putr_data <- read.table("putr_gmtc.csv", header=TRUE, sep=",")
putr_dups <- read.table("duplicates_gmtc.csv", header=TRUE, sep=",")
putr_data <- dup_replace(putr_data, putr_dups)
putr_data <- order_sample(putr_data)

setwd("output")
write.table(putr_data,
            paste("putr", "_gmtc.csv", sep=""),
            sep=",", row.names=FALSE)


# ##### ##### ##### ##### #####
#
# Allele Characterization.    #
#
# ##### ##### ##### ##### #####

# Initialize locus files.
putr_data <- read.table("putr_gmtc.csv", header=TRUE, sep=",")
gap <- 0.3
bin_init(putr_data, gap)

putr_data <- read.table("putr_gmtc.csv", header=TRUE, sep=",")
loci <- names(putr_data)[seq(3, ncol(putr_data), by=2)]
loci <- substr(loci, 1, nchar(loci)-1)
#
locus.l <- list()
for (i in 1:length(loci))
  {
  locus.l[[i]]<-read.table(paste(loci[i], '_alleles.csv', sep=''),
                           header=TRUE, sep=',')
  }
allele_process(putr_data, locus.l)

# ##### ##### #####
#
# Scoring Bins.   #
#
# ##### ##### #####

putr_data <- read.table("putr_gmtc.csv", header=TRUE, sep=",")
loci <- names(putr_data)[seq(3,ncol(putr_data),by=2)]
loci <- substr(loci, 1, nchar(loci)-1)
#
bin_score(putr_data, loci,
          paste("putr", "_scored_gmtc.csv", sep=""))


# ##### #####
#
# Export.   #
#
# ##### #####

# GenAlEx Format
putr_data <- read.table("putr_scored_gmtc.csv", header=TRUE, sep=",")
putr_data <- genobins(putr_data)
genomatic2genalex(putr_data, "putr")

# NTSYSpc Format
putr_data <- read.table("putr_scored_gmtc.csv", header=TRUE, sep=",")
putr_data <- genobins(putr_data)
genomatic2ntsys(putr_data, "putr")

# Matrix Format
putr_data <- read.table("putr_scored_gmtc.csv", header=TRUE, sep=",")
putr_data <- genobins(putr_data)
write.table(putr_data,
            paste("putr", "_matrix.csv", sep=''),
            sep=",", row.names=FALSE)

##### ##### ##### ##### #####
# EOF.
