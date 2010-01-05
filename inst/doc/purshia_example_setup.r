# Example of setting up the Genomatic
# purshia example.
# Hash marks denote comments which are ignored by R.
# File created by Brian J. Knaus, Feb. 2008.
# 
# File last updated by Brian J. Knaus, January 5, 2010.

##### ##### ##### ##### ##### #
#
# Copy the example to a       #
# directory where you have    #
# read and write permisions   #
#
##### ##### ##### ##### ##### #

# If you don't know where your libraries (including the genomatic)
# are installed, try this:

.libPaths()

# Or:
genomatic.dir <- .find.package('genomatic')

# The example files are located in the '/genomatic/doc'
# directory.  For example:

# Move to the directory.
setwd(genomatic.dir)

# Move to the 'docs' directory.
# setwd('doc')

# Verify your directory.
getwd()
list.files()

# Designate a new working directory where you
# have permissions to read and write.  Note
# that this directory must be created with
# your operating system when using Windows.

# Windows
userdir <- c("c:/purshia_example/")

# Linux
dir.create("~/purshia_example")
setwd("~/")
setwd("purshia_example")
userdir <- getwd()

# Copy files to this new working directory.
putr_files <- c("extdata/plate_1_II.txt", "extdata/plate_1_I.txt", 
                "extdata/plate_5_II.txt", "extdata/plate_5_I.txt",
                "extdata/purshia_loci.csv",
                "extdata/purshia_loci_fsetD.csv",
                "extdata/purshia_loci_fsetG5.csv",
                "extdata/purshia_pops.csv",
                "doc/Genomatic_users_manual_v06.pdf")

setwd(genomatic.dir)
file.copy(putr_files, userdir)

# Now you are ready to start the example.
