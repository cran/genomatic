genogui <- function(){
#
# Genomatic Graphical User Interface.
#
require(tcltk)
#
# ##### ##### ##### #####
#
# Main splashscreen.    #
#
# ##### ##### ##### #####
#
main <- function()
  {
  tkdestroy(genoframe)
  #
  geno.dir <- .find.package("genomatic")
  geno.dir <- paste(geno.dir, "/images/purshia.gif", sep="")
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  image1 <- tclVar()
  tcl("image", "create", "photo", image1,
      file = geno.dir)
  imgAsLabel <- tklabel(genoframe, image = image1)
  tkpack(imgAsLabel)
  }
#
# ##### ##### ##### 
#
# Input/Output.   #
#
# ##### ##### #####
#
inout <- function()
  {
  tkdestroy(genoframe)
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  def.1 <- tclVar(data.dir)
  def.2 <- tclVar(out.dir)
  #
  heading <- tklabel(genoframe, text="Input/Output Configurationn")
  label.1 <- tklabel(genoframe, text="Data Directory")
  entry.1 <- tkentry(genoframe, text=def.1, width=30)
  but.1  <- tkbutton(genoframe, text="Select Directory",
              command=function(){
                tclvalue(tkchooseDirectory())->tclvalue(def.1)})
  #
  label.2 <- tklabel(genoframe, text="Output Directory")
  entry.2 <- tkentry(genoframe, text=def.2, width=30)
  but.2  <- tkbutton(genoframe, text="Select Directory",
              command=function(){
                tclvalue(tkchooseDirectory())->tclvalue(def.2)})
  #
  tkgrid.propagate(genoframe, FALSE)
  tkgrid(heading, columnspan=3)
  tkgrid(label.1, entry.1, but.1)
  tkgrid(label.2, entry.2, but.2)
  #
  tkgrid.configure(label.1, label.2, sticky="e")
  tkgrid.configure(entry.1, entry.2, sticky="w")
  #
  but <- tkbutton(genoframe, text="Set directory",
                  command=function(){
                    data.dir <<- tclvalue(def.1)
                    out.dir  <<- tclvalue(def.2)
                    #
                    tkmessageBox(title="In/Out Directory",
                                 message=paste("Input Output Set\n",
                                   data.dir, "\n",
                                   out.dir, "\n"),
                                 type="ok")
                  })
  tkgrid(tklabel(genoframe, text=""), but)
  #
  data.dir <<- tclvalue(def.1)
  out.dir  <<- tclvalue(def.2)
  }
#
# ##### ##### #####
#
# 96-Well Maps.   #
#
# ##### ##### #####
#
wellmap <- function()
  {
  tkdestroy(genoframe)
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  def.1 <- tclVar("Filename")
  def.2 <- tclVar("Prefix")
  #
  heading <- tklabel(genoframe, text="Create 96-Well Maps")
  label.1 <- tklabel(genoframe, text="Population File")
  entry.1 <- tkentry(genoframe, text=def.1, width=30)
  but.1  <- tkbutton(genoframe, text="Select File",
              command=function(){
                tclvalue(tkgetOpenFile(initialdir=data.dir))->tclvalue(def.1)
                })
  but.1chk <- tkbutton(genoframe, text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.1), header=TRUE, sep=","))})
  label.2 <- tklabel(genoframe, text="Output Prefix")
  entry.2 <- tkentry(genoframe, text=def.2, width=30)
  #
  but <- tkbutton(genoframe, text="Create 96-Well Maps",
                command=function(){
                  purshia <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                  p.indiv <- pop2indiv(purshia)
                  p.plates <- plater(p.indiv)
                  setwd(out.dir)
                  plate_write(p.plates, tclvalue(def.2))
                  #                 
                  tkmessageBox(title="96-Well Maps",
                               message="Maps Created",
                               type="ok")
                })
  #
  tkgrid.propagate(genoframe, FALSE)
  tkgrid(heading, columnspan=4)
  tkgrid(label.1, entry.1, but.1, but.1chk)
  tkgrid(label.2, entry.2)
  tkgrid(but, columnspan=2)
  #
  tkgrid.configure(label.1, label.2, sticky="e")
  tkgrid.configure(entry.1, entry.2, sticky="w")
  }
#
# ##### ##### #####
#
# ABI Submission. #
#
# ##### ##### #####
#
abisub <- function()
  {
  tkdestroy(genoframe)
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  def.1 <- tclVar("Filename")
  def.2 <- tclVar("Filename")
  def.3 <- tclVar("Prefix")
  #
  heading <- tklabel(genoframe, text="ABI Submission Forms")
  label.1 <- tklabel(genoframe, text="Filterset File")
  entry.1 <- tkentry(genoframe, text=def.1, width=30)
  but.1  <- tkbutton(genoframe, text="Select File",
              command=function(){tclvalue(tkgetOpenFile(initialdir=data.dir))->tclvalue(def.1)})
  but.1chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.1), header=TRUE, sep=","))})
  label.2 <- tklabel(genoframe, text="Sample Plate File")
  entry.2 <- tkentry(genoframe, text=def.2, width=30)
  but.2  <- tkbutton(genoframe, text="Select File",
              command=function(){tclvalue(tkgetOpenFile(initialdir=data.dir))->tclvalue(def.2)})
  but.2chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.2), header=TRUE, sep=","))})
  label.3 <- tklabel(genoframe, text="Output Prefix")
  entry.3 <- tkentry(genoframe, text=def.3, width=30)
  label.3b <- tklabel(genoframe, text="_fset_abi_sub.csv")
  #
  but <- tkbutton(genoframe, text="Create ABI Submission Forms",
                command=function(){
                  #
                  fset <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                  p.plate <- read.table(tclvalue(def.2), header=TRUE,
                                        sep=",", row.names=1)
                  setwd(out.dir)
                  abi_sub(p.plate, fset, tclvalue(def.3))
                  #
                  tkmessageBox(title="ABI Submission Form",
                               message="Form Created",
                               type="ok")
                })
  #
  tkgrid.propagate(genoframe, FALSE)
  tkgrid(heading, columnspan=4)
  tkgrid(label.1, entry.1, but.1, but.1chk)
  tkgrid(label.2, entry.2, but.2, but.2chk)
  tkgrid(label.3, entry.3, label.3b)
  tkgrid(but, columnspan=2)
  #
  tkgrid.configure(label.1, label.2, label.3, sticky="e")
  tkgrid.configure(entry.1, entry.2, entry.3, sticky="w")
  }
#
# ##### ##### ##### ##### ##### ##### ##### #####
#
# Input genotypic data to the Genomatic.        #
#
# ##### ##### ##### ##### ##### ##### ##### #####
#
gtyper2gmatic <- function(){
  tkdestroy(genoframe)
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  def.5 <- tclVar("Master file (_gmtc.csv)")
  def.1 <- tclVar("Filename")
  def.2 <- tclVar("Filename")
  def.3 <- tclVar("All Files")
  def.4 <- tclVar("Prefix")
  #
  heading <- tklabel(genoframe, text="Input Genotypic Data")
  #
  but.6 <- tkbutton(genoframe, text="Initialize Master File",
             command=function(){
               tt2 <- tktoplevel()
               tktitle(tt2) <- "Initialize Master File"
               label.a <- tklabel(tt2, text="Initialize Master File")
               label.b <- tklabel(tt2,
                                  text="Warning: writes over existing file!")
               #
               def.c <- tclVar("Locus file")
               label.c <- tklabel(tt2, text="Locus File")
               entry.c <- tkentry(tt2, text=def.c, width=30)
               but.c <- tkbutton(tt2, text="Select File",
                          command=function(){
                            tclvalue(def.c)<-tclvalue(tkgetOpenFile(initialdir=data.dir))
                })
               #
               but.a <- tkbutton(tt2, text="Initialize",
                          command=function(){
                            loci <- read.table(tclvalue(def.c),
                                               header=TRUE, sep=",")
                            gmtc <- init_gmtc(loci)
                            write.table(gmtc,
                                        file=paste(out.dir, "gmtc.csv", sep="/"),                                        sep=",", row.names=FALSE)
                            tkdestroy(tt2)
                          })
               but.b <- tkbutton(tt2, text="Cancel",
                          command=function(){tkdestroy(tt2)})
               #
               tkgrid(tklabel(tt2, text=""), label.a)
               tkgrid(tklabel(tt2, text=""), label.b)
               tkgrid(label.c, entry.c, but.c)
               tkgrid(tklabel(tt2, text=""), but.a)
               tkgrid(tklabel(tt2, text=""), but.b)
               #
               screenw <- as.integer(tclvalue(tkwinfo("screenwidth", tt2)))
               screenh <- as.integer(tclvalue(tkwinfo("screenheight", tt2)))
               midscreen <- paste("+", (screenw/2)-300, "+", (screenh/2)-200, sep="")
               #
               tkwm.geometry(tt2, midscreen)
               #
               })
  #
  label.5 <- tklabel(genoframe, text="Master File")
  entry.5 <- tkentry(genoframe, text=def.5, width=30)
  but.5 <- tkbutton(genoframe, text="Select File",
                command=function(){
                tclvalue(def.5)<-tclvalue(tkgetOpenFile(initialdir=out.dir))
                })
  but.5chk <- tkbutton(genoframe, text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.5),
                                         header=TRUE, sep=","))})
  #
  label.1 <- tklabel(genoframe, text="Initial File")
  entry.1 <- tkentry(genoframe, text=def.1, width=30)
  but.1  <- tkbutton(genoframe, text="Initialize File List",
              command=function(){
                tclvalue(tkgetOpenFile(initialdir=data.dir))->tclvalue(def.1)
                tclvalue(def.3) <- tclvalue(def.1)
                })
  but.1chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.1),
                                         header=TRUE, sep="\t"))})
  #
  label.2 <- tklabel(genoframe, text="Add File")
  entry.2 <- tkentry(genoframe, text=def.2, width=30)
  but.2  <- tkbutton(genoframe, text="Add File",
              command=function(){
                tclvalue(def.2) <- tclvalue(tkgetOpenFile(initialdir=data.dir))
                tclvalue(def.3)<-paste(tclvalue(def.3), tclvalue(def.2))
                       })
  but.2chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.2),
                                         header=TRUE, sep="\t"))})  
  #
  label.3 <- tklabel(genoframe, text="All Files")
  entry.3 <- tkentry(genoframe, text=def.3, width=30)
  #
  label.4 <- tklabel(genoframe, text="Output Prefix")
  entry.4 <- tkentry(genoframe, text=def.4, width=30)
  label.4b <- tklabel(genoframe, text="_gmtc.csv")
  #
  tkgrid.propagate(genoframe, FALSE)
  tkgrid(heading, columnspan=4)
  tkgrid(tklabel(genoframe, text=""), but.6)
  tkgrid(label.5, entry.5, but.5, but.5chk)
  tkgrid(label.1, entry.1, but.1, but.1chk)
  tkgrid(label.2, entry.2, but.2, but.2chk)
  tkgrid(label.3, entry.3)
  tkgrid(label.4, entry.4, label.4b)
  #
  tkgrid.configure(label.5, label.1, label.2, label.3, label.4, sticky="e")
  tkgrid.configure(entry.5, entry.1, entry.2, entry.3, entry.4,sticky="w")
  #
  but <- tkbutton(genoframe, text="Convert",
                  command=function(){
                    #
                    tkconfigure(tt, cursor="watch")
                    #
                    fnames <- unlist(strsplit(tclvalue(def.3), " "))
                    #
                    # Read in files.
                    purshia.mp <- list()
                    for(i in 1:length(fnames))
                      {
                      purshia.mp[[i]]<-read.table(fnames[i], header=TRUE,
                                                  sep="\t")
                      }
                    #
                    # Read in master file
                    gmtc <- read.table(tclvalue(def.5), header=TRUE, sep=",")
                    #
                    convert.fn <- function(purshia.mp, gmtc){
                      #
                      putr_data <- genotyper2genomatic(gmtc, purshia.mp)
                      #
                      setwd(out.dir)
                      #
                      putr_data <- condense(putr_data)
                      #
                      return(putr_data)
                      }
                    #
                    putr_data <- convert.fn(purshia.mp, gmtc)
                    #
                    write.table(putr_data,
                                paste(tclvalue(def.4), "_gmtc.csv", sep=""),
                                sep=",", row.names=FALSE)
                    #
#                    tkconfigure(genoframe, cursor="arrow")
                    tkconfigure(tt, cursor="arrow")
                    tkmessageBox(title="Genotyper Formatting",
                                 message="Data Formatted",
                                 type="ok")
                  })
  tkgrid(but, columnspan=2)
  }
#
# ##### ##### #####
#
# Managing Reruns. #
#
# ##### ##### #####
#
rerun <- function(){
  tkdestroy(genoframe)
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  def.1 <- tclVar("Filename")
  def.2 <- tclVar("Filename")
  def.3 <- tclVar("Prefix")
  #
  heading <- tklabel(genoframe, text="Rerun Management")
  label.1 <- tklabel(genoframe, text="Data File")
  entry.1 <- tkentry(genoframe, text=def.1, width=30)
  but.1  <- tkbutton(genoframe, text="Select File",
              command=function(){tclvalue(tkgetOpenFile(initialdir=out.dir))->tclvalue(def.1)})
  but.1chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.1), header=TRUE, sep=","))})
  #
  but.chk <- tkbutton(genoframe, text="Check For Reruns",
                command=function(){
                  #
                  tkconfigure(tt, cursor="watch")
                  #
                  putr_data <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                  putr_dups <- dup_checker(putr_data)
                  #
                  tkconfigure(tt, cursor="arrow")
                  #
                  if(is.null(putr_dups)==TRUE)
                    {
                    #
                    tkmessageBox(title="Duplicate Search",
                                 message="No Duplicates Found",
                                 type="ok")
                    #
                    } else {
                    #
                    setwd(out.dir)
                    write.table(putr_dups, "duplicates_gmtc.csv", sep=",",
                                row.names=FALSE)
                    #
                    tkmessageBox(title="Duplicate Search",
                                 message=paste(
                                           "Duplicates Found\n",
                                           "Check duplicates file\n",
                                           out.dir, "/duplicates_gmtc.csv", sep=""),
                                 type="ok")
                  }
                })
  #
  label.2 <- tklabel(genoframe, text="Processed Duplicates File")
  entry.2 <- tkentry(genoframe, text=def.2, width=30)
  but.2  <- tkbutton(genoframe, text="Select File",
              command=function(){tclvalue(tkgetOpenFile(initialdir=data.dir))->tclvalue(def.2)})
  but.2chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.2), header=TRUE, sep=","))})
  #
  label.3 <- tklabel(genoframe, text="Output Prefix")
  entry.3 <- tkentry(genoframe, text=def.3, width=30)
  label.3b <- tklabel(genoframe, text="_gmtc.csv")
  #
  but.rmd <- tkbutton(genoframe, text="Remove Reruns",
                command=function(){
                  #
                  tkconfigure(tt, cursor="watch")                  
                  #
                  putr_data <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                  putr_dups <- read.table(tclvalue(def.2), header=TRUE, sep=",")
                  putr_data <- dup_replace(putr_data, putr_dups)
                  putr_data <- order_sample(putr_data)
                  setwd(out.dir)
                  write.table(putr_data,
                              paste(tclvalue(def.3), "_gmtc.csv", sep=""),
                              sep=",", row.names=FALSE)
                  #
                  tkconfigure(tt, cursor="arrow")                  
                  #
                  tkmessageBox(title="Duplicates Removed",
                               message="Duplicates Removed",
                               type="ok")
                  })
  #
  tkgrid.propagate(genoframe, FALSE)
  tkgrid(heading, columnspan=4)
  tkgrid(label.1, entry.1, but.1, but.1chk)
  tkgrid(but.chk, columnspan=2)
  #
  tkgrid(but.chk, columnspan=2)
  #
  tkgrid(label.2, entry.2, but.2, but.2chk)
  tkgrid(label.3, entry.3, label.3b)
  tkgrid.configure(label.1, label.2, label.3, sticky="e")
  tkgrid.configure(entry.1, entry.2, entry.3, sticky="w")
  #
  tkgrid(but.rmd, columnspan=2)
  }
#
#
# ##### ##### ##### ##### #####
#
# Allele Characterization.    #
#
# ##### ##### ##### ##### #####
#
allelechar <- function()
  {
  tkdestroy(genoframe)
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  def.1 <- tclVar("Filename")
  def.3 <- tclVar('0.3')
  #
  heading <- tklabel(genoframe, text="Allele Characterization")
  label.1 <- tklabel(genoframe, text="Data File")
  entry.1 <- tkentry(genoframe, text=def.1, width=30)
  but.1  <- tkbutton(genoframe, text="Select File",
              command=function(){tclvalue(tkgetOpenFile(initialdir=data.dir))->tclvalue(def.1)})
  but.1chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.1), header=TRUE, sep=","))})
  #
  but.2  <- tkbutton(genoframe, text="Initialize allele files",
              command=function(){
                putr_data <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                gap <- as.numeric(tclvalue(def.3))
                #
                setwd(out.dir)
                bin_init(putr_data, gap)
                #
                tkmessageBox(title="Bin Initialization",
                             message="Bins Initialized!",
                             type="ok")

                })
  #
  label.3 <- tklabel(genoframe, text="Minimum Gap Size")
  entry.3 <- tkentry(genoframe, text=def.3, width=30)
  but <- tkbutton(genoframe, text="Characterize Alleles",
                command=function(){
                  #
                  tkconfigure(tt,cursor="watch")
                  #
                  putr_data <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                  loci <- names(putr_data)[seq(3, ncol(putr_data), by=2)]
                  loci <- substr(loci, 1, nchar(loci)-1)
                  #
                  setwd(out.dir)
                  locus.l <- list()
                  for (i in 1:length(loci))
                    {
                    locus.l[[i]]<-read.table(paste(loci[i], '_alleles.csv',
                                                   sep=''), header=TRUE,
                                             sep=',')
                    }
                  #
                  setwd(out.dir)
                  #
                  allele_process(putr_data, locus.l)
                  #
                  tkconfigure(tt,cursor="arrow")
                  #
                  tkmessageBox(title="Allele Characterization",
                               message="Alleles Characterized!",
                               type="ok")
                  })
  #
  tkgrid.propagate(genoframe, FALSE)
  tkgrid(heading, columnspan=4)
  tkgrid(label.1, entry.1, but.1, but.1chk)
  tkgrid(label.3, entry.3, but.2)
  tkgrid(but, columnspan=2)
  #
  tkgrid.configure(label.1, label.3, sticky="e")
  tkgrid.configure(entry.1, entry.3, sticky="w")
  }
#
# ##### ##### #####
#
# Scoring Bins.   #
#
# ##### ##### #####
#
scorebins <- function()
  {
  tkdestroy(genoframe)
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  def.1 <- tclVar("Filename")
  def.4 <- tclVar("Prefix")
  #
  heading <- tklabel(genoframe, text="Score Bins")
  label.1 <- tklabel(genoframe, text="Data File")
  entry.1 <- tkentry(genoframe, text=def.1, width=30)
  but.1  <- tkbutton(genoframe, text="Select File",
              command=function(){tclvalue(tkgetOpenFile(initialdir=data.dir))->tclvalue(def.1)})
  but.1chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.1), header=TRUE, sep=","))})
  #
  label.4 <- tklabel(genoframe, text="Outfile Prefix")
  label.4b <- tklabel(genoframe, text="_scored_gmtc.csv")
  entry.4 <- tkentry(genoframe, text=def.4, width=30)
  #
  but <- tkbutton(genoframe, text="Score Bins (may take a while)",
                command=function(){
                  tkconfigure(tt,cursor="watch")
                  #
                  putr_data <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                  loci <- names(putr_data)[seq(3,ncol(putr_data),by=2)]
                  loci <- substr(loci, 1, nchar(loci)-1)
                  #
                  setwd(out.dir)
                  #
                  bin_score(putr_data, loci,
                            paste(tclvalue(def.4), "_scored_gmtc.csv", sep=""))
                  #
                  tkconfigure(tt,cursor="arrow")
                  #
                  tkmessageBox(title="Bin Scoring",
                               message="Bins Scored",
                               type="ok")
                })
  #
  tkgrid.propagate(genoframe, FALSE)
  tkgrid(heading, columnspan=4)
  tkgrid(label.1, entry.1, but.1, but.1chk)
  tkgrid(label.4, entry.4, label.4b)
  tkgrid(but, columnspan=2)
  #
  tkgrid.configure(label.1, label.4, sticky="e")
  tkgrid.configure(entry.1, entry.4, sticky="w")
  }
#
# ##### #####
#
# Export.   #
#
# ##### #####
#
export <- function()
  {
  tkdestroy(genoframe)
  #
  genoframe <<- tkframe(tt, width=600, height=375)
  tkpack(genoframe)
  #
  def.1 <- tclVar("Filename")
  def.2 <- tclVar("Filename")
  def.3 <- tclVar("Prefix")
  #
  heading <- tklabel(genoframe, text="Export Data From Genomatic")
  label.1 <- tklabel(genoframe, text="Data File")
  entry.1 <- tkentry(genoframe, text=def.1, width=30)
  but.1  <- tkbutton(genoframe, text="Select File",
              command=function(){tclvalue(tkgetOpenFile(initialdir=data.dir))->tclvalue(def.1)})
  #
  label.3 <- tklabel(genoframe, text="Output Prefix")
  entry.3 <- tkentry(genoframe, text=def.3, width=30)  
  label.3b <- tklabel(genoframe, text="_format.csv")
  #
  but.1chk <- tkbutton(genoframe,
                       text="Check File",
                       command=function(){
                         edit(read.table(tclvalue(def.1), header=TRUE, sep=","))})
  but.2 <- tkbutton(genoframe, text="GenAlEx Format",
              command=function(){
                #
                putr_data <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                putr_data <- genobins(putr_data)
                setwd(out.dir)
                genomatic2genalex(putr_data, tclvalue(def.3))
                #
                tkmessageBox(title="Export",
                             message="GenAlEx File Created",
                             type="ok")
                })
  but.3 <- tkbutton(genoframe, text="NTSYSpc Format",
              command=function(){
                #
                putr_data <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                putr_data <- genobins(putr_data)
                setwd(out.dir)
                genomatic2ntsys(putr_data, tclvalue(def.3))
                #
                tkmessageBox(title="Export",
                             message="NTSYSpc File Created",
                             type="ok")
                })
  but.4 <- tkbutton(genoframe, text="Matrix Format",
              command=function(){
                #
                putr_data <- read.table(tclvalue(def.1), header=TRUE, sep=",")
                putr_data <- genobins(putr_data)
                setwd(out.dir)
                write.table(putr_data,
                            paste(tclvalue(def.3), "_matrix.csv", sep=''),
                            sep=",", row.names=FALSE)
                #
                tkmessageBox(title="Export",
                             message="Matrix File Created",
                             type="ok")
                })
  #
  tkgrid.propagate(genoframe, FALSE)
  tkgrid(heading, columnspan=4)
  tkgrid(label.1, entry.1, but.1, but.1chk)
  tkgrid(label.3, entry.3, label.3b)  
  tkgrid(but.2, but.3, but.4)
  #
  tkgrid.configure(label.1, sticky="e")
  tkgrid.configure(entry.1, sticky="w")
  }
#
#
#
# ##### ##### ##### ##### #####
#
# Initialize 'toplevel.'      #
#
# ##### ##### ##### ##### #####
#
tt <- tktoplevel(width=600)
tktitle(tt) <- "Genomatic"
#
genoframe <- tkframe(tt, width=600, height=375)
tkpack(genoframe)
#
screenw <- as.integer(tclvalue(tkwinfo("screenwidth", tt)))
screenh <- as.integer(tclvalue(tkwinfo("screenheight", tt)))
midscreen <- paste("+", (screenw/2)-300, "+", (screenh/2)-200, sep="")
#
tkwm.geometry(tt, midscreen)
#
#
# ##### ##### ##### ##### #####
#
# File menu options.          #
#
# ##### ##### ##### ##### #####
#
topMenu <- tkmenu(tt)
tkconfigure(tt, menu=topMenu)
#
fileMenu <- tkmenu(topMenu, tearoff=FALSE)
tkadd(fileMenu, "command", label="Exit Genomatic",
      command=function(){tkdestroy(tt)})
tkadd(topMenu, "cascade", label="File",
      menu=fileMenu)
#
#
#
# ##### ##### ##### ##### #####
#
# Genomatic menu options.     #
#
# ##### ##### ##### ##### #####
#
genomenu <- tkmenu(topMenu, tearoff=FALSE)
tkadd(genomenu, "command", label="Main",
      command=main)
tkadd(genomenu, "command", label="Input/output",
      command=inout)
tkadd(genomenu, "command", label="96-well maps",
      command=wellmap)
tkadd(genomenu, "command", label="ABI submission",
      command=abisub)
tkadd(genomenu, "command", label="Input genotypic data",
      command=gtyper2gmatic)
tkadd(genomenu, "command", label="Reruns",
      command=rerun)
tkadd(genomenu, "command", label="Allele characterization",
      command=allelechar)
tkadd(genomenu, "command", label="Score data",
      command=scorebins)
tkadd(genomenu, "command", label="Export",
      command=export)
tkadd(topMenu, "cascade", label="Genomatic",
      menu=genomenu)
#
data.dir <- R.home()
out.dir  <- R.home()
#
main()
}
#
#
##### ##### ##### ##### #####
##### ##### ##### ##### #####
# EOF.
