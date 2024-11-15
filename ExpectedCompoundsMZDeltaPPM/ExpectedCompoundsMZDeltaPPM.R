#
# ExpectedCompoundsMZDeltaPPM.R    04/05/2020    Ralf Tautenhahn, Christopher Hu
# 
# Creates a column with m/z values for each expected compound, based on features detected in the sample with highest abundance for this compound
# Also pushes the delta mass in ppm for this feature to the Expected Compounds table
#
# v1.1
# 


# Read arguments from CD.
args <- commandArgs()

# At least for now, the 6th argument is the name of the JSON file
inputFile <- args[6]

# Open JSON file, find exported files, read into tables
library(rjson)
CD_json_in <- fromJSON(file=inputFile)

datafile1 <- CD_json_in$Tables[[1]]$DataFile 
CD.input.ExpCmp <- read.table(datafile1, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

datafile2 <- CD_json_in$Tables[[2]]$DataFile
CD.input.CmpPerFile <- read.table(datafile2, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

datafile3 <- CD_json_in$Tables[[3]]$DataFile
CD.input.ExpFeatures <- read.table(datafile3, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

datafile4 <- CD_json_in$Tables[[4]]$DataFile
CD.input.ExpCmp_ExpCmpPerFile <- read.table(datafile4, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

datafile5 <- CD_json_in$Tables[[5]]$DataFile
CD.input.ExpCmpPerFile_ExpFeatures <- read.table(datafile5, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)


# save.image(file="C:\\Users\\ralf.tautenhahn\\Documents\\temp\\CD node Rimage.dat")
# load("C:\\Users\\ralf.tautenhahn\\Documents\\temp\\CD node Rimage.dat")

# create result vector: m/z values
mz=double(nrow(CD.input.ExpCmp))

# create result vector: delta ppm values
DeltaPPM = double(nrow(CD.input.ExpCmp))

## loop through all Expected Compounds
for (ci in 1:nrow(CD.input.ExpCmp)) {
 
  cid <- CD.input.ExpCmp[ci,"Expected Compounds ID"]
  ecpfid1 <- CD.input.ExpCmp_ExpCmpPerFile[,'Expected Compounds ID'] %in% cid
  ecpfid2 <- CD.input.ExpCmp_ExpCmpPerFile[ecpfid1,'Expected Compounds per File ID']
  
  # use the one with the highest area
  cmpperfilehits <- CD.input.CmpPerFile[ecpfid2,]
  imaxArea <- which.max(cmpperfilehits[,"Area"])
  ecpfid3 <- ecpfid2[imaxArea]
  
  efid1 <- CD.input.ExpCmpPerFile_ExpFeatures[,'Expected Compounds per File ID'] %in% ecpfid3
  efid2 <- CD.input.ExpCmpPerFile_ExpFeatures[efid1,'Expected Features ID']
  
  efi1 <- CD.input.ExpFeatures[,'Expected Features ID'] %in% efid2
  expFeatures <- CD.input.ExpFeatures[efi1,]
  
  # use the featues with highest Area
  iexpF <- which.max(expFeatures[,"Area"])
  mz[ci] <- expFeatures[iexpF,"mz"]
  DeltaPPM[ci] <- expFeatures[iexpF, "Delta Mass in ppm"]
  
}


# add result column to table
data.output <- cbind(CD.input.ExpCmp, "m/z"=mz, "Delta Mass [ppm]"=DeltaPPM)

# Add new column to JSON structure.
newcolumn <- list()
newcolumn[[1]] = "m/z"     ## ColumnName
newcolumn[[2]] = ""     ## IsID
newcolumn[[3]] = "Float"      ## DataType
newcolumn[[4]] <- list(PositionAfter="Molecular Weight")    ## Options

names(newcolumn) <- c("ColumnName", "ID", "DataType", "Options")


newcolumn2 <-list()
newcolumn2[[1]] = "Delta Mass [ppm]"   ## columnName
newcolumn2[[2]] = ""     ## IsID
newcolumn2[[3]] = "Float"      ## DataType
newcolumn2[[4]] <- list(PositionBefore="Dealkylated", FormatString="F2")    ## Options

names(newcolumn2) <- c("ColumnName", "ID", "DataType", "Options")

CD_json_in$Tables[[1]]$ColumnDescriptions[[length(CD_json_in$Tables[[1]]$ColumnDescriptions) + 1]] <- newcolumn
CD_json_in$Tables[[1]]$ColumnDescriptions[[length(CD_json_in$Tables[[1]]$ColumnDescriptions) + 1]] <- newcolumn2

# Write modified table to temporary folder.

resultout <- gsub(".txt", ".out.txt", datafile1)
write.table(data.output, file = resultout, sep='\t', row.names = FALSE)

# Write out node_response.json file - use same file as node_args.json but change the pathway input file to the new one

CD_json_in$Tables[[1]]$DataFile = resultout
jsonOutFile <- CD_json_in$ExpectedResponsePath

# Try removing all the other tables in the JSON so that only the Expected Compounds table is used

CD_json_in$Tables[5] <- NULL;
CD_json_in$Tables[4] <- NULL;
CD_json_in$Tables[3] <- NULL;
CD_json_in$Tables[2] <- NULL;

responseJSON <- toJSON(CD_json_in, indent=1, method="C")

# responseJSON has incorrect format for the empty Options lists.  Will use a regular expression to find and replace the [\n\n\] with the {}

responseJSON2 <- gsub("\\[\n\n[[:blank:]]+\\]", "{ }", responseJSON)

jsonfileconn <- file(jsonOutFile)

writeLines(responseJSON2, jsonfileconn)

close (jsonfileconn)



