#
# CompoundsMZ.R    03/29/2020    Ralf Tautenhahn
# 
# Creates a column with m/z values for each compound, positioned right after the Molecular Weight column in the Compounds table
# To populate the "m/z" column it currently performs the following procedure. For each compound it finds the sample with highest abundance for this compound, looks for a molecular ion, uses ion with highest abundance if a molecular ion was not detected. 
#
# v1.0
# 


# Read arguments from CD.
args <- commandArgs()

# At least for now, the 6th argument is the name of the JSON file
inputFile <- args[6]

# Open JSON file, find exported files, read into tables
library(rjson)
CD_json_in <- fromJSON(file=inputFile)

datafile1 <- CD_json_in$Tables[[1]]$DataFile
CD.input.Cmp <- read.table(datafile1, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

datafile2 <- CD_json_in$Tables[[2]]$DataFile
CD.input.CmpPerFile <- read.table(datafile2, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

datafile3 <- CD_json_in$Tables[[3]]$DataFile
CD.input.Features <- read.table(datafile3, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

datafile4 <- CD_json_in$Tables[[4]]$DataFile
CD.input.Cmp_CmpPerFile <- read.table(datafile4, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

datafile5 <- CD_json_in$Tables[[5]]$DataFile 
CD.input.CmpPerFile_Features <- read.table(datafile5, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)

# save.image(file="C:\\Users\\ralf.tautenhahn\\Documents\\temp\\CD node Rimage.dat")
# load("C:\\Users\\ralf.tautenhahn\\Documents\\temp\\CD node Rimage.dat")

# create result vector (here the m/z values)
mz=double(nrow(CD.input.Cmp))

## loop through all Compounds
for (ci in 1:nrow(CD.input.Cmp)) {
 
  cid <- CD.input.Cmp[ci,"Compounds ID"]
  cpFidB <- CD.input.Cmp_CmpPerFile[,'Compounds ID'] %in% cid
  cpFid <- CD.input.Cmp_CmpPerFile[cpFidB,'Compounds per File ID']
  
  # use the sample with the highest peak area
  cmpperfilehits <- CD.input.CmpPerFile[cpFid,]
  imaxArea <- which.max(cmpperfilehits[,"Area"])
  cpFidMax <- cpFid[imaxArea] 
  
  #get the features for this sample and compound ID
  fidB <- CD.input.CmpPerFile_Features[,'Compounds per File ID'] %in%  cpFidMax
  fid <- CD.input.CmpPerFile_Features[fidB,'Features ID']
  
  fiB <- CD.input.Features[,'Features ID'] %in% fid
  features <- CD.input.Features[fiB,]
  
  # try to use molecular ion 
  ift <- features[,"Ion"] %in% "[M+H]+1"
  if (!any(ift))
    ift <- features[,"Ion"] %in% "[M-H]-1"
  
  # or else ion with highest peak area
  if (!any(ift))
    ift <- which.max(features[,"Area"])
  else
    if (length(which(ift)) > 1) # rare case of multiple peaks, pick largest
      ift <- which.max(features[ift,"Area"])
    
  mz[ci] <- features[ift,"mz"]
}


# add result column to table
data.output <- cbind(CD.input.Cmp, "m/z" = mz)

# Add new column to JSON structure.
newcolumn <- list()
newcolumn[[1]] = "m/z"     ## ColumnName
newcolumn[[2]] = ""     ## IsID
newcolumn[[3]] = "Float"      ## DataType
newcolumn[[4]] <- list(PositionAfter="Molecular Weight")

names(newcolumn) <- c("ColumnName", "ID", "DataType", "Options")

CD_json_in$Tables[[1]]$ColumnDescriptions[[length(CD_json_in$Tables[[1]]$ColumnDescriptions) + 1]] <- newcolumn

# Write modified table to temporary folder.

resultout <- gsub(".txt", ".out.txt", datafile1)
write.table(data.output, file = resultout, sep='\t', row.names = FALSE)
#write.table(data.output, file = "C:\\RScripts\\WorkflowInputFile.txt", sep='\t', row.names = FALSE)

# Write out node_response.json file - use same file as node_args.json but change the pathway input file to the new one

CD_json_in$Tables[[1]]$DataFile = resultout
jsonOutFile <- CD_json_in$ExpectedResponsePath

# Try removing all the other tables in the JSON so that only the new Compounds table is used

CD_json_in$Tables[5] <- NULL;
CD_json_in$Tables[4] <- NULL;
CD_json_in$Tables[3] <- NULL;
CD_json_in$Tables[2] <- NULL;

responseJSON <- toJSON(CD_json_in, indent=1, method="C")

# responseJSON has incorrect format for the empty Options lists.  Will use a regular expression to find and replace the [\n\n\] with the {}

responseJSON2 <- gsub("\\[\n\n[[:blank:]]+\\]", "{ }", responseJSON)

jsonfileconn <- file(jsonOutFile)
backupfileconn <- file("C:\\RScripts\\node_response.json")

writeLines(responseJSON2, jsonfileconn)
writeLines(responseJSON2, backupfileconn)

close (jsonfileconn)
close (backupfileconn)


