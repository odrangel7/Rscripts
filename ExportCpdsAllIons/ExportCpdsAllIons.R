#
# ExportCpdsAllIons.R    01/27/2023    Ralf Tautenhahn       ralf.tautenhahn@thermofisher.com
# 
#
# v1.0
# 
#  Export the Compounds table with ALL* ions for each compound
#  
#  *ions are based on the sample with the highest peak area for each compound
#
#

#AREA="Area Ref. Ion"
AREA="Area (All Ions)"

getTableIdx <- function(JSON_in, name) {
  TableNames <- sapply (JSON_in$Tables, function(x) x$TableName)
  which(TableNames %in% name)
}
  
getTable <- function(JSON_in, name) {
  
  TableNames <- sapply (JSON_in$Tables, function(x) x$TableName)
  idx <- TableNames %in% name
  
  if (!any(idx)) 
    stop("Table not found.")
  
  read.table(JSON_in$Tables[[ which(idx) ]]$DataFile, 
             header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)
}

# Read arguments from CD.
args <- commandArgs()

# At least for now, the 6th argument is the name of the JSON file
inputFile <- args[6]

# Open JSON file, find exported files, read into tables
library(rjson)
CD_json_in <- fromJSON(file=inputFile)

Compounds <- getTable(CD_json_in, "Compounds")
CompoundsPerFile <- getTable(CD_json_in, "Compounds per File")
FeaturesPerFile <- getTable(CD_json_in, "Features per File")
Compounds_CompoundsPerFile_ID <- getTable(CD_json_in, "ConsolidatedUnknownCompoundItem-UnknownCompoundInstanceItem")
CompoundsPerFile_FeaturesPerFile_ID <-  getTable(CD_json_in, "UnknownCompoundInstanceItem-UnknownCompoundIonInstanceItem")


## prepare a list for output
resL <- vector("list", nrow(Compounds))
  
## loop through all Compounds
for (ci in 1:nrow(Compounds)) {
 
  cid <- Compounds[ci,"Compounds ID"]
  
  cpFid <- Compounds_CompoundsPerFile_ID[ (Compounds_CompoundsPerFile_ID[,'Compounds ID'] %in% cid)   ,'Compounds per File ID']
  
  # use the sample with the highest peak area
  maxArea <- max(CompoundsPerFile[cpFid,AREA])
  cpFidMax <- cpFid[CompoundsPerFile[cpFid,AREA] %in% maxArea]

  #get the features for this sample and compound ID
  fid <- CompoundsPerFile_FeaturesPerFile_ID[ CompoundsPerFile_FeaturesPerFile_ID[,'Compounds per File ID'] %in%  cpFidMax   ,'Features per File ID']
  features <- FeaturesPerFile[ FeaturesPerFile[,'Features per File ID'] %in% fid  ,]
  
  #results for this compound
  resL[[ci]] <- cbind("Name"=Compounds[ci,"Name"],"Formula"=Compounds[ci,"Formula"], "Calc. MW"=Compounds[ci,"Calc. MW"], features[,c("Ion", "m/z", "Area")], "RT [min]"=Compounds[ci,"RT [min]"])
  
}  

data.output <- do.call("rbind", resL)

resultout = paste(CD_json_in$ResultFilePath,"Export_All_Ions.csv",sep=".")
write.csv(data.output, file = resultout, row.names = FALSE)



