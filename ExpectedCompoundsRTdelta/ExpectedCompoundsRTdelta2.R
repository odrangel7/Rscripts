
# ExpectedCompoundsMZDeltaRT.R    06/11/2023    Oscar Rangel-Huerta.
### Norwegian Veterinary Institute
# 
# Creates a column with the delta RT for each expected compound using the Parent compound as reference.
###The reference compound is considered as the only compound without transformation in the expected compounds table.
# This calculation is useful for quickly determine the difference in RT for the expected compounds
###the idea is to integrate this RT in the name for naming compounds in later stages.
#
# v0.1 beta
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


CD.input.ExpCmp$parentRT <- NA

## Loop through all Expected Compounds to find the parent compound
for (ci in 1:nrow(CD.input.ExpCmp)) {
  compChange <- CD.input.ExpCmp[ci, "Composition Change"]
  
  if (is.na(compChange) | compChange == "") {
    # This is the parent compound
    CD.input.ExpCmp[ci, "parentRT"] <- as.numeric(CD.input.ExpCmp[ci, "RT [min]"])
  }
}

# Calculate the RT difference for each compound with respect to its corresponding parent RT
rtFromPC <- numeric(nrow(CD.input.ExpCmp))

## Calculate the difference in RT for each compound with respect to the parent compound
for (ci in 1:nrow(CD.input.ExpCmp)) {
  cid <- CD.input.ExpCmp[ci, "Expected Compounds ID"]
  
  # Calculate the RT difference in minutes
  currRT <- as.numeric(CD.input.ExpCmp[ci, "RT [min]"])
  parentRT <- as.numeric(CD.input.ExpCmp[ci, "parentRT"])
  rtFromPC[ci] <- currRT - parentRT  # Assuming RT values are in minutes
}

# add result column to table
data.output <- cbind(CD.input.ExpCmp, "RT diff PC"=rtFromPC)

# Add new column to JSON structure.
#####MODIFIED FOR EXPECTED COMPOUNDS
newcolumn <-list()
newcolumn[[1]] = "Delta RT [min] from PC"   ## columnName
newcolumn[[2]] = ""     ## IsID
newcolumn[[3]] = "Float"      ## DataType
newcolumn[[4]] <- list(PositionAfter="RT [min]")    ## Options

names(newcolumn) <- c("ColumnName", "ID", "DataType", "Options")

CD_json_in$Tables[[1]]$ColumnDescriptions[[length(CD_json_in$Tables[[1]]$ColumnDescriptions) + 1]] <- newcolumn


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


