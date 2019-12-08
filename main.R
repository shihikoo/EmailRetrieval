# -----load library ----------
library(stringr)
source('regularExpressionMatching.R')
source('basicFunctions.R')
# ----- Functions ----------
Combine_emails <- function(emails){
  paste(unique(emails), collapse = ", ")
}

Extract_emails <- function(text){
  require(stringr)
  emailRegex <- '[[:alnum:].-_]+@[[:alnum:].-]+'
  return(str_extract_all(text,emailRegex))
}

Clean_emails <- function(emailList){
  emailList <- gsub("\"","", emailList)
  emailList <- gsub("mailto:","", emailList)
  emailList <- gsub("address:","", emailList)
  emailList <- gsub("addresses:","", emailList)
}

# -------- Read file names in data folder ----------
datafolder <- "data/"
outputFolder <-  "output/"

folderList <- list.dirs(path = datafolder, full.names = TRUE, recursive = FALSE)
fileList <- c()
for(folder in folderList){
  fileList <- c(fileList, list.files(path = folder, pattern = "(.[pP][dD][fF])$", all.files = FALSE, full.names = TRUE,
                                     recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
}
myData <- as.data.frame(as.array(fileList))
names(myData) <- "pdfFileLink"

# Convert Pdf to Text
myData$txtFileExist <- sapply(myData$pdfFileLink,  ConvertPdftoText,removePdf = TRUE)
# get text file names
myData$txtFileLink <- gsub(".pdf",".txt",myData$pdfFileLink)
# Read full text from the file
myData$fullText <- readFullText(myData$txtFileLink)

myData$fullText <- cleanText(myData$fullText)

myData$emails <- Extract_emails(myData$fullText)

myData$emailList <- sapply(myData$emails, Combine_emails)

myData$emailList <- Clean_emails(myData$emailList)

# -------- writes output data -----------
outputfileName <- paste(outputFolder,  "Output.txt", sep="")
outputData <- myData[,c("pdfFileLink", "txtFileExist" ,"emailList")]
write.table(outputData, outputfileName, quote = F, sep = "\t", row.names = F)
