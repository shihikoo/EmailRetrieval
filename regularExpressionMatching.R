# Functions for regular expression Identification for data frame

# Read full text
readFullText <- function(txtfilename){
  
  readinFulltext <- function(inputarray){  
    return (readChar(inputarray[1], inputarray[2])) 
  }
  
  #--------- Calculate file size  -------------------------------------------------------------------------------
  fulltextsize <- file.info(txtfilename)$size
  
  #--------- Read text from file into myData$fulltext -----------------------------------------------------------
  fulltext <- apply(cbind(txtfilename = txtfilename, fulltextsize = fulltextsize), 1, readinFulltext)
  
  return(fulltext)
}

# Identify any pattern (regular expression) in fullText (array)
regularExpressionIdentification <- function(fullText, pattern){
  
  greplFunction <- function(text, pattern=""){  
    return(grepl(pattern, text, ignore.case = T, perl = T))
  }
  
  return(sapply(fullText, greplFunction, pattern = pattern))
}

# Clean up text from pdfExtractor new lines, dashlines
cleanText <- function(text, pdfExtractor=T, newLine=T, dashLine=T){
  if(pdfExtractor==T) text <- gsub("[(][a-zA-Z0-9. ]*PDF Extractor SDK[a-zA-Z0-9 .]*[)]","", text, perl=T)
  
  if(newLine==T) text <- gsub("\r|\n|\f"," ", text, perl=T)
  
  if(dashLine==T) text <- gsub("-","", text, perl=T)
  
  return(text)
}