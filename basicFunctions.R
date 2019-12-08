#---------- Clean up data from Meta-analysis Data Base (ACCESS)-----------
# Created by Jing Liao
# Created on May 31 2017
#-------------------------------------------------------------------------

#---------- Libraries and Functions ----------------------------------------
LocateFolder <- function(DocumentLink) {
  library(stringr)
  
  if (grepl("http",
            DocumentLink,
            perl = T,
            ignore.case = T)) {
    return("")
  }
  
  if (grepl("Publications",
            DocumentLink,
            perl = T,
            ignore.case = T))
  {
    return(str_extract(DocumentLink, "(?<=Publications[/]).+?(?=[/])"))
  }
  else {
    return(str_extract(DocumentLink, "^.+?(?=[/])"))
  }
}

validURL <- function(fileLink) {
  result <- tryCatch({
    foo <- !http_error(fileLink)
    return(foo)
  }
  , warning = function(war) {
    return(F)
  }
  , error = function(err) {
    return(F)
  }
  , finally = {
  })
}

DirectoryCreation <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

CopyPdf <- function(fileLink, fileLinkNew, overwirte = F)
{
  file.copy(fileLink, fileLinkNew, overwrite = F)
  
  return(file.exists(fileLinkNew))
}

#Convert Pdf to Text
ConvertPdftoText <- function(pdfLink, ignoreExistingTextFile = FALSE, removePdf = FALSE)
{
  
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  
  txtLink <- sub('.pdf', '.txt', pdfLink)
  if(file.exists(txtLink) & ignoreExistingTextFile == FALSE) 
  {
    if(removePdf) file.remove(pdfLink)
    return(TRUE)
  }
  
  os <- get_os()
  if(os == "linux")  exe <- '"pdftotext"'
  else  exe <- '"C:\\xpdfbin-win-3.04\\bin64\\pdftotext.exe"'
  
  if(file.exists(pdfLink))
  {
    com <- paste(exe, paste('"',pdfLink,'"',sep=''))
    statusCode <- system(com, wait = T)
    
    if(statusCode == 0) 
      { 
      if(removePdf) file.remove(pdfLink)
      return(TRUE)
    }
    else {
      # print(pdfLink)
      # print("----------------------------")
      return(FALSE)
    }
  }
  return(FALSE)
}
