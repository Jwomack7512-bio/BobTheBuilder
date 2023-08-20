library(dplyr)
library(stringr)

# Count your lines of R code
CountCodeLines <- function(directory, 
                           fileType, 
                           foldersToIgnore = NULL) {
  # @directory - path to explore for count
  # @filetype - (str) file extenstion (txt, css, py, etc)
  # @foldersToIgnore - (vec, str) paths to remove from search
  
  # Grab files in directory
  all.files <- list.files(path = directory, recursive = T, full.names = T)
  
  # Filter out ignored folders
  if (!is.null(foldersToIgnore)) {
    for (directory.name in foldersToIgnore) {
      all.files <- all.files[!grepl(directory.name, all.files)]
    }
  }
  
  # Extract fileType
  file.of.type <- all.files[endsWith(all.files, paste0(".", fileType))]
  
  num.lines <- file.of.type %>%
               sapply(function(x) x %>% readLines() %>% length()) %>%
               sum()
  
  return(num.lines)
}

base.directory <- getwd()

R.lines <- CountCodeLines(base.directory, "R")
print(R.lines)
css.lines <- CountCodeLines(base.directory, "css")
print(css.lines)
js.lines <- CountCodeLines(base.directory, "js", foldersToIgnore = "docs")
print(js.lines)
rst.lines <- CountCodeLines(base.directory, "rst")
print(rst.lines)

total.lines <- sum(R.lines, css.lines, js.lines, rst.lines)
print(paste0("Total number of lines in project: ", total.lines))
