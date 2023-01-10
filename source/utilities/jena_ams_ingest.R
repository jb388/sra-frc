#' @author J. Beem-Miller
#' @description Reads Jena AMS reports (.xlsx) files
#' @details Function first checks that the target file matches the template, then strips the headers out of the file and returns a data frame with the data values
#' @param template_file Filename, character
#' @return list of data frames
#' @import openxlsx

library(openxlsx)

read_jena_ams_results <- function(jena_ams_dir, template_file, start = 27) {
  if(missing(template_file)) {
    template_file <- "../data/raw/ams_jena_template_2020-04-22/ams_jena_template.xlsx"
  }
  
  # start input at column header row (27)
  template <- read.xlsx(template_file, startRow = start)

  # get pathnames for .xlsx files in jena_ams_dir
  data_files <- list.files(jena_ams_dir, pattern = "\\.xlsx", full.names = TRUE)
  # remove open .xlsx files
  data_files <- grep(data_files, pattern='\\~\\$', inv=T, value=T)
  
  # check template format and data structure
  for(i in seq_along(data_files)) {
    
    # check for template version
    df <- read.xlsx(data_files[i], startRow = start)
    if (names(df)[1] != "P-Nr.") {
      start <- 31
      template_file <- "../data/raw/ams_jena_template_2022-01-24/ams_jena_template.xlsx"
      template <- read.xlsx(template_file, startRow = start)
      df <- read.xlsx(data_files[i], startRow = start)
    }
    
    # check that names match
    nms <- names(df)
    for(j in seq_along(nms)) {
      if(names(df)[j] != names(template)[j])
        cat(paste("Row", start, "of ", data_files[i], " does not contain proper column names"))
    }
  }
  
  # read in files
  data_ls <- lapply(seq_along(data_files), function(i) {
    read.xlsx(data_files[i], startRow = start)
  })
  
  names(data_ls) <- grep(list.files(jena_ams_dir, pattern = "\\.xlsx"),pattern='\\~\\$', inv=T, value=T)

  return(data_ls)
}
