# This files contain functions for EMOCEMP analysis, it should be in the same directory 
# as the other scripts to work
# Author: Rafael Sommer
# --------------------------------------------------------------------------------------

prob_entires <- function(x) {
  num_entries <- suppressWarnings(as.numeric(x))
  ind <- is.na(num_entries)
  ind
}

merge_emocemp <- function(x) {
        
}

