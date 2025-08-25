###############################################################################
# Function: preprocess_RESEdates
# Description:
#   Turn RESE dates (PCC format like "01Jan2020", possibly with [[lcen]]/[[rcen]])
#   into POSIXct columns *_posoxctformat. Warn if any dates fail to parse.
# Input:
#   - RESELOC: data.frame with res_entry_start / res_entry_end (character)
# Output:
#   - RESELOC with res_entry_start_posoxctformat / res_entry_end_posoxctformat
###############################################################################

preprocess_RESEdates <- function(RESELOC) {

  # remember locale, switch to a safe one for %b month parsing, restore on exit
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  suppressWarnings({
    ok <- Sys.setlocale("LC_TIME", "C")
    if (is.na(ok)) Sys.setlocale("LC_TIME", "English")
  })

  # strip censor tags and normalize empties to NA
  strip_tags <- function(x) {
    x <- gsub("[[rcen]]", "", x, fixed = TRUE)
    x <- gsub("[[lcen]]", "", x, fixed = TRUE)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }

  RESELOC$res_entry_start <- strip_tags(RESELOC$res_entry_start)
  RESELOC$res_entry_end   <- strip_tags(RESELOC$res_entry_end)

  # parse to POSIXct (PCC uses like 01Jan2020)
  RESELOC$res_entry_start_posoxctformat <-
    as.POSIXct(as.character(RESELOC$res_entry_start), format = "%d%b%Y", tz = "UTC")
  RESELOC$res_entry_end_posoxctformat   <-
    as.POSIXct(as.character(RESELOC$res_entry_end),   format = "%d%b%Y", tz = "UTC")

  # quick warning if any parse failed
  anystartdatesmissing <- sum(is.na(RESELOC$res_entry_start_posoxctformat)) > 0
  anyenddatesmissing   <- sum(is.na(RESELOC$res_entry_end_posoxctformat))   > 0

  if (anystartdatesmissing || anyenddatesmissing) {
    message(
      "WARNING: not all dates could be converted successfully. ",
      "Missing start: ", sum(is.na(RESELOC$res_entry_start_posoxctformat)),
      " | Missing end: ", sum(is.na(RESELOC$res_entry_end_posoxctformat))
    )
  }

  RESELOC
}

###############################################################################
# Function: check_anyNAinRESEdates
# Returns TRUE if there are any NAs in either parsed date column.
###############################################################################
check_anyNAinRESEdates <- function(RESELOC) {
  anystartdatesmissing <- sum(is.na(RESELOC$res_entry_start_posoxctformat)) > 0
  anyenddatesmissing   <- sum(is.na(RESELOC$res_entry_end_posoxctformat))   > 0
  anystartdatesmissing || anyenddatesmissing
}
