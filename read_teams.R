# This function reads team data from a .xlsx file
# Expects data in a tidy format as follows:
# 1   team  var1    var2    var3    var4 ...
# 2   1     obs1    obs1    obs1    obs1 ...
# 3   2     obs2    obs2    obs2    obs2 ...
# 4   3     obs3    obs3    obs3    obs3 ...
# ...
# If everything but the first column in a given row is NA, consider that
# match as unplayed (unsure if this matters.)
# Can input a `skip` parameter to have the code ignore that row systematically.
# Inputs:
#   1) teams: the number of teams in the .xlsx file
#   2) rows: the number of rows of observations 
#             (defaults to 12, the standard number in most districts)
#   3) cols: the number of cols of observations (variables you track)
#   4) skip: a vector of numeric indicies of matches to skip for each team.
#     NOTE: for skips, you are constrained between 1 and rows!
# Output:
#   1) data: list, length = teams, with all the team data stored in it.
library(readxl)
library(assertthat)
read_teams <- function(file, teams, rows = 12, skip = NULL){
  # Check the skip isn't null and is between 1 and rows
  assert_that(is.null(skip) | all(skip <= rows & skip >= 1), 
              msg = "skip is not a valid input")
  raw <- read_xlsx(file, col_names = FALSE)
  data <- vector("list", length = teams)
  for(i in 1:teams){
    # Set the value
    matches <- raw[((i - 1) * (rows) + i):((i * (rows)) + i), ]
    colnames(matches) <- raw[((i - 1) * (rows) + i), ]
    matches <- matches[-1, ]
    if(!is.null(skip)){
      matches <- matches[-(skip), ]
    }
    data[[i]] <- matches
    # If expected to skip every time, delete that/those row/rows.
    # Set the name
    names(data)[i] <- colnames(matches)[1]
  }
  return(data)
}

library(readxl)   
# Code adapted from Jeromy Anglim, url:
# "https://stackoverflow.com/questions/12945687/
# read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames"
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}
