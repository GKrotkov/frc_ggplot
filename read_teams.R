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
read_excel_allsheets <- function(filename, 
                                 tibble = FALSE, colnames = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  result <- lapply(sheets, function(X){
    readxl::read_excel(filename, sheet = X, col_names = colnames)
  })
  if(!tibble) result <- lapply(result, as.data.frame)
  names(result) <- sheets
  return(result)
}

# Reads a 1712-style .xlsm and isolates team data.
# Team sheets much be sheets with numbers and only numbers as the title.
# Any sheets that are not team data sheets must have at least one non-numeric
# character in their title.
# Inputs:
#     1) file: the filename of the file we're reading.
#     2) startRow: The first row of match data 1 = 1, 2 = 2, ...
#     3) endRow: The row of the final match of data. 1 = 1, 2 = 2, ...
#     4) startCol: The column of match #s. A = 1, B = 2, C = 3, ...
#     5) endCol: The last variable to read in. A = 1, B = 2, C = 3, ...
#     6) titleRow: The row containing the titles we want to use. 
# Outputs:
#     1) result: a list of dataframes, each for a single team.
read_teams_allsheets <- function(file, startRow, endRow, 
                                 startCol, endCol, titleRow){
  # Extract teams from full file, ignoring any non-team data. 
  # Defines non-team sheets as sheet for which the title, when coerced to 
  # a numeric, will return NA. 
  raw <- read_excel_allsheets(file)
  teams <- raw[!is.na(as.numeric(names(raw)))]
  result <- vector("list", length = 60)
  names(result) <- names(teams)
  for(i in 1:length(teams)){
    tmp <- teams[[i]]
    tmp <- tmp[startRow:endRow, startCol:endCol]
    colnames(tmp) <- teams[[i]][titleRow, startCol:endCol]
    result[[i]] <- tmp
  }
  return(result)
}

# Simple function that returns the number of NAs in a column
countNA <- function(column){
  return(sum(is.na(column)))
}

# Function that manually coerces as much of the data as possible into 
# a form better for ggplot use. This does not take input as for which 
# data type each column should be, so it is a crude conversion. 
# Converts to numeric if possible. Converts strings to factors. 
# If can be neither numeric nor factor, will left left alone. 
# Inputs:
#   1) teams: a list of dataframes, each dataframe being a team at an event.
# Outputs:
#   1) teams: the same list of dataframes, but with data types coerced if 
#     helpful. 
manual_coerce <- function(teams){
  for(i in 1:length(teams)){
    for(j in 1:ncol(teams[[i]])){
      # If I can make it numeric, I do.
      # If coerceing to numeric changes the number of NAs, then we shouldn't
      # coerce to numeric. 
      if(countNA(teams[[i]][, j]) == countNA(as.numeric(teams[[i]][, j]))){
        teams[[i]][, j] <- as.numeric(teams[[i]][, j])
      }
      # If it can't be made numeric, make it a factor.
      else{
        teams[[i]][, j] <- factor(teams[[i]][, j])
      }
    }
  }
  return(teams)
}

# A simple wrapper function to make the interface for reading all teams
# more direct. 
read_team_sheets_coerced <- function(file, startRow, endRow, 
                                     startCol, endCol, titleRow){
  return(manual_coerce(read_teams_allsheets(file, startRow, endRow, 
                                            startCol, endCol, titleRow)))
}

# TODO: write a function that makes all NAs 0 in the team sheets. 
# TODO: write a function that renames the columns for ease of use later.