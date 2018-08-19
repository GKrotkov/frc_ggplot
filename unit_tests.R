library(testthat)
test_read_teams <- function(){
  source("read_teams.R")
  test_that("read_teams works on good data", {
    teams <- read_teams("experimentalData.xlsx", 60, 12)
    expect_true(all.equal(names(teams), 
                          c("11", "25", "41", "56", "75", "102", "103", 
                            "203", "219", "222", "223", "225", "272", "293", 
                            "303", "316", "321", "365", "433", "708", "747", 
                            "834", "1089", "1143", "1168", "1218", "1257", 
                            "1391", "1403", "1640", "1676", "1712", "1807", 
                            "1923", "1989", "2016", "2180", "2495", "2539",
                            "2590", "2600", "2607", "2729", "3142", "3314", 
                            "3637", "3929", "3974", "4342", "4361", "4653", 
                            "5401", "5404", "5407", "5420", "5895", "5992", 
                            "6203", "6860", "6495")))
    expect_true(all.equal(teams$"1712"$"Exchange Placed",
                          c("8", NA, NA, NA, NA, "4", NA, 
                            "7", "3", NA, NA, NA)))
  })
  
  test_that("read_teams exempts correctly", {
    teams <- read_teams("experimentalData.xlsx", 60, 12, 1:12)
    result <- vector(length = 60)
    for(i in range(60)){
      result[i] <- nrow(teams[[i]])
    }
    expect_true(length(teams) == 60)
    expect_true(all.equal(result, rep(0, 60)))
  })
  
  test_that("Read teams requires correct inputs", {
    expect_error(read_teams("experimentalData.xlsx", 
                            60, 12, "fred"))
    expect_error(read_teams("experimentalData.xlsx", 
                            60, 12, 13))
    expect_error(read_teams("experimentalData.xlsx", 
                            60, 12, c(1, 50)))
  })
}


test_read_teams()