#utils to manipulate the RCID table to extract useful info

#' function to create a mapping of the country and country codes
#what this basically does is to take the RCID table and split the RCID column to country code and number
#' @return the data frame for the RCID with two extra columns
map_country_codes <- function(df){

  #for this used https://stackoverflow.com/questions/37604391/how-do-i-make-a-column-from-substring-of-another-column-in-r
  df$RC.Number <- stringr::str_sub(df$Research.Cycle.ID, -4)
  df$Country.Code <- stringr::str_sub(df$Research.Cycle.ID, 0, 3)
  return(df)
}


#' function to map the cycle numbers to the years
#' @return df with extra column with year definition
map_cycle_year <- function(df){

  df <- mutate(df, Year = (as.integer(as.integer(RC.Number)/100))+2000)
  return(df)
}


test_lappl_funct <- function(x, y){

  #x e il vettore riga della df legacy, y e la df rcid
  list_of_ind <- which(y$Country == x["Country"])
  x["RC.ID"] <- y$RC.ID[list_of_ind[1]]
}
