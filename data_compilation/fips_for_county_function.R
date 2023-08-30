# function to append FIPS code to compiled CCC data
# dependencies: tigris, stringr

fips_for_county <- function(data,
                            resolved_county_col = "resolved_county",
                            resolved_state_col = "resolved_state",                  
                            city_col = "resolved_locality",
                            state_col = "state",
                            location_detail_col = "location_detail",
                            newcol = "fips_code") {

  require(tigris)
  require(stringr)

  # lookup vector for independent cities in VA
  va_independent_cities <- c("Alexandria", "Bristol", "Buena Vista", "Charlottesville", "Chesapeake",
                             "Colonial Heights", "Covington", "Danville", "Emporia", "Fairfax",
                             "Falls Church", "Franklin", "Fredericksburg", "Galax", "Hampton",
                             "Harrisonburg", "Hopewell", "Lexington", "Lynchburg", "Manassas",
                             "Manassas Park", "Martinsville", "Newport News", "Norfolk", "Norton",
                             "Petersburg", "Poquoson", "Portsmouth", "Radford", "Richmond",
                             "Roanoke", "Salem", "Staunton", "Suffolk", "Virginia Beach",
                             "Waynesboro", "Williamsburg", "Winchester")
  va_cities_fips <- c("510", "520", "530", "540", "550",
                      "570", "580", "590", "595", "600",
                      "610", "620", "630", "640", "650",
                      "660", "670", "678", "680", "683",
                      "685", "690", "700", "710", "720",
                      "730", "735", "740", "750", "760",
                      "770", "775", "790", "800", "810",
                      "820", "830", "840")

  X <- data

  # create a new col in the data frame called newcol (defaults to "fips_code" ) with fips codes;
  # use apply() to iterate process rowwise
  X[,newcol] <- apply(X, 1, function(j) {

     z <- NA

     # if state or county is missing or state abbreviation is invalid, leave it as NA
     if(is.na(j[resolved_state_col]) | is.na(j[resolved_county_col]) | !(j[resolved_state_col] %in% state.abb)) {

       z <- NA

     # otherwise, use lookup_code() from 'tigris' to get fips code
     } else {

       y <- tigris::lookup_code(j[resolved_state_col], iconv(j[resolved_county_col], from = 'UTF-8', to = 'ASCII//TRANSLIT'))

       state <- stringr::str_extract(y, "\\d{2}")

       county <- stringr::str_extract(y, "\\d{3}")

       z <- ifelse(is.na(state) | is.na(county), NA, paste0(state, county))

     }

     # handling a bunch of exceptions that get NAs from tigris

     if(isTRUE(j[resolved_state_col] == "DC")) { z <- "11001" }

     if(isTRUE(j[resolved_state_col] == "MD" & j[city_col] == "Baltimore")) { z <- "24510" }

     if(isTRUE(j[resolved_state_col] == "AK" & j[resolved_county_col] == "Petersburg Borough")) { z <- "02195" }

     if(isTRUE(j[resolved_state_col] == "MO" & j[city_col] == "St. Louis")) { z <- "29510" }

     if(isTRUE(j[resolved_state_col] == "NM" & j[resolved_county_col] == "Doña Ana County")) { z <- "35013" }

     if(isTRUE(j[resolved_state_col] == "VA" & j[city_col] %in% va_independent_cities)) {

       z <- paste0("51", va_cities_fips[match(j[city_col], va_independent_cities)])

     }

     if(isTRUE(j[resolved_state_col] == "NV" & j[city_col] == "Carson City")) { z <- "32510" }

     if(isTRUE(j[resolved_state_col] == "NY" & j[city_col] == "New York")) {

       if(isTRUE(grepl("Manhattan", j[location_detail_col]))) {

         z <- "36061"

       } else if(isTRUE(grepl("Brooklyn", j[location_detail_col]))) {

         z <- "36047"

       } else if(isTRUE(grepl("Queens", j[location_detail_col]))) {

         z <- "36081"

       } else if(isTRUE(grepl("Bronx", j[location_detail_col]))) {

         z <- "36005"

       } else if(isTRUE(grepl("Staten Island", j[location_detail_col]))) {

         z <- "36085"

       } else {

         z <- "36061"  # defaults to Manhattan when no location detail given

       }

     }

     if(isTRUE(j[state_col] == "AS")) { z <- "60000" }

     if(isTRUE(j[state_col] == "GU")) { z <- "66000" }

     if(isTRUE(j[state_col] == "MP")) { z <- "69000" }

     if(isTRUE(j[state_col] == "PR")) { z <- "72000" }

     if(isTRUE(j[state_col] == "VI")) { z <- "78000" }

     return(z)

  })

  return(X)

}

# version using 'usmap' that fails for Alaska
# fips_for_county <- function(data,
#                             resolved_county_col = "resolved_county",
#                             resolved_state_col = "resolved_state",
#                             newcol = "fips_code") {
#
#   require(usmap)
#
#   X <- data
#
#   X[,newcol] <- apply(X, 1, function(j) {
#
#      x <- try(fips(state = j[resolved_state_col],
#                    county = j[resolved_county_col]),
#                    silent = TRUE)
#
#      x <- ifelse(grepl("Error", x), NA, x)
#
#   })
#
#   return(X)
#
# }
