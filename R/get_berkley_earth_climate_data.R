#' query climate data from https://berkeleyearth.org/data/
#'
#' @param focalCountry A country name, see 'countryNamesBerkley' within function for formatting and conventions
#'
#' @return
#' @export
#'
#' @examples
#'     # this should work
#'     get_berkley_earth_climate_data( "Cook Islands" )
#'     # this should return an error
#'     get_berkley_earth_climate_data( "Cook-Islands" )


get_berkley_earth_climate_data <- function( focalCountry = "Tuvalu" ){

          countryNamesBerkley <-  c( "Afghanistan", "Albania", "Algeria", "Andorra", "Angola",
                                    "Anguilla", "Antigua and Barbuda", "Argentina", "Armenia",
                                    "Aruba", "Australia", "Austria", "Azerbaijan", "Bahrain",
                                    "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize",
                                    "Benin", "Bermuda", "Bhutan", "Bolivia",
                                    "Bosnia and Herzegovina", "Botswana", "Brazil",
                                    "British Virgin Islands", "Brunei", "Bulgaria", "Burkina Faso",
                                    "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada",
                                    "Central African Republic", "Chad", "Chile", "China", "Colombia",
                                    "Comoros", "Cook Islands", "Costa Rica", "Croatia", "Cuba",
                                    "Curacao", "Cyprus", "Czechia",
                                    "Democratic Republic of the Congo", "Denmark", "Djibouti",
                                    "Dominica", "Dominican Republic", "East Timor", "Ecuador",
                                    "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea",
                                    "Estonia", "Eswatini", "Ethiopia", "Faroe Islands",
                                    "Federated States of Micronesia", "Fiji", "Finland", "France",
                                    "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany",
                                    "Ghana", "Greece", "Greenland", "Grenada", "Guatemala",
                                    "Guinea", "Guinea Bissau", "Guyana", "Haiti", "Honduras",
                                    "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran",
                                    "Iraq", "Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica",
                                    "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo",
                                    "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho",
                                    "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg",
                                    "Macao", "Macedonia", "Madagascar", "Malawi", "Malaysia",
                                    "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania",
                                    "Mauritius", "Mexico", "Moldova", "Mongolia", "Montenegro",
                                    "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia",
                                    "Nauru", "Nepal", "Netherlands", "New Caledonia", "New Zealand",
                                    "Nicaragua", "Niger", "Nigeria", "Niue", "North Korea", "Norway",
                                    "Oman", "Pakistan", "Palau", "Palestine", "Panama",
                                    "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland",
                                    "Portugal", "Puerto Rico", "Qatar", "Republic of Serbia",
                                    "Republic of the Congo", "Romania", "Russia", "Rwanda",
                                    "Saint Helena", "Saint Kitts and Nevis", "Saint Lucia",
                                    "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines",
                                    "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal",
                                    "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten",
                                    "Slovakia", "Slovenia", "Solomon Islands", "Somalia",
                                    "South Africa", "South Korea", "South Sudan", "Spain",
                                    "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland",
                                    "Syria", "Taiwan", "Tajikistan", "Thailand", "The Bahamas",
                                    "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey",
                                    "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", "Uganda",
                                    "Ukraine", "United Arab Emirates", "United Kingdom",
                                    "United Republic of Tanzania", "United States of America",
                                    "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam",
                                    "Wallis and Futuna", "Yemen", "Zambia", "Zimbabwe" )


          if( focalCountry %in% countryNamesBerkley == TRUE ){

              dataUrl <- paste("https://berkeleyearth.org/wp-content/themes/client-theme/temperature-data/",
                               gsub( pattern = " ", replacement = "-", x = focalCountry ),
                               "-projection.txt", sep="" )

              thisData <- read.table( dataUrl, skip=51 )

              names( thisData ) <- c("Year", "AnnualAverage", "AnnualAverageUncertainty", "10YearSmooth", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "ModelHistorical")

              countryNameWithoutBlankSpace <- gsub( pattern = " ", replacement = "", x = focalCountry )
              countryNameWithoutBlankSpaceOrDashes <- gsub( pattern = "-", replacement = "", x = countryNameWithoutBlankSpace )

              countryDataName <- paste( countryNameWithoutBlankSpaceOrDashes,
                                        "Data", sep="")

              assign( countryDataName, thisData  )

              return( eval(parse(text = countryDataName)) )

          }else{

              stop( "country name not in list... please check the spelling :) [no dashes!] Please check the country list." )

          }

}

