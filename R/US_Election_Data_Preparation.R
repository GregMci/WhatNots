###___ preparing US Election Data objects for year by state

    # script produces an object for each election year, divided by state

    # voting data is...
        # non spatial
        # with one candidate per row
        # including all candidates

    # output shape file has...
    # each row as a state,
    # with columns of...
        # fips code (unique # identifier)
        # state name
        # two letter state name
    # and for every fourth year...
        # starting 1976,
        # ending 2020,
    # columns for each of...
        # vote numbers
        # and proportion of vote
    # by main voting party
        # Republican
        # Democrat
        # or Other
    # sf spatial 'geometry' (Type: MULTIPOLYGON) in last column


###___ data References

    #_ SPATIAL DATA
    # Walker K (2023). _tigris: Load Census TIGER/Line Shapefiles_.
    # R package, version 2.0.4, <https://CRAN.R-project.org/package=tigris>.
    # tigris package sources US states shape files from  United States Census Bureau
    # TIGER system = "Topologically Integrated Geographic Encoding and Referencing"
    # https://rdrr.io/cran/tigris/man/states.html
    # https://journal.r-project.org/archive/2016-2/walker.pdf
    # https://github.com/walkerke/tigris
    # uses data from https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
    # for background see - https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html
    # & https://walker-data.com/census-r/mapping-census-data-with-r.html
    # data codes here:
    # https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2017/TGRSHP2017_TechDoc_Ch3.pdf

    #_ VOTING DATA
    # MIT Election Data and Science Lab
    # "U.S. President 1976–2020",
    # https://doi.org/10.7910/DVN/42MVDX,
    # Harvard Dataverse, V7, UNF:6:MkQHX147hJCgscG5IqK77g== [fileUNF]
    # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX


###___ housekeeping - libraries and data

    if( exists( "housekeeping" ) == F ){

        # create variable so the housekeeping only runs once
        housekeeping <- 023

        # load packages
        library( tigris )
        library( sf )

        # CHANGE DIRECTORY PATH TO INTENDED FOLDERS

        # folder data is read from
        my_read_folder <- "C:\\Users\\u1473392\\Documents\\Warwick - CIM\\Anti-Tufte\\US_election"

        # folder data is written to
        my_write_folder <- "C:\\Users\\u1473392\\Documents\\Warwick - CIM\\Anti-Tufte\\WhatNots_test_folder"

        # read in all election data ( by state and every 4 years from 1976 )
        setwd( my_read_folder )
        pres_data <- read.csv( "1976-2020-president.csv")


        # read shape files if saved or create from the 'tigris' package

        # states shape files
        if ( file.exists( "the_states.shp" ) ) {
            the_states <- st_read( "the_states.shp" )
            print("file exists... read data in locally...")
        }else{
            the_states <- states(cb = FALSE, resolution = "500k", year = NULL )
            st_write( obj = the_states,
                      dsn = "the_states.shp" )
            print("downloaded data and created file in folder ...")
        }

        # counties shape files
        if ( file.exists( "the_counties.shp" ) ) {
            the_counties <- st_read( "the_counties.shp" )
            print("file exists... read data in locally...")
        }else{
            the_counties <- counties(cb = FALSE, resolution = "500k", year = NULL )
            st_write( obj = the_counties,
                      dsn = "the_counties.shp" )
            print("downloaded data and created file in folder ...")
        }

    }


###___ remove unused columns & rename columns for ease of merging with shape files

    # select state codes, name and geometry
    elect_states <- the_states[ c("STATEFP", "NAME", "geometry") ]

    # change names to match those of votes data
    names( elect_states )[1:2] <- c( "fips", "state" )

    # ensure fips code is numerical (rather than a character)
    elect_states$fips <- as.numeric( elect_states$fips)

    # trim votes data to relevant columns
    pres <- pres_data[  , c("year", "state_po", "state_fips", "candidatevotes", "totalvotes", "party_simplified", "candidate" ) ]

    # change names to shorter, more familiar names
    names( pres ) <- c("year", "state_po", "fips", "votes", "totalvotes", "party", "candidate")

    # remove DC (District of Columbia)
    pres <- pres[ pres$state_po!="DC", ]


###___ create subsets of vote data for each election year

    for( this_year in seq( 1976, 2020, by=4 ) ) {

        # SUBSET FOR YEAR
        this_data <- pres[ pres$year==this_year, ]

        # CALC PROPORTION OF VOTE PER CANIDATE (i.e. each row)
        this_data$vote_proportion <- round( this_data$votes / this_data$totalvotes, digits = 5 )

        # subset Democrat and Republican intio separate objects
        # rename to include year
        this_DEMOCRAT <- this_data[ this_data$party=="DEMOCRAT" & this_data$candidate != "OTHER", ]
        names( this_DEMOCRAT ) <- c("year", "state_po", "fips",
                                    paste( "DEM_votes", this_year, sep="_" ),
                                    paste("totalvotes", this_year, sep="_"),
                                    "party",
                                    "candidate",
                                    paste( "DEM_prop", this_year, sep="_" ))

        this_REPUBLICAN <- this_data[ this_data$party=="REPUBLICAN" & this_data$candidate != "OTHER", ]
        names( this_REPUBLICAN ) <- c("year", "state_po", "fips",
                                    paste( "REP_votes", this_year, sep="_" ),
                                    paste("totalvotes", this_year, sep="_"),
                                    "party",
                                    "candidate",
                                    paste( "REP_prop", this_year, sep="_" ))

        # Note...
        # where multiple votes were counted for the same candidate
        # (e.g. MD 2016 and AZ 2016)
        # they have been 'manually' added together (i.e. in excel)
        # where candidate name was blank it was 'manually' replaced with "OTHER"

        # calculate vote remainder for "OTHER" (which includes "LIBERTARIAN")
        # and append to 'this_REPUBLICAN'
        this_REPUBLICAN$OTH_votes <- this_REPUBLICAN$totalvotes -
                                      ( this_DEMOCRAT$DEM_votes +
                                          this_REPUBLICAN$REP_votes )
        this_REPUBLICAN$OTH_prop <- 1 - ( this_DEMOCRAT$DEM_prop +
                                            this_REPUBLICAN$REP_prop )

        # rename Other voting with year suffix
        names( this_REPUBLICAN )[9] <- paste( "OTH_votes", this_year, sep="_")
        names( this_REPUBLICAN )[10] <- paste( "OTH_prop", this_year, sep="_")

        # first merge of spatial and votes (i.e. year 1976) includes all columns
        # afterwards... just 'fips' (for matching) and voting data
        if( this_year == 1976 ){
          this_DEMOCRAT <- this_DEMOCRAT[  c( 2:5, 8 ) ]
        }else{
          this_DEMOCRAT <- this_DEMOCRAT[  c( 3:5, 8 ) ]
        }
        this_REPUBLICAN <- this_REPUBLICAN[  c( 3:4, 8:10 ) ]

        # merge into 'elect_states' data frame(sf)
        elect_states <- merge( x = elect_states,
                               y =  this_DEMOCRAT,
                               by.x = "fips",
                               by.y="fips" )

        elect_states <- merge( x = elect_states,
                               y =  this_REPUBLICAN,
                               by.x = "fips",
                               by.y = "fips" )

    }


    # use a geographic centre of whole US (i.e. including Hawaii and Alaska)
    this_projection <- "+proj=laea +x_0=0 +y_0=0 +lon_0=-103.46 +lat_0=44.58"

    # project spatial data to this_projection
    elect_states <- st_transform( x = elect_states,
                                  crs = this_projection )

    elect_states$long <- NULL
    elect_states$lat <- NULL

    # get centroid of states for labels
    for( st in 1:dim( elect_states )[1] ){
      elect_states$long[st] <- st_centroid( elect_states$geometry[st] )[[1]][1]
      elect_states$lat[st] <- st_centroid( elect_states$geometry[st] )[[1]][2]
    }


    # save the data
    setwd( my_write_folder )
    st_write( obj = elect_states,
              dsn = "elect_states.shp",
              append = F )

    # saving column names for checks
    write.table( x = names(elect_states),
                 file = "names_elect_states.csv",
                 sep = ",",
                 row.names = F,
                 col.names = F)




###___ as above, but simplified, non spatial file for 2020 only

    for( this_year in seq( 2020, 2020, by = 4 ) ) {

      # SUBSET FOR YEAR
      this_data <- pres[ pres$year==this_year, ]
      # CALC PROPORTION OF VOTE PER CANIDATE
      this_data$vote_proportion <- round( this_data$votes / this_data$totalvotes, digits = 5 )

      # subset democrat and republican
      # rename to include year
      this_DEMOCRAT <- this_data[ this_data$party=="DEMOCRAT" & this_data$candidate != "OTHER", ]
      names( this_DEMOCRAT ) <- c("year", "state_po", "fips",
                                  paste( "DEM_votes", this_year, sep="_" ),
                                  paste("totalvotes", this_year, sep="_"),
                                  "party",
                                  "candidate",
                                  paste( "DEM_prop", this_year, sep="_" ))

      this_REPUBLICAN <- this_data[ this_data$party=="REPUBLICAN" & this_data$candidate != "OTHER", ]
      names( this_REPUBLICAN ) <- c("year", "state_po", "fips",
                                    paste( "REP_votes", this_year, sep="_" ),
                                    paste("totalvotes", this_year, sep="_"),
                                    "party",
                                    "candidate",
                                    paste( "REP_prop", this_year, sep="_" ))

      # calc remainder of votes for "OTHER" (which includes "LIBERTARIAN")
      # and append to 'this_REPUBLICAN'
      this_REPUBLICAN$OTH_votes <- this_REPUBLICAN$totalvotes -  ( this_DEMOCRAT$DEM_votes + this_REPUBLICAN$REP_votes )
      this_REPUBLICAN$OTH_prop <- 1 -  round( ( this_DEMOCRAT$DEM_prop + this_REPUBLICAN$REP_prop ), digits=5 )

      # rename with year suffix
      names( this_REPUBLICAN )[9] <- paste( "OTH_votes", this_year, sep="_")
      names( this_REPUBLICAN )[10] <- paste( "OTH_prop", this_year, sep="_")


    }



    POTUS_votes_2020 <- as.data.frame( cbind( this_DEMOCRAT$year, this_DEMOCRAT$state_po, this_DEMOCRAT$fips,
                                              this_DEMOCRAT$DEM_votes_2020,
                                              this_REPUBLICAN$REP_votes_2020,
                                              this_REPUBLICAN$OTH_votes_2020,
                                              this_REPUBLICAN$totalvotes_2020,
                                              this_DEMOCRAT$DEM_prop_2020,
                                              this_REPUBLICAN$REP_prop_2020,
                                              this_REPUBLICAN$OTH_prop_2020 ) )

    names( POTUS_votes_2020 ) <- c( "year", "state_po", "fips",
                                    "DEM_votes",
                                    "REP_votes",
                                    "OTH_votes",
                                    "totalvotes",
                                    "DEM_prop",
                                    "REP_prop",
                                    "OTH_prop"  )


    # save the data
    setwd( my_write_folder )
    write.csv( x = POTUS_votes_2020,
               file = "POTUS_votes_2020.csv",
               row.names = F )


###___ read in shape file for checks

    setwd( "C:\\Users\\u1473392\\Documents\\Warwick - CIM\\Anti-Tufte\\WhatNots_test_folder" )

    TEST_votes_ALL <- st_read( "elect_states.shp")

    str( TEST_votes_ALL )

