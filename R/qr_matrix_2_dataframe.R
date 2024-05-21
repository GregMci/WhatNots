#' Convert matrix output of qr_code to data frame
#'
#' @param qr_data a matrix outputted by qrcode::qr_code
#'
#' @return
#' @export
#'
#' @examples
#'
#' library( qrcode )
#' qrData <- qr_code("https://warwick.ac.uk/fac/cross_fac/cim/apply-to-study/masters-programmes/visualisation/")
#' qr_df <- qr_matrix_2_dataframe( qrData )

qr_matrix_2_dataframe <- function( qr_data = NULL ){

    if( is.null( qr_data) == T ) qr_data <- qr_code("https://warwick.ac.uk/fac/cross_fac/cim/apply-to-study/masters-programmes/visualisation/")

    # blank out position guide squares
    qr_data[ 4:10, 4:10 ] <- "BLANK"
    
    rightPos1 <- dim( qr_data )[2] - 10
    rightPos2 <- dim( qr_data )[2] - 3
    
    qr_data[ rightPos1:rightPos2, 4:10 ] <- "BLANK"
    qr_data[ 4:10, rightPos1:rightPos2 ] <- "BLANK"

    # blank out alignment guide squares
    qr_data[ (rightPos1-1):(rightPos1+3), (rightPos1-1):(rightPos1+3) ] <- "BLANK"

    # create object of entries by row for all  cells
    xyVals <- expand.grid( 1:dim( qr_data )[1], 1:dim( qr_data )[2] )
    # create variable for 3rd value... 1 = Fill/TRUE, 0 = Empty/FALSE, 2 = BLANK
    zVals <- rep( -99, dim(xyVals)[1] )

    # loop through all cells
    for( i in 1:dim( xyVals )[1] ){
      # if filled
      if( qr_data[ xyVals[ i, 1 ], xyVals[ i, 2 ] ] == TRUE ){
        zVals[i] <- 1
      } else {
        # if empty
        if( qr_data[ xyVals[ i, 1 ], xyVals[ i, 2 ] ] == FALSE ){
          zVals[i] <- 0
        } else{
          # if blank
          if( qr_data[ xyVals[ i, 1 ], xyVals[ i, 2 ] ] == "BLANK" ){
            zVals[i] <- 2
          }
        }
      }
    }

    qr_dataFrame <- as.data.frame( cbind( xyVals, zVals ) )
    names( qr_dataFrame) <- c( "y", "x", "z" )
    qr_dataFrame <- qr_dataFrame[ , c( 2, 1, 3) ] 

    return( qr_dataFrame )

}



