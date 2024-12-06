#' Title
#'
#' @param qr_df qr data data frame produced by qr_matrix_2_dataframe()
#' @param rounding amount symbol is like circle, 1= circle & 0 = square
#'
#' @return
#' @export
#'
#' @examples
#' library( qrcode )
#' qrData <- qr_code("https://warwick.ac.uk/fac/cross_fac/cim/apply-to-study/masters-programmes/visualisation/")
#' qr_df <- qr_matrix_2_dataframe( qrData )
#' library( remotes )
#' remotes::install_github("GregMci/baselines", force = T)
#' library( baselines ) # uses the rect_rounded() function
#' par(mfrow= c( 2, 2 ) )
#' par( mar=rep( 0, 4 ) )
#' qr_plot( qr_df )
#' qr_plot( qr_df, rounding = 0.5 )
#' qr_plot( qr_df, rounding = 1 )
#' qr_plot( qr_df, rounding = seq( 1, 0, l=5) )

qr_plot <- function( qr_df = NULL, mat_2_df =T, rounding = 0, col = "black", bg="white" ){

    #if( is.null( qr_df) == T ) qr_df <- qr_matrix_2_dataframe()
    if( is.null( qr_df) == T ) qr_df <- WhatNots::CIM
  
    if( is.data.frame( qr_df ) == F) stop("use qr_matrix_2_dataframe() to convert qrcode matrix to data frame")

    # check rounding is 1 value or a list of five ( main and 4 guides NW, NE, SW, SE )
    if( length( rounding ) == 5 ){
      this_rounding <- rounding
    } else if( length( rounding ) ==1 ){
      this_rounding <- rep( rounding, 5 )
    } else {
      this_rounding <- rep( rounding[1], 5 )
      warning( "rounding expects 1 or 5 values, 1st value repeated " )
    }

    buff <- 0

    plot_blank( x = 1:max(qr_df$x), y= 1:max(qr_df$y),
                xlim=c( 1, max(qr_df$x)+1 ), 
                ylim=c( max(qr_df$y)+1 , 1 ),
                asp=1 )

    # plot_background( col = bg )

    # plot all cells except guides
    for( j in which( qr_df$z == 1 ) ){
      polygon( rect_rounded( qr_df$x[ j ], qr_df$y[ j ],
                             1+qr_df$x[ j ], 1+qr_df$y[ j ],
                             rounding = this_rounding[1] ),
               col=col, border=NA  )
    }

    # add position guides as 3 overlaying shapes ( black, white, black )
    guideStart <- max( qr_df ) - 6
    for( i in seq(3.5, 1.5, by = -1) ){
      
      # top left
      polygon( 7.5 + i*rect_rounded( n_points = 360, rounding = this_rounding[2] )$x,
               7.5 + i*rect_rounded( n_points = 360, rounding = this_rounding[2] )$y,
               col=c( col, bg, col )[i-0.5], border=F  )

      # bottom left
      polygon( guideStart+0.5 + i*rect_rounded( n_points = 360, rounding = this_rounding[3] )$x,
               7.5 + i*rect_rounded( n_points = 360, rounding = this_rounding[3] )$y,
               col=c( col, bg, col )[i-0.5], border=F  )

      # top right
      polygon( 7.5 + i*rect_rounded( n_points = 360, rounding = this_rounding[4] )$x,
               guideStart+0.5 + i*rect_rounded( n_points = 360, rounding = this_rounding[4] )$y,
               col=c( col, bg, col )[i-0.5], border=F  )
    }


    # add alignment guides as circles
    guideStart <- max( qr_df ) - 8
    for( i in seq( 2.5, 0.5, by = -1 ) ){
      polygon( guideStart-0.5 + i*rect_rounded( n_points = 360, rounding = this_rounding[5] )$x,
               guideStart-0.5 + i*rect_rounded( n_points = 360, rounding = this_rounding[5] )$y,
               col=c( col, bg, col )[i+0.5], border=F  )
               
    }

}

