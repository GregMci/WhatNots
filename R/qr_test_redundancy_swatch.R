
#' create swatch of qr codes with cricle or rect patch to test redundant area
#'
#' @param qr_df a data frame of the qr code
#' @param mask_circle if TRUE a circle if FALSE a rect
#'
#' @return
#' @export
#'
#' @examples
#' library( qrcode )
#' qr_test_redundancy_swatch()

qr_test_redundancy_swatch <-  function( qr_df = NULL, mask_circle = TRUE ){

    if( is.null( qr_df) == T ) qr_df <- qr_matrix_2_dataframe()

    if( is.data.frame( qr_df ) == F) stop("use qr_matrix_2_dataframe() to convert qrcode matrix to data frame")

    par( mfrow = c( 4, 4 ) )
    par( mar = c( rep( 1, 4 ) ))

    for( mask_size in 1:16 ){

        qr_plot( qr_df )

        mask_1 <- get_plot_centre()+1

        if( mask_circle == T){

          polygon( mask_1[1] + mask_size*arc_points()$x,
                mask_1[2] + mask_size*arc_points()$y,
                col="white", border=F )

        }else{

              rect( -1*mask_size + mask_1[1], -1*mask_size + mask_1[2],
                    mask_size + mask_1[1], mask_size + mask_1[2], col="white", border=F )

        }
    }

}
