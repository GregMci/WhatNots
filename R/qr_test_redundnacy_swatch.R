


#qr_test_redundnacy_swatch <-  function( qr_data = NULL ){}

#if( is.null( qr_data) == T ) qr_data <- qr_matrix_2_dataframe( qr_code("https://warwick.ac.uk/fac/cross_fac/cim/apply-to-study/masters-programmes/visualisation/") )

par( mfrow = c( 4, 4 ) )
par( mar = c( rep( 1, 4 ) ))


qr_data <-  qr_matrix_2_dataframe( qrData )

mask_circle <- TRUE

for( mask_size in 1:16 ){


qr_plot( qr_data )

#lines( plot_region_rect( ), col="slateblue" )

#rect( min( qr_data ), min( qr_data ), max( qr_data ), max( qr_data ), border="hotpink" )

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


