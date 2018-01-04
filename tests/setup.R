if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

pme_cat <-
	get_catalog( "pme" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( pme_cat ) ) / ceiling( nrow( pme_cat ) / 25 ) )

pme_cat <- unique( rbind( pme_cat[ record_categories == this_sample_break , ] , pme_cat[ pme_cat$year == 2016 , ] ) )

lodown( "pme" , pme_cat )
