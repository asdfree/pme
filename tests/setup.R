if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

pme_cat <-
	get_catalog( "pme" ,
		output_dir = file.path( getwd() ) )

# sample 75% of the records
which_records <- sample( seq( nrow( pme_cat ) ) , round( nrow( pme_cat ) * 0.75 ) )

# always sample year == 2016
pme_cat <- unique( pme_cat[ which_records , ] , subset( pme_cat , year == 2016 ) )

lodown( "pme" , pme_cat )
