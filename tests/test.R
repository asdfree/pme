if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available PME microdata files
pme_cat <-
	get_catalog( "pme" ,
		output_dir = file.path( getwd() ) )

# 2016 only
pme_cat <- subset( pme_cat , year == 2016 )
# download the microdata to your local computer
stopifnot( nrow( pme_cat ) > 0 )

options( survey.lonely.psu = "adjust" )

library(survey)

pme_df <- readRDS( file.path( getwd() , "pme 2016 01.rds" ) )

# throw out records missing their cluster variable
pme_df <- subset( pme_df , !is.na( v113 ) )

pop_totals <- unique( pme_df[ , c( 'v035' , 'v114' ) ] )

prestratified_design <- 
	svydesign( 
		~ v113 , 
		strata = ~ v112 , 
		data = pme_df ,
		weights = ~ v211 , 
		nest = TRUE
	)

pme_design <- 
	postStratify( prestratified_design , ~ v035 , pop_totals )
pme_design <- 
	update( 
		pme_design , 

		one = 1 ,
		
		# calculate whether each person is at least ten years of age
		pia = as.numeric( v234 >= 10 ) ,

		# determine individuals who are employed
		ocup_c = as.numeric( v401 == 1 | v402 == 1 | v403 == 1 ) ,
		
		sexo = factor( v203 , labels = c( "male" , "female" ) ) ,
		
		region = 
			factor( 
				v035 , 
				levels = c( 26 , 29 , 31 , 33 , 35 , 43 ) , 
				labels = c( "Recife" , "Salvador" , "Belo Horizonte" , 
					"Rio de Janeiro" , "Sao Paulo" , "Porto Alegre" )
			)
	)
	
pme_design <-
	update(
		pme_design ,
		
		# determine individuals who are unemployed
		desocup30 = as.numeric( ocup_c == 0 & !is.na( v461 ) & v465 == 1 )
	)
		
pme_design <-
	update(
		pme_design ,
		
		# determine individuals who are either working or not working
		pea_c = as.numeric( ocup_c == 1 | desocup30 == 1 )

	)
sum( weights( pme_design , "sampling" ) != 0 )

svyby( ~ one , ~ region , pme_design , unwtd.count )
svytotal( ~ one , pme_design )

svyby( ~ one , ~ region , pme_design , svytotal )
svymean( ~ vd25 , pme_design , na.rm = TRUE )

svyby( ~ vd25 , ~ region , pme_design , svymean , na.rm = TRUE )
svymean( ~ sexo , pme_design )

svyby( ~ sexo , ~ region , pme_design , svymean )
svytotal( ~ vd25 , pme_design , na.rm = TRUE )

svyby( ~ vd25 , ~ region , pme_design , svytotal , na.rm = TRUE )
svytotal( ~ sexo , pme_design )

svyby( ~ sexo , ~ region , pme_design , svytotal )
svyquantile( ~ vd25 , pme_design , 0.5 , na.rm = TRUE )

svyby( 
	~ vd25 , 
	~ region , 
	pme_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ desocup30 , 
	denominator = ~ pea_c , 
	pme_design ,
	na.rm = TRUE
)
sub_pme_design <- subset( pme_design , v234 %in% 13:19 )
svymean( ~ vd25 , sub_pme_design , na.rm = TRUE )
this_result <- svymean( ~ vd25 , pme_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ vd25 , 
		~ region , 
		pme_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pme_design )
svyvar( ~ vd25 , pme_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ vd25 , pme_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ vd25 , pme_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ ocup_c , pme_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( vd25 ~ ocup_c , pme_design )
svychisq( 
	~ ocup_c + sexo , 
	pme_design 
)
glm_result <- 
	svyglm( 
		vd25 ~ ocup_c + sexo , 
		pme_design 
	)

summary( glm_result )
library(srvyr)
pme_srvyr_design <- as_survey( pme_design )
pme_srvyr_design %>%
	summarize( mean = survey_mean( vd25 , na.rm = TRUE ) )

pme_srvyr_design %>%
	group_by( region ) %>%
	summarize( mean = survey_mean( vd25 , na.rm = TRUE ) )

