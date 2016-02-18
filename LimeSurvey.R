######### libraries ##########
Sys.setenv(JAVA_HOME = '/Library/Java//Home')
# Java 1.6 will set up the 'Home' link under /Library/Java
Sys.setenv(LD_LIBRARY_PATH = '$LD_LIBRARY_PATH:$JAVA_HOME/lib')

repos = 'https://cran.stat.unipd.it/'
options( repos = structure( c( CRAN = repos ) ) )


if ( !require( "ggplot2") ||
     !require( "rpart" ) ||
     !require( "rpart.plot" ) ||
     !require( "forecast" ) ||
     !require( "xlsx" )
     !require( "stringi" )
) {
    install.packages( "ggplot2" )
    install.packages( "rpart" )
    install.packages( "rpart.plot" )
    install.packages( "forecast" )
    install.packages( "xlsx")
}

library( ggplot2 )
library( rpart )
library( rpart.plot )
library( forecast )
library( xlsx )
library( stringi )


0 -> DEBUG
options( warn = 1 )
##############################

plotgraph <- function(data, kot, xaxis_label_label, y_axis_label)
{
    ggplot( data, kot ) +
        geom_point( shape = 1 ) +
        geom_smooth( ) +
        theme(
            axis.title.x = element_text( face = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16 ),
            axis.title.y = element_text( face = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
        scale_x_continuous( name = xaxis_label_label ) +
        scale_y_continuous( name = y_axis_label ) +
        theme( legend.position = "none" ) +
        geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )

}

#################### convert the table to UTF-8

readfile <- function ( ) {
    #filename vars
    inputfile <- "./ergasia.csv"
    tempfile  <- "./kot.csv"
    excelfile <- "LimeSurvey.xlsx"

    limesurveyresults <- read.table( file <- inputfile, sep = "$", header <- TRUE )#, encoding = "UTF-8" )
    write.table(limesurveyresults , file <- tempfile, sep = "$", fileEncoding = "UTF-8" )
    limesurveyresults <- read.table( file <- tempfile, sep = "$", header <- TRUE )
    unlink( tempfile )
    write.xlsx( limesurveyresults, excelfile )
    return( limesurveyresults )
}

limesurveyresults <- readfile( )
columnnames <- colnames( limesurveyresults )

# lookup:
#   scatterplot matrix
#   how to create a scatterplot

# ideally the first thing in a project we want is scatterplot to
# get a feel of things that can be correlated.
#   that has to go through a scatterplot matrix?

plotgraph( limesurveyresults, aes( limesurveyresults$age, limesurveyresults$education ), "age", "education" )

