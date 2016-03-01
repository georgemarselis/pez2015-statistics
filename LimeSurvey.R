#!/usr/bin/evn R

######### libraries ##########
Sys.setenv(JAVA_HOME = '/Library/Java//Home')
# Java 1.6 will set up the 'Home' link under /Library/Java
Sys.setenv(LD_LIBRARY_PATH = '$LD_LIBRARY_PATH:$JAVA_HOME/lib')

#set everything to utf-8
options(encoding = "utf-8")

repos = 'https://cran.stat.unipd.it/'
options( repos = structure( c( CRAN = repos ) ) )


if ( !require( "ggplot2") ||
     !require( "rpart" ) ||
     !require( "rpart.plot" ) ||
     !require( "forecast" ) ||
     !require( "xlsx" ) ||
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
#    excelfile <- "LimeSurvey.xlsx"

    limesurveyresults <- read.table( file <- inputfile, sep = "$", header <- TRUE )
    write.table(limesurveyresults , file <- tempfile, sep = "$", fileEncoding = "UTF-8" )
    limesurveyresults <- read.table( file <- tempfile, sep = "$", header <- TRUE )
    unlink( tempfile )
#    write.xlsx( limesurveyresults, excelfile )
    return( limesurveyresults )
}

limesurveyresults <- readfile( )
attach( limesurveyresults )
columnnames <- colnames( limesurveyresults )


quantile( limesurveyresults$age, probs = c( 0, 0.25, 0.50, 0.75, 1) )
boxplot( limesurveyresults$age, main = "boxplot", ylab = "age", ylim = c( 1, 11), las = 1 )


# The default is c(3, 1, 0).
axis.title.position <- 3
axis.label.position <- 1
axis.line.position <- 0

# default is c(5, 4, 4, 2) + 0.1.
bottom <- 5
left   <- 6   # give more margin to the left, so the axis label can fit
top    <- 4
right  <- 2

par( mgp = c( axis.title.position, axis.label.position, axis.line.position ) ,
     mar = (c( bottom, left, top, right ) + 0.1 ) )

#boxplot( limesurveyresults$age, data = limesurveyresults,  main = "boxplot", ylab = "age", ylim = c( 1, 11 ), yaxt = "n" )
boxplot( limesurveyresults$age, data = limesurveyresults, axes = FALSE, ann = FALSE )


ageGroupsTextLabels <- c( "15-19", "15-19", "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
BOTTOM_SIDE <- 1
LEFT_SIDE <- 2
TICK_MARKS <- c( 1:12 )

# axis( BOTTOM_SIDE, labels = FALSE, lwd.ticks = 0 ) # we don't care to draw an x-axis
axis( LEFT_SIDE, at = TICK_MARKS, labels = ageGroupsTextLabels, col.axis = "red", cex.axis = 1, las = TRUE )
title( ylab = "age", cex.lab = 2.0, line = 4.5)
box()
# lookup:
#   scatterplot matrix
#   how to create a scatterplot

# ideally the first thing in a project we want is scatterplot to
# get a feel of things that can be correlated.
#   that has to go through a scatterplot matrix?

#plotgraph( limesurveyresults, aes( limesurveyresults$age, limesurveyresults$education ), "age", "education" )

