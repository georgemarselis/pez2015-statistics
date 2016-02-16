#!/usr/bin/env R

######### libraries ##########
########## run once ##########
repos = 'https://cran.stat.unipd.it/'
options( repos = structure( c( CRAN = repos ) ) )
#pdf.options( encoding = 'utf-8' )


if ( !require( "ggplot2") ||
     !require( "rpart" ) ||
     !require( "rpart.plot" ) ||
     !require( "forecast" )
    ) {
    install.packages( "ggplot2" )
    install.packages( "rpart" )
    install.packages( "rpart.plot" )
    install.packages( "forecast" )


}

0 -> DEBUG
options( warn = 1 )
##############################

##############################
# read file
weather = read.table( "weather.all.2008.csv", header = T, sep = "," )
# name the columns from imported weather.all.2008.csv file
names( weather )
w.month = as.numeric( format( as.Date( weather$dt ), "%m" ) )
w.day   = as.numeric( format( as.Date( weather$dt ), "%d" ) )
w.date  = as.POSIXlt( strptime( weather$dt, "%Y-%m-%d %H:%M" ) )
w.hour  = w.date$hour

weather = data.frame(weather, month = w.month, day = w.day, hour = w.hour)


##############################
# name the columns from imported weather.all.2008.csv file
names <- function(weather) { list( "id", "temp", "bar", "hum", "sol", "w.d", "w.s", "dt", "month",
          "day", "hour" )
}


#### graphs for all variables vs temp

##############################
# load library ggplot2
library(ggplot2)


plotgraph <- function(data = weather, kot = aes( x = bar, y = temp ),
                      xaxis_label_label = "atmospheric pressure (bar)" ,
                      y_axis_label = "temperature (C)" )
{
    ggplot( data, eval( kot ) ) +
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

cairo_pdf( './bar-vs-temp-1.pdf', family = "DejaVu Sans" )
plotgraph( data = weather, kot = aes( x = bar, y = temp ), xaxis_label_label = "atmospheric pressure (bar)", y_axis_label = "temperature (C)" )

##############################

#weather_result <- weather$bar < 950
sum( weather$bar < 950 )
sel = which( weather$bar < 950 )

##############################
cairo_pdf( './bar-vs-temp-2.pdf', family = "DejaVu Sans" )
plotgraph( data = weather[ -sel, ], kot = aes( x = bar, y = temp ), xaxis_label_label = "atmospheric pressure (bar)", y_axis_label = "temperature (C)" )

##############################
cairo_pdf( './humidity-vs-temp-1.pdf', family = "DejaVu Sans" )
plotgraph( data = weather, kot = aes( x = hum, y = temp ), xaxis_label_label = "Υγρασία (hum)", y_axis_label = "Θερμοκρασία (temp)" )

##############################
cairo_pdf( './solrad-vs-temp-1.pdf', family = "DejaVu Sans" )
plotgraph( data = weather, kot = aes( x = sol, y = temp ), xaxis_label_label = "Ακτινοβολία (sol)", y_axis_label = "Θερμοκρασία (temp)" )

#############################
cairo_pdf( './solrad-vs-temp-2-colorhour.pdf', family = "DejaVu Sans" )
plotgraph( data = weather, kot = aes( x = sol, y = temp, color = hour ), xaxis_label_label = "Ακτινοβολία (sol) ", y_axis_label = "Θερμοκρασία (temp)" )

##############################
cairo_pdf( './solrad-vs-temp-3-colormonth.pdf', family = "DejaVu Sans" )
plotgraph( data = weather, kot = aes( x = sol, y = temp, color = month ), xaxis_label_label = "Ακτινοβολία (sol)", y_axis_label = "Θερμοκρασία (temp)" )

##############################
cairo_pdf( './windir-vs-temp-1.pdf', family = "DejaVu Sans" )
plotgraph( data = weather, aes( x = w.d, y = temp ), xaxis_label_label = "Κατεύθυνση ανέμου (w.d)", y_axis_label = "Θερμοκρασία (temp)" )

##############################
sel1 = which( weather$w.d > 360 )
cairo_pdf( './windir-vs-temp-2.pdf', family = "DejaVu Sans" )
plotgraph( data = weather[ -sel1, ], aes( x = w.d, y = temp ), xaxis_label_label = "Κατεύθυνση ανέμου (w.d)", y_axis_label = "Θερμοκρασία (temp)" )

##############################
cairo_pdf( './windspeed-vs-temp-1.pdf', family = "DejaVu Sans" )
plotgraph( data = weather, aes( x = w.s, y = temp ), xaxis_label_label = "Ταχύτητα ανέμου (w.s)", y_axis_label = "Θερμοκρασία (temp)")

##############################
#cairo_pdf( './time-vs-temp-1.pdf', family = "DejaVu Sans" )
#plotgraph( data = weather, aes( x = dt, y = temp ), xaxis_label_label = "Time (dt)", y_axis_label = "Temperature (C)" )

##############################
#cairo_pdf( './time-vs-temp-2.pdf', family = "DejaVu Sans" )
#plotgraph( data = weather, aes( x = as.Date( dt, "%Y-%m-%d" ), y = temp ), xaxis_label_label = "Χρόνος (dt)", y_axis_label = "Θερμοκρασία (temp)" )

##############################

### clean data ###
toremove  = union( sel, sel1 )
weather.f = weather[ -toremove , ]
##################

##### create pseudovariables
cairo_pdf( './hour-vs-solrad-2.pdf', family = "DejaVu Sans" )
plotgraph( data = weather.f, aes( x = hour, y = sol ), xaxis_label_label  = "Ώρα (hour)", y_axis_label = "Ακτινοβολία (sol)" )

##############################
hour.f = rep( "day", length( weather.f$hour ) )
hour_f_results <- hour.f
if ( 1 == DEBUG ) {
    hour_f_results
}
timeslices <- c( 0:4, 20:23 )
for (i in timeslices) {
    hour.f[ weather.f$hour == i  ] = c( "night" )
}

table( hour.f )

cairo_pdf( './solrad-vs-temp-2.pdf', family = "DejaVu Sans" )
plotgraph( data = weather.f, aes( x = sol, y = temp, color = hour.f ), xaxis_label_label = "Ακτινοβολία (sol) ", y_axis_label = "Θερμοκρασία (temp)" )

##############################
weather.f = data.frame( weather.f, hour.f = hour.f )
season = rep( "winter", length( weather.f$month ) )

for (i in 4:6 ) {
    season[ which( weather.f$month == i ) ] = c( "spring" )
}
for (i in 7:9 ) {
    season[ which( weather.f$month == i ) ] = c("summer")
}
for (i in 10:12 ) {
    season[ which( weather.f$month == i ) ] = c("fall")
}
season[ which( weather.f$month == 3 & weather.f$day > 21 ) ] = c( "spring" )
season[ which( weather.f$month == 6 & weather.f$day > 21 ) ] = c( "summer" )
season[ which( weather.f$month == 9 & weather.f$day > 23 ) ] = c( "fall" )
season[ which( weather.f$month == 12 & weather.f$day > 23 ) ] = c( "winter" )
weather.f = data.frame( weather.f, season )

##############################

mesi = rep( "π.μ.", length( weather.f$month ) )
mesi[ weather.f$hour >= 12 ] = c( "μ.μ." )
weather.f = data.frame( weather.f, mesi )

##############################

cairo_pdf( './solrad-vs-temp-3.pdf', family = "DejaVu Sans" )
plotgraph( data = weather.f[ hour.f == "day" , ], aes( x = sol, y = temp, color = season ), xaxis_label_label = "Ακτινοβολία (sol)", y_axis_label = "Θερμοκρασία (temp)")

### reduce variables
names <- function(weather.f) {  list( "id", "temp", "bar", "hum", "sol", "w.d", "w.s", "dt",
          "month", "day", "hour", "hour.f", "season", "mesi" )
}

weather.f = weather.f[ , c( 2:7, 9, 11:14 ) ]

names <- function(weather.f) { list( "temp", "bar", "hum", "sol", "w.d", "w.s", "month",
          "hour", "hour.f", "season", "mesi" )
}

### find maximal model ###

library( rpart )
library( rpart.plot )

fit = rpart(temp ~., method = "anova", data = weather.f )
prp( fit )
summary( fit )
prop.table( fit$variable.importance )
round( 100 * prop.table( fit$variable.importance ), 2 )

### multiple regression ###
fit.lm = lm( temp ~. + season*bar + season*I( hum ^ 2 ) , data = weather.f )
summary( fit.lm )

### use AIC to simplify model ###
fit.lm = step( fit.lm, direction = "both" )
summary( fit.lm )

### BEST FIT - clean data ###

library( forecast )

to.clean.temp = auto.arima( weather.f$temp )
temp.c = to.clean.temp$residuals

weather.c = weather.f
weather.c$temp = auto.arima( weather.f$temp )$residuals
weather.c$bar  = auto.arima( weather.f$bar  )$residuals
weather.c$hum  = auto.arima( weather.f$hum  )$residuals
weather.c$sol  = auto.arima( weather.f$sol  )$residuals
weather.c$w.d  = auto.arima( weather.f$w.d  )$residuals
weather.c$w.s  = auto.arima( weather.f$w.s  )$residuals

n.fit = rpart( temp ~. , method = "anova", data = weather.c )
prp( n.fit )
summary( n.fit )
prop.table( n.fit$variable.importance )
round( 100 * prop.table( n.fit$variable.importance ), 2 )

n.fit.lm = lm( temp ~ . + I( hum ^ 3 ) , data = weather.c )
summary( n.fit.lm )
n.fit.lm = step( n.fit.lm, direction = "both" )
summary( n.fit.lm )

