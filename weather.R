#!/usr/bin/env /opt/local/bin/R

######### libraries ##########
########## run once ##########
repos = 'https://cran.stat.unipd.it/'
options( repos = structure( c( CRAN = repos ) ) )

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

##############################

##############################
# red
weather = read.table("weather.all.2008.csv", header = T, sep = ",")
# name the columns from imported weather.all.2008.csv file
names( weather )
w.month = as.numeric( format( as.Date( weather$dt ), "%m" ) )
w.day   = as.numeric( format( as.Date( weather$dt ), "%d" ) )
w.date  = as.POSIXlt( strptime( weather$dt, "%Y-%m-%d %H:%M" ) )
w.hour  = w.date$hour

weather = data.frame(weather, month = w.month, day = w.day, hour = w.hour)


##############################
# name the columns from imported weather.all.2008.csv file
names <- function(weather) {
    c( "id", "temp", "bar", "hum", "sol", "w.d", "w.s", "dt", "month", "day", "hour")
}


#### graphs for all variables vs temp

##############################
# load library ggplot2
library(ggplot2)

ggplot( data = weather, aes( x = bar, y = temp ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face = "bold", colour = "#990000", size = 20),
            axis.text.x  = element_text( vjust = 0.5, size = 16),
            axis.title.y = element_text( face = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Ατμοσφαιρική Πίεση (bar) ") +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )

##############################

weather_result <- weather$bar < 950
sum( weather$bar < 950 )
sel = which( weather$bar < 950 )

##############################

ggplot( data = weather[ -sel, ], aes( x = bar, y = temp ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16),
            axis.title.y = element_text( face = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Ατμοσφαιρική Πίεση (bar) ") +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none" ) +
    geom_rug( col = rgb( 0.9 , 0, 0, alpha = .1), sides = "lrtb", size = 1)

##############################

ggplot( data = weather, aes( x = hum, y = temp ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16 ),
            axis.title.y = element_text( face  = "bold", colour = "#990000" , size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Υγρασία (hum)" ) +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none") +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1), sides = "lrtb", size = 1 )

##############################

ggplot( data = weather, aes( x = sol, y = temp ) ) +
    geom_point( shape = 1) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16 ),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
  scale_x_continuous( name = "Ακτινοβολία (sol) ") +
  scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
  theme( legend.position = "none" ) +
  geom_rug( col = rgb( 0.9, 0, 0, alpha = .1), sides = "lrtb", size = 1 )

##############################

ggplot( data = weather, aes( x = sol, y = temp, color = hour ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16 ),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text(angle  = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Ακτινοβολία (sol) ") +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none") +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )

##############################

ggplot( data = weather, aes( x = sol, y = temp, color = month ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Ακτινοβολία (sol)"   ) +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )

##############################

ggplot( data = weather, aes( x = w.d, y = temp ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16 ),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Κατεύθυνση ανέμου (w.d)" ) +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1), sides = "lrtb", size = 1 )

##############################

sel1 = which( weather$w.d > 360 )

ggplot( data = weather[ -sel1, ], aes( x = w.d, y = temp ) ) +
    geom_point( shape = 1) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16 ),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Κατεύθυνση ανέμου (w.d)" ) +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )

##############################

ggplot( data = weather, aes( x = w.s, y = temp ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
        axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
        axis.text.x  = element_text( vjust = 0.5, size = 16 ),
        axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20),
        axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 ) ) +
    scale_x_continuous( name = "Ταχύτητα ανέμου (w.s)" ) +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1), sides = "lrtb", size = 1 )

##############################

ggplot( data = weather, aes( x = dt, y = temp ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_discrete( name = "Χρόνος (dt)" ) +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )

##############################

ggplot( data = weather, aes( x = as.Date( dt, "%Y-%m-%d" ), y = temp ) ) +
  geom_point( shape = 1 ) +
  geom_smooth( ) +
  theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16 ),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
  scale_x_date( name = "Χρόνος (dt)" ) +
  scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
  theme( legend.position = "none" ) +
  geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )

##############################

### clean data ###
toremove  = union( sel, sel1 )
weather.f = weather[ -toremove , ]
##################

##### create pseudovariables

ggplot( data = weather.f, aes( x = hour, y = sol ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16)
        ) +
    scale_x_discrete( name  = "Ώρα (hour)")  +
    scale_y_continuous(name = "Ακτινοβολία (sol)" ) +
    theme( legend.position  = "none" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )


##############################

hour.f = rep( "day", length( weather.f$hour ) )

hour_f_results <- hour.f
if( 1 == DEBUG) {
    hour_f_results
}
timeslices <- c( 0:4, 20:23 )
for (i in timeslices) {
    hour.f[ weather.f$hour == i  ] = c( "night" )
}

table( hour.f )

ggplot( data = weather.f, aes( x = sol, y = temp, color = hour.f ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( ) +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Ακτινοβολία (sol) ") +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    theme( legend.position = "none" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1), sides = "lrtb", size = 1 )

##############################

weather.f = data.frame(weather.f, hour.f = hour.f)

season = rep(  "winter", length( weather.f$month ) )
season[ which( weather.f$month == 3 & weather.f$day > 21 ) ] = c( "spring" )
season[ which( weather.f$month == 4) ] = c( "spring" )
season[ which( weather.f$month == 5) ] = c( "spring" )
season[ which( weather.f$month == 6) ] = c( "spring" )
season[ which( weather.f$month == 6 & weather.f$day > 21 ) ] = c( "summer" )
season[ which( weather.f$month == 7) ] = c("summer")
season[ which( weather.f$month == 8) ] = c("summer")
season[ which( weather.f$month == 9) ] = c("summer")
season[ which( weather.f$month == 9 & weather.f$day > 23 ) ] = c( "fall" )
season[ which( weather.f$month == 10) ] = c("fall")
season[ which( weather.f$month == 11) ] = c("fall")
season[ which( weather.f$month == 12) ] = c("fall")
season[ which( weather.f$month == 12 & weather.f$day > 23 ) ] = c( "winter" )
weather.f = data.frame( weather.f, season )

##############################

mesi = rep( "π.μ.", length( weather.f$month ) )
mesi[ weather.f$hour >= 12 ] = c( "μ.μ." )
weather.f = data.frame( weather.f, mesi )

##############################

ggplot( data = weather.f[ hour.f == "day" , ], aes( x = sol, y = temp, color = season ) ) +
    geom_point( shape = 1 ) +
    geom_smooth() +
    theme(
            axis.title.x = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.x  = element_text( vjust = 0.5, size = 16 ),
            axis.title.y = element_text( face  = "bold", colour = "#990000", size = 20 ),
            axis.text.y  = element_text( angle = 90, vjust = 0.5, size = 16 )
        ) +
    scale_x_continuous( name = "Ακτινοβολία (sol)" ) +
    scale_y_continuous( name = "Θερμοκρασία (temp)" ) +
    geom_rug( col = rgb( 0.9, 0, 0, alpha = .1 ), sides = "lrtb", size = 1 )

### reduce variables
names <- function(weather.f) {
    c( "id", "temp", "bar", "hum", "sol", "w.d", "w.s", "dt", "month", "day", "hour", "hour.f", "season", "mesi" )
}

weather.f = weather.f[ , c( 2:7, 9, 11:14 ) ]

names <- function(weather.f) {
    c( "temp", "bar", "hum", "sol", "w.d", "w.s", "month", "hour", "hour.f", "season", "mesi" )
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

