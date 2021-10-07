library(DT)
library(leaflet)
library(lubridate)
library(RColorBrewer)
library(RPostgres)
library(scales)
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(treemap)

# connection to the hotelsV database----
con <- dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'hotels4',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com', 
  port = 25061,
  user = 'proj4',
  password = 'n4rvell8mim96674',
  sslmode = 'require'
)

# hotels table----
hot <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM hotels ORDER BY hotel_name'
)


dis <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM disneyland ORDER BY park_name'
)

dis_att <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM ds_attractions ORDER BY ds_attraction_name'
)

att <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM attractions ORDER BY attr_name'
)

disrict <- dbGetQuery(
  conn = con,
  statement = "select  distinct  a[2] district from ( select regexp_split_to_array(address, ',') from hotels) as dt(a)"
)

res <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM restaurants ORDER BY rest_name'
)

own <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM owners'
)

# distance between two coordinates - Haversine formula----
dist <- function(lat1, lng1, lat2, lng2) {
  r <- 6378.1 #radius of earth in km
  f1 <- lat1 * pi / 180
  f2 <- lat2 * pi / 180
  d1 <- (lat2 - lat1) * pi / 180
  d2 <- (lng2 - lng1) * pi / 180
  a <- sin(d1 / 2) ^ 2 + cos(f1) * cos(f2) * sin(d2 / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  return(round(r * c, 2))
}

# when exiting app, disconnect from the kpop database
onStop(
  function()
  {
    dbDisconnect(con)
  }
)