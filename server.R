server <- function(input, output, session) {
 
  # code to collapse navbar after selecting a menu item
  observeEvent(
    eventExpr = input$navBar, 
    {
      runjs(
        'var elem = document.getElementsByClassName("navbar-collapse")[0]
       elem.setAttribute("aria-expanded", "false");
       elem.setAttribute("class", "navbar-collapse collapse");'
      )
    }
  )
  
# ***********************---- 
# page - home----
  
  # _home reactive values----
  hm <- reactiveValues(
    i = 1, # i = index of image
    n = 5  # n = # of home images
  )
  
  # _ui home----
  output$home <- renderUI(
    if (input$navBar == '0') {
      fluidRow(
        style = paste0(
          'background-image:url(pg0_', hm$i, '.jpg);',
          'background-size:cover;',
          'padding-top:25%; padding-bottom:25%;'
        ),
        align = 'center',
        h1(style = 'color:#fdfe02;', 'Hong Kong'),
        actionBttn(
          inputId = 'left',
          label = '<',
          style = 'simple',
          size = 'md'
        ),
        actionBttn(
          inputId = 'right',
          label = '>',
          style = 'simple',
          size = 'md'
        )
      )
    }
  )
  
  # _event left button----
  observeEvent(
    eventExpr = input$left,
    if (hm$i > 1) {
      hm$i <- hm$i - 1
    } else {
      hm$i <- hm$n
    }
  )
  
  # _event right button----
  observeEvent(
    eventExpr = input$right,
    if (hm$i < hm$n) {
      hm$i <- hm$i + 1
    } else {
      hm$i <- 1
    }
  )

# ***********************----
# page - hotels----
  
  # _navigate to hotels page----
  observeEvent(
    eventExpr = input$pg1Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'a'
    )
  )
  
  # _hotels reactive values----
  hotelSel <- reactiveValues(
    id = NULL, nm = NULL, st = NULL, # hotel_id, hotel_name, star_rating
    ad = NULL, ws = NULL,            # address, website
    lt = NULL, ln = NULL,            # latitude, longitude
    p1 = 0, p2 = 0, p3 = 0,          # price per night for 3 rooms
    i = 1, n = 5                     # image index, total images per hotel
  )
  
  # _ui hotels----
  output$hote <- renderUI(
    if (input$navBar == 'a') {
      fluidRow(
        column(
          width = 6,
          wellPanel(
            radioGroupButtons(
              inputId = 'neigh',
              label = '1. Select an area to explore:',
              choices = c(
                'Hong Kong'
              ),
              selected = character(0),
              justified = TRUE
            ),
            pickerInput(
              inputId = 'hotel',
              label = NULL,
              choices = NULL,
              width = '100%',
              options = list(
                title = ' Select a hotel'
              )
            ),
            hr(),
            uiOutput('hotelInfo')
          )
        ),
        column(
          width = 6,
          tabsetPanel(
            id = 'hTabs',
            type = 'pills',
            # __ui hotel map----
            tabPanel(
              title = 'Hotel Map',
              value = 'h1',
              leafletOutput(
                outputId = 'hotMap',
                height = '700px'
              )
            ),
            # __ui booking form----
            tabPanel(
              title = 'Booking Form',
              value = 'h2',
              uiOutput('book')
            )
          )
        )
      )
    }
  )

  # _event neigh click----
  observeEvent(
    eventExpr = input$neigh,
    {
      h <- hot #%>% 
        #filter(neigh == input$neigh)
      hotelSel$id <- NULL
      updatePickerInput(
        session = session,
        inputId = 'hotel',
        choices = h$hotel_name
      )
    }
  )
  
  # _hotel map----
  output$hotMap <- renderLeaflet(
    {
      if (!is.null(input$neigh)) {
        mapData <- hot  #%>% 
          #filter(neigh == input$neigh)
        leaflet(
          data = mapData,
          options = leafletOptions(zoomControl = TRUE)
        ) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%  
          addMarkers(
            lng = ~lng,
            lat = ~lat,
            label = ~hotel_name,
          ) 
      } 
    }
  )
  
  # _event hotel picker----
  observeEvent(
    eventExpr = input$hotel,
    if (input$hotel != '') {
      h <- hot %>% 
        filter(hotel_name == input$hotel)
      hotelSel$lt <- h$lat
      hotelSel$ln <- h$lng
      hotelSel$id <- h$hotel_id
      hotelSel$nm <- h$hotel_name
      hotelSel$ad <- h$address
      hotelSel$ws <- h$website
      hotelSel$st <- h$star_rating
      hotelSel$i <- 1
      leafletProxy(
        mapId = 'hotMap',
      ) %>%
        clearMarkers() %>% 
        addMarkers(
          layerId = '0',
          lng = hotelSel$ln,
          lat = hotelSel$lt,
          label = hotelSel$nm
        ) %>% 
        setView(
          lng = hotelSel$ln,
          lat = hotelSel$lt,
          zoom = 15
        )
    }
  )
  
  # _event hotel map marker----
  observeEvent(
    eventExpr = input$hotMap_marker_click,
    {
      hh <- input$hotMap_marker_click
      h <- hot %>% 
        filter(lat == hh$lat & lng == hh$lng)
      hotelSel$lt <- h$lat
      hotelSel$ln <- h$lng
      hotelSel$id <- h$hotel_id
      hotelSel$nm <- h$hotel_name
      hotelSel$ad <- h$address
      hotelSel$ws <- h$website
      hotelSel$st <- h$star_rating
      hotelSel$i <- 1
      updatePickerInput(
        session = session,
        inputId = 'hotel',
        selected = hotelSel$nm
      )
    }
  )
  
  # _ui hotel info----
  output$hotelInfo <- renderUI(
    {
      if (!is.null(hotelSel$id)) {
        wellPanel(
          align = 'center',
          style = 'background-color:#4e4e4e;
                   padding-top:5px; padding-bottom:0px; 
                   padding-left:10px; padding-right:10px;'
          ,
          # __hotel name----
          h4(
            style = 'text-align:left;', 
            tags$a(
              href = hotelSel$ws,
              target = '_blank',
              hotelSel$nm
            )
          ),
          # __hotel address----
          h6(style = 'text-align:left;', hotelSel$ad),
          fluidRow(
            # __hotel star rating----
            column(
              width = 4,
              align = 'left',
              h5(style = 'color:#01cdfe;', 
                 str_dup('\U2605\U0020', hotelSel$st))
            ),
            # __hotel owner----
            column(
              width = 8,
              align = 'right',
              if (input$hotel != '') {
                h5(
                  paste0(
                    'Owner: ', 
                    dbGetQuery(
                      conn = con,
                      statement = paste0(
                        'SELECT owner_name ',
                        'FROM hotels JOIN owners USING (owner_id) ',
                        'WHERE hotel_name = \'', input$hotel, '\''
                      )
                    )
                  )
                )
              } 
              
            )
          ),
          # __hotel images----
          div(
            style = 'position:relative; overflow:hidden; 
                     border:5px solid #ffffff;
                     background-color:#4e4e4e;',
            img(
              src = paste0(
                'hotels/', hotelSel$id, '_', 
                str_pad(input$imgCtr, 2, 'left', '0'), '.jpg'
              ),
              width = 'auto',
              height = '370'
            )
          ),
          br(),
          # __hotel image slider----
          sliderInput(
            inputId = 'imgCtr',
            label = NULL,
            min = 1,
            max = hotelSel$n,
            value = hotelSel$i,
            step = 1,
            ticks = FALSE,
            width = '90%'
          )
        )
      }
    }
  )
  
  # _event image slider move----
  observeEvent(
    eventExpr = input$imgCtr,
    {
      hotelSel$i <- as.numeric(input$imgCtr)
    }
  )
  
  # _booking form----
  output$book <- renderUI(
    if (!is.null(hotelSel$id)) {
      wellPanel(
        align = 'center',
        fluidRow(
          column(
            width = 4,
            dateInput(
              inputId = 'bookDate1',
              label = 'From',
              min = Sys.Date(),
              value = Sys.Date()
            )
          ),
          column(
            width = 4,
            dateInput(
              inputId = 'bookDate2',
              label = 'To',
              min = Sys.Date() + 1,
              value = Sys.Date() + 1
            )
          ),
          column(
            width = 4,
            pickerInput(
              inputId = 'gsts',
              label = 'Number of Guests:',
              choices = c(1:4),
              selected = 2
            )
          )
        ),
        hr(),
        actionBttn(
          inputId = 'viewRms',
          label = 'View Available Rooms',
          style = 'fill',
          color = 'success',
          size = 'md'
        ),
        hr(),
        uiOutput('availRms')
      )
    }
  )
  
  # __event view rooms button----
  observeEvent(
    eventExpr = input$viewRms,
    {
      stmt <- paste0(
        'SELECT avg(price_pn) m, stddev(price_pn) s\n',
        'FROM bookings\n',
        'WHERE hotel_id = \'', hotelSel$id, '\'\n',
        'AND guests = ', input$gsts, '\n',
        'AND date_part(\'month\', chkin_dt) = ', 
        month(input$bookDate1), ';'
      )
      hp <- dbGetQuery(con, stmt)
      hotelSel$p1 <- round(hp$m *(1 + runif(1, 0.1, 0.2)), 2)
      hstar <- hot %>% 
        filter(hotel_id == hotelSel$id)
      hotelSel$p2 <- round(hotelSel$p1 + hstar$star_rating / 4 * as.numeric(input$gsts) * 50
                           *(1 + runif(1, 0.1, 0.2)), 2)
      hotelSel$p3 <- round(hotelSel$p2 * (1 + runif(1, 0.25, 0.5)), 2)
    }
  )
  
  # __ui avail rooms----
  output$availRms <- renderUI(
    if (input$viewRms > 0) {
      div(
        wellPanel(
          fluidRow(
            column(
              width = 9,
              div(
                h4('Standard Room'),
                h3(paste0('Price/Night: ', dollar(hotelSel$p1)))
              )
            ),
            column(
              width = 3,
              actionBttn(
                inputId = 'bookRm1',
                label = 'Book This Room',
                style = 'fill',
                color = 'primary',
                size = 'sm'
              )
            )
          )
        ),
        wellPanel(
          fluidRow(
            column(
              width = 9,
              div(
                h4('Premium Room'),
                h3(paste0('Price/Night: ', dollar(hotelSel$p2)))
              )
            ),
            column(
              width = 3,
              actionBttn(
                inputId = 'bookRm2',
                label = 'Book This Room',
                style = 'fill',
                color = 'primary',
                size = 'sm'
              )
            )
          )
        ),
        wellPanel(
          fluidRow(
            column(
              width = 9,
              div(
                h4('The Presidential Suite'),
                h3(paste0('Price/Night: ', dollar(hotelSel$p3)))
              )
            ),
            column(
              width = 3,
              actionBttn(
                inputId = 'bookRm3',
                label = 'Book This Room',
                style = 'fill',
                color = 'primary',
                size = 'sm'
              )
            )
          )
        )
      )
    }
  )
  
  # __event adjust date 2----
  observeEvent(
    eventExpr = input$bookDate1,
    {
      updateDateInput(
        session = session,
        inputId = 'bookDate2',
        value = input$bookDate1 + 1
      )
    }
  )
  
  # __event book room 1 button----
  observeEvent(
    eventExpr = input$bookRm1,
    {
      los <- input$bookDate2 - input$bookDate1
      rmr <- hotelSel$p1 * as.numeric(los)
      txf <- round(0.15 * rmr, 2)
      shinyalert(
        title = 'Enjoy your stay!',
        text = paste0(
          '<hr>Your booking for ',
          los,
          ifelse(los == 1, ' night at ', ' nights at<br>'),
          '<h4>', hotelSel$nm, '</h4>',
          'is confirmed.<hr>Your total cost will be:<br>',
          '<h3>', dollar(rmr + txf), '</h3>',
          '= ', los, ' x ', dollar(hotelSel$p1), ' (room)<br>',
          '+ ', dollar(txf), ' (taxes & fees)'
        ),
        type = 'success',
        html = TRUE
      )
    }
  )
  
  # __event book room 2 button----
  observeEvent(
    eventExpr = input$bookRm2,
    {
      los <- input$bookDate2 - input$bookDate1
      rmr <- hotelSel$p2 * as.numeric(los)
      txf <- round(0.15 * rmr, 2)
      shinyalert(
        title = 'Enjoy your stay!',
        text = paste0(
          '<hr>Your booking for ',
          los,
          ifelse(los == 1, ' night at ', ' nights at<br>'),
          '<h4>', hotelSel$nm, '</h4>',
          'is confirmed.<hr>Your total cost will be:<br>',
          '<h3>', dollar(rmr + txf), '</h3>',
          '= ', los, ' x ', dollar(hotelSel$p2), ' (room)<br>',
          '+ ', dollar(txf), ' (taxes & fees)'
        ),
        type = 'success',
        html = TRUE
      )
    }
  )
  
  # __event book room 3 button----
  observeEvent(
    eventExpr = input$bookRm3,
    {
      los <- input$bookDate2 - input$bookDate1
      rmr <- hotelSel$p3 * as.numeric(los)
      txf <- round(0.15 * rmr, 2)
      shinyalert(
        title = 'Enjoy your stay!',
        text = paste0(
          '<hr>Your booking for ',
          los,
          ifelse(los == 1, ' night at ', ' nights at<br>'),
          '<h4>', hotelSel$nm, '</h4>',
          'is confirmed.<hr>Your total cost will be:<br>',
          '<h3>', dollar(rmr + txf), '</h3>',
          '= ', los, ' x ', dollar(hotelSel$p3), ' (room)<br>',
          '+ ', dollar(txf), ' (taxes & fees)'
        ),
        type = 'success',
        html = TRUE
      )
    }
  )
  
  
  # ***********************----
  
  # page - Disneyland----
  
  # _navigate to Disneyland page----
  observeEvent(
    eventExpr = input$pg6Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'f'
    )
  )
  
  # _disney reactive values----
  disSel <- reactiveValues(
    id = NULL, nm = NULL,  # attr_id, attr_name
    ws = NULL, od = NULL   # website, opening date
  )
  
  # _ui Disneyland----
  output$disn <- renderUI(
    if (input$navBar == 'f') {
      fluidRow(
        column(
          width = 6,
          align = 'center',
          radioGroupButtons(
            inputId = 'park',
            label = NULL,
            choices = dis$park_name,
            selected = character(0),
            direction = 'vertical',
            justified = FALSE
          ),
          uiOutput('disatt')
        ),
        column(
          width = 6,
          uiOutput('disImg')
        )
      )
    }
  )
  
  # _event radio park click----
  observeEvent(
    eventExpr = input$park,
    if (!is.null(input$park)) {
      d <- dis %>% 
        filter(park_name == input$park)
      disSel$id <- d$park_id
      disSel$nm <- d$park_name
      disSel$ws <- d$website
      disSel$od <- d$opening_dt
    }
  )
  
  # _ui Disney Image----
  output$disImg <- renderUI(
    {
      if (!is.null(disSel$id)) {
        wellPanel(
          align = 'center',
          style = 'background-color:#4e4e4e;
                   padding:10px;'
          ,
          # __park name----
          h4(
            style = 'text-align:left;', 
            tags$a(
              href = disSel$ws,
              target = '_blank',
              disSel$nm
            )
          ),
          # __park opening date----
          h6(style = 'text-align:left;', disSel$od),
          # __attr image----
          div(
            style = 'position:relative; overflow:hidden; 
                     border:5px solid #ffffff;
                     background-color:#4e4e4e;',
            img(
              src = paste0(
                'disn/pk', disSel$id, '.jpg'
              ),
              width = 'auto',
              height = '500px'
            )
          )
        )
      }
    }
  )
  
  # _ui disney attractions table----
  output$disatt <- renderUI(
    if (!is.null(disSel$id)) {
      dataTableOutput('disattTbl')
    }
  )
  
  # __output disney attractions table----
  output$disattTbl <- renderDataTable(
    {
      z <- dis_att %>% 
        filter(park_id == disSel$id) %>% 
        select('Attraction' = ds_attraction_name)
      if (nrow(z) > 0) {
        c <- paste0(
          '<style>th {text-align: center; color:white;}</style>',
          '<table><thead><tr>',
          '<th>Park Attraction</th>',
          '</tr></thead></table>'
        )
        datatable(
          data = z,
          class = 'cell-border stripe',
          rownames = FALSE,
          container = c,
          selection = 'single',
          options = list(
            pageLength = 5,
            paging = TRUE,
            searching = FALSE,
            ordering = FALSE,
            scrollX = TRUE,
            dom = 't', # shows table and nothing else
            columnDefs = list(
              list(className = 'dt-body-center', targets = 0:0)
            )
          )
        )
      }
    }
  )
  
  # __event click table row----
  observeEvent(
    eventExpr = input$disattTbl_selected_row,
    {
      print('a')
    }
  )
  
  # ***********************----
  # ***********************----
  
  # page - attractions----
  
  # _navigate to attractions page----
  observeEvent(
    eventExpr = input$pg2Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'b'
    )
  )
  
  # _attractions reactive values----
  attrSel <- reactiveValues(
    id = NULL, nm = NULL,  # attr_id, attr_name
    ad = NULL, ws = NULL,  # address, website
    lt = NULL, ln = NULL   # latitude, longitude
  )
  
  # _ui attractions----
  output$attr <- renderUI(
    if (input$navBar == 'b') {
      fluidRow(
        column(
          width = 6,
          radioGroupButtons(
            inputId = 'neigh2',
            label = NULL,
            choices = c(
              'HongKong'
            ),
            selected = character(0),
            justified = TRUE
          ),
          checkboxGroupButtons(
            inputId = 'attType',
            label = NULL,
            choices = sort(unique(att$attr_type)),
            selected = NULL,
            justified = TRUE,
            checkIcon = list(
              yes = icon(
                name = 'ok', 
                lib = 'glyphicon'
              )
            )
          ),
          leafletOutput(
            outputId = 'attMap',
            height = '520px'
          )
        ),
        column(
          width = 6,
          uiOutput('attrInfo')
        )
      )
    }
  )
  # _event attr neigh radio----
  observeEvent(
    eventExpr = input$neigh2,
    {
      attrSel$id <- NULL
      updateCheckboxGroupButtons(
        session = session,
        inputId = 'attType',
        selected = character(0)
      )
    }
  )
  
  # _event atttype checkbox----
  observeEvent(
    eventExpr = input$attType,
    if (!is.null(input$neigh2)) {
      leafletProxy(
        mapId = 'attMap',
      ) %>%
        hideGroup(unique(att$attr_type)) %>%
        showGroup(input$attType)
    }
  )
  
  # _attractions map----
  output$attMap <- renderLeaflet(
    {
      if (!is.null(input$neigh2)) {
        mapData <- att %>% 
          filter(neigh == input$neigh2)
        aIcon <- makeIcon(
          iconUrl = paste0('www/a_', str_to_lower(mapData$attr_type), '.png'),
          iconWidth = 50,
          iconHeight = 50,
          iconAnchorX = 25,
          iconAnchorY = 25
        )
        leaflet(
          data = mapData,
          options = leafletOptions(zoomControl = TRUE)
        ) %>%
          addProviderTiles(providers$Stamen.Terrain) %>%  
          addMarkers(
            lng = ~lng,
            lat = ~lat,
            label = ~attr_name,
            icon = aIcon,
            group = ~attr_type
          ) %>% 
          hideGroup(unique(att$attr_type))
      } 
    }
  )
  
  # _event attr map marker----
  observeEvent(
    eventExpr = input$attMap_marker_click,
    {
      aa <- input$attMap_marker_click
      a <- att %>% 
        filter(lat == aa$lat & lng == aa$lng)
      attrSel$lt <- a$lat
      attrSel$ln <- a$lng
      attrSel$id <- a$attr_id
      attrSel$nm <- a$attr_name
      attrSel$ad <- a$address
      attrSel$ws <- a$website
    }
  )
  
  # _ui attr info----
  output$attrInfo <- renderUI(
    {
      if (!is.null(attrSel$id) & length(input$attType) > 0) {
        wellPanel(
          align = 'center',
          style = 'background-color:#4e4e4e;
                   padding:10px;'
          ,
          # __attr name----
          h4(
            style = 'text-align:left;', 
            tags$a(
              href = attrSel$ws,
              target = '_blank',
              attrSel$nm
            )
          ),
          # __attr address----
          h6(style = 'text-align:left;', attrSel$ad),
          # __attr image----
          div(
            style = 'position:relative; overflow:hidden; 
                     border:5px solid #ffffff;
                     background-color:#4e4e4e;',
            img(
              src = paste0(
                'attrs/', attrSel$id, '.jpeg'
              ),
              width = 'auto',
              height = '500px'
            )
          ),
          # __attr dist to hotel----
          h5(
            if (!is.null(hotelSel$id)) {
              paste0(
                'Distance to ', hotelSel$nm, ' = ', 
                dist(hotelSel$lt, hotelSel$ln, attrSel$lt, attrSel$ln), ' km'
              )
            } else {
              'Select a hotel to measure the distance to here.'
            }
          )
        )
      }
    }
  )
  
  # ***********************----
  
  # page - restaurants----
  
  # _navigate to restaurants page----
  observeEvent(
    eventExpr = input$pg3Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'c'
    )
  )
  
  # _restaurants reactive values----
  restSel <- reactiveValues(
    id = NULL, nm = NULL,  # rest_id, rest_name, address
    ad = NULL, ws = NULL,  # address, website
    lt = NULL, ln = NULL   # latitude, longitude
  )
  
  # _ui restaurants----
  output$rest <- renderUI(
    if (input$navBar == 'c') {
      fluidRow(
        column(
          width = 6,
          radioGroupButtons(
            inputId = 'neigh3',
            label = NULL,
            choices = c(
              'Hong Kong'
            ),
            selected = character(0),
            justified = TRUE
          ),
          checkboxGroupButtons(
            inputId = 'resType',
            label = NULL,
            choices = sort(unique(res$rest_type)),
            selected = NULL,
            justified = TRUE,
            checkIcon = list(
              yes = icon(
                name = 'ok', 
                lib = 'glyphicon'
              )
            )
          ),
          leafletOutput(
            outputId = 'resMap',
            height = '520px'
          )
        ),
        column(
          width = 6,
          uiOutput('restInfo')
        )
      )
    }
  )
  
  # _event rest neigh radio----
  observeEvent(
    eventExpr = input$neigh3,
    {
      restSel$id <- NULL
      updateCheckboxGroupButtons(
        session = session,
        inputId = 'resType',
        selected = character(0)
      )
    }
  )
  
  # _event restype checkbox----
  observeEvent(
    eventExpr = input$resType,
    if (!is.null(input$neigh3)) {
      leafletProxy(
        mapId = 'resMap',
      ) %>%
        hideGroup(unique(res$rest_type)) %>%
        showGroup(input$resType)
    }
  )
  
  # _restaurants map----
  output$resMap <- renderLeaflet(
    {
      if (!is.null(input$neigh3)) {
        mapData <- res 
        rIcon <- makeIcon(
          iconUrl = paste0('www/r_', str_to_lower(mapData$rest_type), '.png'),
          iconWidth = 50,
          iconHeight = 50,
          iconAnchorX = 25,
          iconAnchorY = 25
        )
        leaflet(
          data = mapData,
          options = leafletOptions(zoomControl = TRUE)
        ) %>%
          addProviderTiles(providers$Stamen.TonerLite) %>%  
          addMarkers(
            lng = ~lng,
            lat = ~lat,
            label = ~rest_name,
            icon = rIcon,
            group = ~rest_type
          ) %>% 
          hideGroup(unique(res$rest_type))
      } 
    }
  )
  
  # _event rest map marker----
  observeEvent(
    eventExpr = input$resMap_marker_click,
    {
      rr <- input$resMap_marker_click
      r <- res %>% 
        filter(lat == rr$lat & lng == rr$lng)
      restSel$lt <- r$lat
      restSel$ln <- r$lng
      restSel$id <- r$rest_id
      restSel$nm <- r$rest_name
      restSel$ad <- r$address
      restSel$ws <- r$website
    }
  )
  
  # _ui rest info----
  output$restInfo <- renderUI(
    {
      if (!is.null(restSel$id) & length(input$resType) > 0) {
        wellPanel(
          align = 'center',
          style = 'background-color:#4e4e4e;
                   padding:10px;'
          ,
          # __rest name----
          h4(
            style = 'text-align:left;', 
            tags$a(
              href = restSel$ws,
              target = '_blank',
              restSel$nm
            )
          ),
          # __rest address----
          h6(style = 'text-align:left;', restSel$ad),
          # __rest image----
          div(
            style = 'position:relative; overflow:hidden; 
                     border:5px solid #ffffff;
                     background-color:#4e4e4e;',
            img(
              src = paste0(
                'rests/', restSel$id, '.jpg'
              ),
              width = 'auto',
              height = '500px'
            )
          ),
          # __rest dist to hotel----
          h5(
            if (!is.null(hotelSel$id)) {
              paste0(
                'Distance to ', hotelSel$nm, ' = ', 
                dist(hotelSel$lt, hotelSel$ln, restSel$lt, restSel$ln), ' km'
              )
            } else {
              'Select a hotel to measure the distance to here.'
            }
          )
        )
      }
    }
  )
  
  # ***********************----
  
  # page - insights----
  
  # _insights reactive values----
  ins <- reactiveValues(
    cd1 = NULL, cd2 = NULL, cd3 = NULL, 
    cd4a = NULL, cd4b = NULL, # code for each insight
    i2 = NULL, # best/worst 3 months
    i4m = 0, i4d = 0 # mean for insight 4, sd for insight 4
  )
  
  # _navigate to insights page----
  observeEvent(
    eventExpr = input$pg4Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'd'
    )
  )
  
  # _ui insights----
  output$insi <- renderUI(
    if (input$navBar == 'd') {
      
      if (!is.null(input$insight)) {
        
        # __ui insight 1----
        if (input$insight == 'Insight 1') {
          div(
            style = 'padding-left:20px;',
            fluidRow(
              h3(
                'Number of Guests Per Stay by Hotel'
              ),
              hr(),
              pickerInput(
                inputId = 'hot1',
                label = NULL,
                choices = hot$hotel_name,
                width = '75%',
                options = list(
                  title = '1. Select a hotel'
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = 'center',
                sliderInput(
                  inputId = 'los',
                  label = 'Length of Stay (Nights):',
                  value = 3,
                  min = 1,
                  max = 5,
                  step = 1,
                  ticks = FALSE,
                  width = '75%'
                )
              ),
              column(
                width = 8,
                uiOutput('uiIns1b')
              )
            )
          )
        
          # __ui insight 2----  
        } else if (input$insight == 'Insight 2') {
          div(
            style = 'padding-left:20px;',
            fluidRow(
              h3(
                'Best / Worst Months of the Year by Hotel'
              ),
              hr(),
              pickerInput(
                inputId = 'hot2',
                label = NULL,
                choices = hot$hotel_name,
                width = '75%',
                options = list(
                  title = '1. Select a hotel'
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = 'center',
                switchInput(
                  inputId = 'beswor',
                  onStatus = 'primary',
                  offStatus = 'warning',
                  onLabel = 'Best',
                  offLabel = 'Worst',
                  value = TRUE
                ),
                uiOutput('uiIns2a')
              ),
              column(
                width = 8,
                uiOutput('uiIns2b')
              )
            )
          )
          
        # __ui insight 3---- 
        } else if (input$insight == 'Insight 3') {
          div(
            fluidRow(
              style = 'padding-left:20px;',
              h3(
                'Top / Bottom Five Hotels by Day of Week & Year of Check-Ins'
              ),
              hr(),
              column(
                width = 4,
                align = 'center',
                switchInput(
                  inputId = 'topbot',
                  onStatus = 'success',
                  offStatus = 'danger',
                  onLabel = 'Top',
                  offLabel = 'Bottom',
                  value = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                checkboxGroupButtons(
                  inputId = 'dow',
                  label = '1. Select Day(s) of Week',
                  choices = c(
                    'Sunday' = 0,
                    'Monday' = 1,
                    'Tuesday' = 2,
                    'Wednesday' = 3,
                    'Thursday' = 4,
                    'Friday' = 5,
                    'Saturday' = 6
                  ),
                  selected = character(0),
                  direction = 'vertical',
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon(
                      name = 'ok', 
                      lib = 'glyphicon'
                    )
                  )
                ),
                checkboxGroupButtons(
                  inputId = 'year',
                  label = '2. Select Year(s)',
                  choices = c(2019:2021),
                  selected = character(0),
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon(
                      name = 'ok', 
                      lib = 'glyphicon'
                    )
                  )
                )
              ),
              column(
                width = 8,
                uiOutput('uiIns3')
              )
            )
          )
        # __ui insight 4----  
        } else if (input$insight == 'Insight 4') {
          div(
            fluidRow(
              style = 'padding-left:20px;',
              h3(
                'Distribution of Lead Time for Bookings'
              ),
              hr()
            ),
            fluidRow(
              column(
                width = 4,
                radioGroupButtons(
                  inputId = 'neigh4',
                  label = '1. Select an area:',
                  choices = c(
                    'Hong Kong'
                  ),
                  selected = character(0),
                  justified = TRUE
                ),
                radioGroupButtons(
                  inputId = 'qtr4',
                  label = '2. Select Quarter of Check-In Date:',
                  choices = c(paste('Qtr', 1:4)),
                  selected = character(0),
                  justified = TRUE
                ),
                uiOutput('uiIns4a')
              ),
              column(
                width = 8,
                uiOutput('uiIns4b')
              )
            )
          )
        }
        # __ui insight 5----
      else if (input$insight == 'Insight 5') {
          div(
            style = 'padding-left:20px;',
            fluidRow(
              h3(
                'Average cost per guest per night at hotel each year'
              ),
              hr(),
              pickerInput(
                inputId = 'hotel5',
                label = NULL,
                choices = hot$hotel_name,
                width = '75%',
                options = list(
                  title = '1. Select a hotel'
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = 'center',
                sliderInput(
                  inputId = 'year',
                  label = 'Year:',
                  value = 2020,
                  min = 2019,
                  max = 2021,
                  step = 1,
                  ticks = FALSE,
                  width = '75%'
                )
              ),
              column(
                width = 8,
                uiOutput('uiIns5')
              )
            )
          )
        
        
        # __ui insight 6---- 
      } else if (input$insight == 'Insight 6') {
        div(
          fluidRow(
            style = 'padding-left:20px;',
            h3(
               'Disney v.s City'
            ),
            hr(),
            column(
              width = 4,
              align = 'center'
            )
          ),
          fluidRow(
            column(
              width = 6,
              checkboxGroupButtons(
                inputId = 'month',
                label = '1. Select Month(s) of the year',
                choices = c(
                  'Jan' = 1 ,
                  'Feb' = 2,
                  'Mar' = 3,
                  'Apr' = 4,
                  'May' = 5,
                  'Jun' = 6,
                  'Jul' = 7,
                  'Aug' = 8,
                  'Sep' = 9,
                  'Oct' = 10,
                  'Nov' = 11,
                  'Dec' = 12
                ),
                selected = character(0),
                justified = TRUE,
                checkIcon = list(
                  yes = icon(
                    name = 'ok', 
                    lib = 'glyphicon'
                  )
                )
              )
            ),
            column(
              width = 6,
              uiOutput('uiIns6')
            )
          )
        )
  # __ui insight 7----  
    } else if (input$insight == 'Insight 7') {
      div(
        style = 'padding-left:20px;',
        fluidRow(
          h3(
            'Sum Guest by District Quarterly'
          ),
          hr(),
          pickerInput(
            inputId = 'dis2',
            label = NULL,
            choices = disrict$district,
            width = '75%',
            options = list(
              title = 'Select a district'
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            align = 'center',
            # switchInput(
            #   inputId = 'beswor',
            #   onStatus = 'primary',
            #   offStatus = 'warning',
            #   onLabel = 'Best',
            #   offLabel = 'Worst',
            #   value = TRUE
            # ),
            uiOutput('uiIns7a')
          ),
          column(
            width = 8,
            uiOutput('uiIns7b')
           )
         )
       )
     }
    }
   }  
 )
  # _output insight 1----
  
  # __right side panel----
  output$uiIns1b <- renderUI(
    if (input$hot1 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            plotOutput('ins1b')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code1')
          )
        )
      )
    }
  )
  
  # ___plot guests per stay----
  output$ins1b <- renderPlot(
    if (input$hot1 != '') {
      ins$cd1 <- paste0(
        'SELECT guests, count(*) n\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE hotel_name = \'', input$hot1 , '\'\n',
        'AND nights = ', input$los, '\n',
        'GROUP BY 1\n',
        'ORDER BY 2 DESC;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd1
      )
      nn <- sum(z$n)
      treemap(
        dtf = z %>% 
                mutate(
                  ind = paste0(
                    guests, 
                    ' Guest', 
                    ifelse(guests == 1, '', 's'), 
                    ' Per Stay\n(',
                    percent(n/nn),
                    ')'
                  )
                ),
        index = 'ind',
        vSize = 'n',
        type = 'index',
        palette = 'Set2',
        title = paste(
          'Distribution of Guests Per Stay for',
          input$los, 
          'Night Stays at',
          input$hot1
        ),
        fontsize.title = 20,
        fontsize.labels = 16,
        border.col = 'grey'
      )
    }
  )
  
  # ___code insight 1----
  output$code1 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd1 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 2----
  
  # __left side panel----
  output$uiIns2a <- renderUI(
    if (input$hot2 != '') {
      wellPanel(
        align = 'center',
        h3(
          paste0(
            ifelse(input$beswor, 'Best', 'Worst'), 
            ' 3 Months'
          )
        ),
        hr(),
        dataTableOutput('ins2a')
      )
    }
  )
  
  # ___datatable insight 2----
  output$ins2a <- renderDataTable(
    if (input$hot2 != '') {
      c <- paste0(
        '<style>th {text-align: center; color:white;}</style>',
        '<table><thead><tr>',
        '<th>Month</th>',
        '<th>Revenue</th>',
        '</tr></thead></table>'
      )
      #print(ins$i2)
      datatable(
        data = ins$i2,
        class = 'cell-border stripe',
        container = c,
        rownames = FALSE,
        selection = 'single',
        options = list(
          pageLength = 3,
          paging = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          dom = 't', # shows table and nothing else
          columnDefs = list(
            list(className = 'dt-body-center', targets = 0:0),
            list(className = 'dt-body-right', targets = 1:1)
          ),
          order = list(
            list(1, ifelse(input$beswor, 'desc', 'asc'))
          )
        )
      ) %>% 
        formatCurrency(2:2, digits = 2)
    }
  )
  
  # __right side panel----
  output$uiIns2b <- renderUI(
    if (input$hot2 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            plotOutput('ins2b')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code2')
          )
        )
      )
    }
  )
  
  # ___plot monthly revenue----
  output$ins2b <- renderPlot(
    bg = 'transparent',
    if (input$hot2 != '') {
      ins$cd2 <- paste0(
        'SELECT mo, rev, CASE WHEN rnk <= 3 THEN 1 ELSE 0 END top3 FROM ',
        '(SELECT date_part(\'month\', chkin_dt) mo, sum(price_pn * nights) rev, ',
        'rank() OVER (ORDER BY sum(price_pn * nights) ',
        ifelse(input$beswor, 'DESC', 'ASC'), ') rnk ',
        'FROM bookings JOIN hotels USING (hotel_id) ',
        'WHERE hotel_name = \'', input$hot2, '\' ',
        'GROUP BY 1) a ',
        'ORDER BY 1;'
      )
      
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd2
      )
      ins$i2 <- z %>% 
        filter(top3 == 1) %>% 
        mutate(mth = month.abb[mo]) %>% 
        select(mth, rev)
      
      ggplot(z, aes(x = mo, y = rev, fill = top3)) +
        geom_bar(stat = 'identity', color = 'white') +
        geom_hline(yintercept = max(z$rev), 
                   color = 'blue', size = 1) +
        geom_hline(yintercept = min(z$rev), 
                   color = 'orange', size = 1) +
        labs(x = 'Month', y = 'Revenue') + 
        scale_x_discrete(limits = month.abb) +
        scale_y_continuous(labels = dollar_format()) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = 'none',
          axis.title.x = element_text(colour = 'white'),
          axis.title.y = element_text(colour = 'white'),
          axis.text.x = element_text(colour = 'white'),
          axis.text.y = element_text(colour = 'white'),
          panel.grid.major = element_line(colour = '#f5f7fb'),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            fill = '#ffffff',
            colour = '#ffffff',
            size = 0.5, 
            linetype = 'solid'
          )
        )
    }
  )
  
  # ___code insight 2----
  output$code2 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd2 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'CASE' = paste0(str_dup('&nbsp;', 7), 'CASE'),
            'WHEN' = paste0(str_dup('&nbsp;', 9), 'WHEN'),
            'ELSE' = paste0(str_dup('&nbsp;', 9), 'ELSE'),
            'END' = paste0(str_dup('&nbsp;', 7), 'END'),
            'rank' = paste0(str_dup('&nbsp;', 7), 'rank')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 3----
  
  # __right side panel----
  output$uiIns3 <- renderUI(
    if (!is.null(input$dow) & !is.null(input$year)) {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Results',
            dataTableOutput(
              outputId = 'ins3'
            )
          ),
          tabPanel(
            title = 'Code',
            htmlOutput(
              outputId = 'code3'
            )
          )
        )
      )
    }
  )
  
  # ___datatable insight 3----
  output$ins3 <- renderDataTable(
    if (!is.null(input$dow) & !is.null(input$year)) {
      ins$cd3 <- paste0(
        'SELECT hotel_name, sum(price_pn * nights) revenue, count(book_id) bookings\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE date_part(\'dow\', chkin_dt) IN (', paste0(input$dow, collapse = ','), ')\n',
        'AND date_part(\'year\', chkin_dt) IN (', paste0(input$year, collapse = ','), ')\n',
        'GROUP BY 1\n',
        'ORDER BY 2 ', ifelse(input$topbot, 'DESC ', 'ASC '), '\n',
        'LIMIT 5;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd3
      )
      c <- paste0(
        '<style>th {text-align: center; color:white;}</style>',
        '<table><thead><tr>',
        '<th>Rank</th>',
        '<th>Hotel</th>',
        '<th>Revenue</th>',
        '<th>Bookings</th>',
        '</tr></thead></table>'
      )
      datatable(
        data = z,
        class = 'cell-border stripe',
        container = c,
        rownames = TRUE,
        selection = 'single',
        options = list(
          pageLength = 10,
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = TRUE,
          dom = 't', # shows table and nothing else
          columnDefs = list(
            list(className = 'dt-body-center', targets = 0:1),
            list(className = 'dt-body-right', targets = 2:3)
          )
        )
      ) %>% 
        formatCurrency(2:2, digits = 2)
    }
  )
  
  # ___code insight 3----
  output$code3 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd3 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER'),
            'LIMIT' = paste0(str_dup('&nbsp;', 1), 'LIMIT')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 4----
  
  # __left side panel----
  output$uiIns4a <- renderUI(
    if (!is.null(input$neigh4) & !is.null(input$qtr4)) {
      wellPanel(
        align = 'center',
        h3('Lead Time Percentiles'),
        hr(),
        fluidRow(
          column(
            width = 4,
            h3(style = 'color:#ffe700', '25%-ile'),
            h3(
              style = 'color:#ffe700',
              paste0(
                sprintf('%.2f', ins$i4m - 0.6745 * ins$i4d),
                ' days'
              )
            )
          ),
          column(
            width = 4,
            h3(style = 'color:#74ee15', '50%-ile'),
            h3(
              style = 'color:#74ee15',
              paste0(
                sprintf('%.2f', ins$i4m),
                ' days'
              )
            )
          ),
          column(
            width = 4,
            h3(style = 'color:#4deeea', '75%-ile'),
            h3(
              style = 'color:#4deeea',
              paste0(
                sprintf('%.2f', ins$i4m + 0.6745 * ins$i4d),
                ' days'
              )
            )
          )
        )
      ) 
    }
  )
  
  # __right side panel----
  output$uiIns4b <- renderUI(
    if (!is.null(input$neigh4) & !is.null(input$qtr4)) {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            align = 'center',
            plotOutput('ins4'),
            hr(),
            uiOutput('binSldr')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code4a'),
            htmlOutput('code4b')
          )
        )
      )
    }
  )
  
  # ___plot lead time----
  output$ins4 <- renderPlot(
    bg = 'transparent',
    if (!is.null(input$neigh4) & !is.null(input$qtr4)) {
      ins$cd4a <- paste0(
        'SELECT chkin_dt - book_dt lead\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE date_part(\'qtr\', chkin_dt) = ', 
        as.numeric(str_sub(input$qtr4, -1)), ';'
      )
      ins$cd4b <- paste0(
        'SELECT avg(lead) mean, stddev(lead) sd\n',
        'FROM\n',
        '(\n',
        'SELECT chkin_dt - book_dt lead\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE date_part(\'qtr\', chkin_dt) = ', 
        as.numeric(str_sub(input$qtr4, -1)), '\n) a;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd4a
      )
      zz <- dbGetQuery(
        conn = con,
        statement = ins$cd4b
      )
      ins$i4m <- zz$mean
      ins$i4d <- zz$sd
      ggplot(z, aes(x = lead)) +
        geom_histogram(
          aes(y = ..density..),
          binwidth = input$bins, 
          color = 'grey', 
          alpha = 0.75
        ) +
        geom_segment(
          aes(x = zz$mean, 
              xend = zz$mean, 
              y = 0, 
              yend = dnorm(zz$mean, zz$mean, zz$sd)),
          color = '#74ee15', 
          size = 1
        ) +
        geom_segment(
          aes(x = zz$mean - 0.6745 * zz$sd, 
              xend = zz$mean - 0.6745 * zz$sd, 
              y = 0, 
              yend = dnorm(zz$mean - 0.6745 * zz$sd, zz$mean, zz$sd)),
          color = '#ffe700', 
          size = 1
        ) +
        geom_segment(
          aes(x = zz$mean + 0.6745 * zz$sd, 
              xend = zz$mean + 0.6745 * zz$sd, 
              y = 0, 
              yend = dnorm(zz$mean + 0.6745 * zz$sd, zz$mean, zz$sd)),
          color = '#4deeea', 
          size = 1
        ) +
        geom_smooth(
          method = 'gam',
          mapping = aes(y = dnorm(lead, zz$mean, zz$sd)),
          color = 'red',
          size = 2
        ) +
        xlim(0, 50) +
        labs(x = 'Lead Time (Days)', y = 'Density') +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = 'none',
          axis.title.x = element_text(colour = 'white'),
          axis.title.y = element_text(colour = 'white'),
          axis.text.x = element_text(colour = 'white'),
          axis.text.y = element_text(colour = 'white'),
          panel.grid.major = element_line(colour = '#000008'),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            fill = '#272a30',
            colour = '#272a30',
            size = 0.5, 
            linetype = 'solid'
          )
        )
    }
  )
  
  # ___bin width slider----
  output$binSldr <- renderUI(
    if (!is.null(input$neigh4) & !is.null(input$qtr4)) {
      sliderInput(
        inputId = 'bins',
        label = 'Histogram Bin Width (Days):',
        value = 4,
        min = 1,
        max = 7,
        step = 1,
        ticks = FALSE,
        width = '75%'
      )
    }
  )
  
  # ___code insight 4a----
  output$code4a <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd4a %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER'),
            'LIMIT' = paste0(str_dup('&nbsp;', 1), 'LIMIT')
          )
        ), 
      '</p>'
    )
  )
  
  # ___code insight 4b----
  output$code4b <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd4b %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER'),
            'LIMIT' = paste0(str_dup('&nbsp;', 1), 'LIMIT')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 5----
  
  # __right side panel----
  output$uiIns5 <- renderUI(
    if (input$hotel5 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            dataTableOutput('ins5')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code5')
          )
        )
      )
    }
  )
  
  # ___datatable insight5----
  output$ins5 <- renderDataTable(
    if (input$hotel5 != '') {
      ins$cd5 <- paste0(
        'SELECT hotel_name Hotel_Name, sum(price_pn)/sum(guests) Avg_Per_Guest\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE hotel_name = \'', input$hotel5 , '\'\n',
        'AND date_part(\'year\', book_dt) = ', input$year,'\n',
        'GROUP BY 1;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd5
      )
      datatable(
        data = z,
        class = 'cell-border stripe',
        rownames = FALSE,
        selection = 'single',
        options = list(
          pageLength = 3,
          paging = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          dom = 't', # shows table and nothing else
          columnDefs = list(
            list(className = 'dt-body-center', targets = 0:0),
            list(className = 'dt-body-right', targets = 1:1)
          )
        )
      ) %>%
        formatCurrency(2:2, digits = 2)
    }
  )
  
  
  # ___code insight 5----
  output$code5 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd5 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 6----
  
  # __right side panel----
  output$uiIns6 <- renderUI(
    if (!is.null(input$month)) {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Results',
            dataTableOutput(
              outputId = 'ins6'
            )
          ),
          tabPanel(
            title = 'Code',
            htmlOutput(
              outputId = 'code6'
            )
          )
        )
      )
    }
  )
  
  # ___datatable insight 6----
  output$ins6 <- renderDataTable(
    if (!is.null(input$month)) {
      ins$cd6 <- paste0(
        'SELECT hotel_name,avg(price_pn / guests ) cost_per_guest\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE date_part(\'month\', chkin_dt) IN (', paste0(input$month, collapse = ','), ')\n',
        'AND hotel_id in (\'131384\',\'131388\',\'131325\',\'131327\')\n',
        'GROUP BY 1;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd6
      )
      c <- paste0(
        '<style>th {text-align: center; color:white;}</style>',
        '<table><thead><tr>',
        '<th>Hotel Name</th>',
        '<th>Average price per guest</th>',
        '</tr></thead></table>'
      )
      datatable(
        data = z,
        class = 'cell-border stripe',
        container = c,
        rownames = FALSE,
        selection = 'single',
        options = list(
          pageLength = 10,
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = TRUE,
          dom = 't', # shows table and nothing else
          columnDefs = list(
            list(className = 'dt-body-center', targets = 0:1)
          )
        )
      ) %>% 
        formatCurrency(2:2, digits = 2)
    }
  )
  
  # ___code insight 6----
  output$code6 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd6 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 7----
  
  # __left side panel----
  output$uiIns7a <- renderUI(
    if (input$dis2 != '') {
      wellPanel(
        align = 'center',
        h3(
          paste0(
            #ifelse(input$beswor, 'Best', 'Worst'), 
            ' 4 Quaters',''
          )
        ),
        hr(),
        dataTableOutput('ins7a')
      )
    }
  )
  
  # ___datatable insight 7----
  output$ins7a <- renderDataTable(
    if (input$dis2 != '') {
      c <- paste0(
        '<style>th {text-align: center; color:white;}</style>',
        '<table><thead><tr>',
        '<th>Quater</th>',
        '<th>Sum Guests</th>',
        '</tr></thead></table>'
      )
      
      #print(ins$d2)
      datatable(
        data = ins$d2,
        class = 'cell-border stripe',
        container = c,
        rownames = FALSE,
        selection = 'single',
        options = list(
          pageLength = 4,
          paging = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          dom = 't', # shows table and nothing else
          columnDefs = list(
            list(className = 'dt-body-center', targets = 0:0),
            list(className = 'dt-body-right', targets = 1:1)
          )
        )
      ) #%>% 
       # formatCurrency(2:2, digits = 2)
    }
  )
  
  
  # __right side panel----
  output$uiIns7b <- renderUI(
    if (input$dis2 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            plotOutput('ins7b')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code7')
          )
        )
      )
    }
  )
  
  
  
  # ___plot monthly revenue----
  output$ins7b <- renderPlot(
    bg = 'transparent',
    if (input$dis2 != '') {
      ins$cd7 <- paste0(
        "select  date_part('quarter', chkin_dt) quater ,sum(guests) guests ",
        "FROM bookings JOIN hotels USING (hotel_id) ",
        "WHERE address like  '%",input$dis2,"%' GROUP by  ",
        "date_part('quarter', chkin_dt) order by date_part('quarter', chkin_dt)"
      )
      
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd7
      )
      z$guests =  as.numeric(z$guests)
      
      ins$d2 <- z
      ggplot(data=z,mapping=aes(x=quater,y=guests,fill=quater,group=factor(1)))+
        geom_bar(stat="identity")
      
    })
  
  
  
  
  # ___code insight 7----
  output$code7 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd7 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'CASE' = paste0(str_dup('&nbsp;', 7), 'CASE'),
            'WHEN' = paste0(str_dup('&nbsp;', 9), 'WHEN'),
            'ELSE' = paste0(str_dup('&nbsp;', 9), 'ELSE'),
            'END' = paste0(str_dup('&nbsp;', 7), 'END'),
            'rank' = paste0(str_dup('&nbsp;', 7), 'rank')
          )
        ), 
      '</p>'
    )
  )  
  
  
  
  
  # ***********************----
  
  # page - about us----
  
  # _navigate to about us page----
  observeEvent(
    eventExpr = input$pg5Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'e'
    )
  )

  # _ui about----
  output$abou <- renderUI(
    if (input$navBar == 'e') {
      h <- '125px'
      p <- '13px'
      fluidRow(
        style = 'padding-top:60px; padding-bottom:50px; margin:0px;
                 background-image:url(team.jpg); 
                 background-size:cover;
                 background-position:center;',
        h1(style = 'padding-left:25px;', 'Our Team'),
        hr(),
        fluidRow(
          column(
            width = 2,
            align = 'right',
            img(src = paste0('member1.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:rgba(0, 255, 0, 0.2); padding:', p, ';', 
              'border-style:solid; border-width: medium;'
            ),
            h2(own$owner_name[1]),
            h4(own$owner_role[1])
          ),
          column(
            width = 3,
            align = 'right',
            img(src = paste0('member2.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:rgba(0, 255, 0, 0.2); padding:', p, ';', 
              'border-style:solid; border-width: medium;'
            ),
            h2(own$owner_name[2]),
            h4(own$owner_role[2])
          )
        ),
        br(),
        fluidRow(
          column(
            width = 2,
            align = 'right',
            img(src = paste0('member3.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:rgba(0, 255, 0, 0.2); padding:', p, ';', 
              'border-style:solid; border-width: medium;'
            ),
            h2(own$owner_name[3]),
            h4(own$owner_role[3])
          ),
          column(
            width = 3,
            align = 'right',
            img(src = paste0('member4.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:rgba(0, 255, 0, 0.2); padding:', p, ';', 
              'border-style:solid; border-width: medium;'
            ),
            h2(own$owner_name[4]),
            h4(own$owner_role[4])
          )
        ),
        br(),
        fluidRow(
          column(
            width = 2,
            align = 'right',
            img(src = paste0('member5.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:rgba(0, 255, 0, 0.2); padding:', p, ';', 
              'border-style:solid; border-width: medium;'
            ),
            h2(own$owner_name[5]),
            h4(own$owner_role[5])
          ),
          column(
            width = 3,
            align = 'right',
            img(src = paste0('member6.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:rgba(0, 255, 0, 0.2); padding:', p, ';', 
              'border-style:solid; border-width: medium;'
            ),
            h2(own$owner_name[6]),
            h4(own$owner_role[6])
          )
        ),
        br(),
        fluidRow(
          column(
            width = 2,
            align = 'right',
            img(src = paste0('member7.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:rgba(0, 255, 0, 0.2); padding:', p, ';', 
              'border-style:solid; border-width: medium;'
            ),
            h2(own$owner_name[7]),
            h4(own$owner_role[7])
          )
        )
      )
    }
  )
  
}
    