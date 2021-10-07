#all pictures are replaced
ui <- tagList(
  
  useShinyjs(),
  useShinyalert(),

  # ***********************----
  # navbarpage layout----
  navbarPage(
    id = 'navBar',
    # hyperlink to a website of your choice
    title = tags$a(
      href = 'https://sps.columbia.edu/academics/masters/applied-analytics',
      target = '_blank',
      img(src = 'cu_light.png', height = '25px')
    ),
    windowTitle = 'Hong Kong', #<--what the tab on browser will state
    position = 'fixed-top', #<--fixed-bottom if you want menu bar on bottom
    collapsible = TRUE, #<--collapse navbar tabs when window is shrunk
    theme = shinytheme('slate'), #<--select shinytheme https://rstudio.github.io/shinythemes/
      # cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper
      # readable, sandstone, simplex, slate, spacelab, superhero, united, yeti
    
    # ***********************----
    # home page----
    tabPanel(
      value = '0',
      title = div(
        img(src = 'vlogo.jpg', height = 25) #<--image for the home page
      ),
      # ui home rendered on server
      uiOutput('home'),
      
      # _hotel section----
      fluidRow(
        style = 'background-image:url(pg1.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg1Bttn',
          label = h1('Hotels'),
          style = 'simple'
        )
      ),
      
      # _disneyland section----
      fluidRow(
        style = 'background-image:url(ds1.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg6Bttn',
          label = h1('Disneyland'),
          style = 'simple'
        )
      ),
      
      # _attractions section----
      fluidRow(
        style = 'background-image:url(pg2.jpg); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg2Bttn',
          label = h1('Attractions'),
          style = 'simple'
        )
      ),
      
      # _restaurants section----
      fluidRow(
        style = 'background-image:url(pg3.jpg); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg3Bttn',
          label = h1('Restaurants'),
          style = 'simple'
        )
      ),
      
      # _insights section----
      fluidRow(
        style = 'background-image:url(pg4.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg4Bttn',
          label = h1('Insights'),
          style = 'simple'
        )
      ),
      
      # _about us section----
      fluidRow(
        style = 'background-image:url(pg5.jpg); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg5Bttn',
          label = h1('About Us'),
          style = 'simple'
        )
      ),
      
      # _footer section----
      fluidRow(
        wellPanel(
          style = 'padding:20px; background-color:#ffffff;',
          align = 'center',
          h4('Our Partners:'),
          hr(),
          tags$span(
            # __marriott
            tags$a(
              href = 'https://marriott.com/',
              target = '_blank',
              img(src = 'marriott.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __hyatt
            tags$a(
              href = 'https://www.hyatt.com/',
              target = '_blank',
              img(src = 'hyatt.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __intercontinental
            tags$a(
              href = 'https://www.intercontinental.com/',
              target = '_blank',
              img(src = 'intercontinental.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __four seasons
            tags$a(
              href = 'https://fourseasons.com/',
              target = '_blank',
              img(src = 'fourseasons.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __mandarin 
            tags$a(
              href = 'https://www.mandarinoriental.com/',
              target = '_blank',
              img(src = 'mandarin.png', height = '30px')
            )
          )
        )
      )
    ),
  
    # ***********************----
    # tabs----
    # _hotels tab----
    tabPanel(
      value = 'a',
      title = div(
        img(src = 'icon_1.png', height = '25px'), 
        HTML('&nbsp;'), 'Hotels'
      ),
      h3(style = 'padding-top:70px;', 'Hong Kong Hotels'),
      hr(),
      uiOutput('hote')
    ),
    
    # _disneyland tab----
    tabPanel(
      value = 'f',
      title = div(
        img(src = 'icon_ds.png', height = '25px'), 
        HTML('&nbsp;'), 'Disneyland'
      ),
      h3(style = 'padding-top:70px;', 'Disneyland Hong Kong'),
      hr(),
      uiOutput('disn')
    ),
  
    # _attractions tab----
    tabPanel(
      value = 'b',
      title = div(
        img(src = 'icon_2.png', height = '25px'), 
        HTML('&nbsp;'), 'Attractions'
      ),
      h3(style = 'padding-top:70px;', 'Hong Kong Attractions'),
      hr(),
      uiOutput('attr')
    ),
    
    # _restaurants tab----
    tabPanel(
      value = 'c',
      title = div(
        img(src = 'icon_3.png', height = '25px'), 
        HTML('&nbsp;'), 'Restaurants'
      ),
      h3(style = 'padding-top:70px;', 'Hong Kong Restaurants'),
      hr(),
      uiOutput('rest')
    ),
    
    # _insights tab----
    tabPanel(
      value = 'd',
      title = div(
        img(src = 'icon_4.png', height = '25px'), 
        HTML('&nbsp;'), 'Insights'
      ),
      fluidRow(
        style = 'padding-top:70px; padding-left:10px; padding-right:10px;',
        radioGroupButtons(
          inputId = 'insight',
          label = 'Select an Insight to View:',
          choices = c(paste0('Insight ', 1:7)),
          selected = character(0),
          justified = TRUE
        )
      ),
      uiOutput('insi')
    ),
    
    # _about us tab----
    tabPanel(
      value = 'e',
      title = div(
        img(src = 'icon_5.png', height = '25px'), 
        HTML('&nbsp;'), 'About Us'
      ),
      uiOutput('abou')
    )
  
  )

)
  

