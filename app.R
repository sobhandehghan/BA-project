library(shiny)
library(shinydashboard)
library(ggplot2)
library(rgl)
library(leaflet)
library(maps)
library(scales)
library(rayshader)
#library(dplyr)
#library(stringr)
#library(tidyverse)
#library(viridis)
#library(isoband)
worldpop <- read.csv("worldpopDone.csv")
gdp <- read.csv("worldGDPDone.csv")

ui <- dashboardPage(
  dashboardHeader(title = "World distribution"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "World_GDP",
      tabName = "worldgdp" ,
      icon = icon('globe')
    ),
    menuItem(
      text = "World_Population",
      tabName = "population" ,
      icon = icon('earth-americas')
    ) ,
    menuItem(
      text = "World Map",
      tabName = "worldm" ,
      icon = icon('map')
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "worldgdp" ,
      # fluidRow(width = 6 ,
      #           column(width = 3,offset = 1.5,
      #                  sliderInput(inputId = "year","Year",min = 1950 , max = 2100,step = TRUE,value = format(Sys.Date(),"%Y"))
      #                 ),
      #           column(width = 3,offset = 1.5,
      #                  radioButtons(inputId = "sex","Sex",choices = c("Male","Female","Total"),selected = "Total"))
      #          ),
      #
      rglwidgetOutput(
        outputId = 'wgdp',
        height = 700,
        width = "100%"
      )
    ),
    tabItem(
      tabName = "population",
      rglwidgetOutput(
        outputId = 'wpop',
        height = 700,
        width = "100%"
      )
    ),
    tabItem(tabName = "worldm",
            # tabBox(
            #   p("for test3")
            # )
            leafletOutput(outputId = 'mapp' , height = 700))
  ))
  
)
# Define server
server <- function(input, output) {
  #data1 <- read.csv("popbysex.csv")
  worldpop <- read.csv("worldpopDone.csv")
  gdp <- read.csv("worldGDPDone.csv")
  
  #######################################################3333333
  
  ############################### GDP map
  plain <- theme(
    #axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    #axis.title = element_blank(),
    panel.background = element_rect(fill = "#eeeeff"),
    plot.title = element_text(hjust = 0.5)
  )
  
  
  worldgdp <-
    ggplot(data = gdp,
           mapping = aes(x = long, y = lat, group = group)) + plain +
    coord_fixed(ratio = 2.5) +
    geom_polygon(aes(
      x = long,
      y = lat ,
      fill = GDP2016 / 1000000
    )) +
    scale_fill_distiller(
      palette = "OrRd",
      na.value = "#aaaaaa" ,
      direction = 1
    ) + # or direction=-1
    ggtitle("GDP report of countries in year 2016")
  
  
  output$wgdp <- renderRglwidget({
    # worldgdp
    plot_gg(
      worldgdp ,
      offset_edges = T,
      multicore = TRUE,
      width = 6,
      height = 6,
      scale = 300,
      windowsize = c(1400, 866),
      zoom = 0.55,
      theta = 0,
      phi = 75
    )
    rglwidget()
    # aa
  })
  #####################################pop map
  worldpopu <-
    ggplot(data = worldpop,
           mapping = aes(x = long, y = lat, group = group)) + plain +
    #coord_fixed(ratio = 2) +
    geom_polygon(aes(
      x = long,
      y = lat ,
      fill = pop / 1000000
    )) +
    scale_fill_distiller(
      palette = "Spectral",
      na.value = "#aaaaaa" ,
      direction = -1
    ) + # or direction=c1
    ggtitle("Population")
  
  output$wpop <- renderRglwidget({
    # worldpopu
    plot_gg(
      worldpopu ,
      offset_edges = T ,
      multicore = TRUE,
      width = 6,
      height = 6,
      scale = 300,
      windowsize = c(1400, 866),
      zoom = 0.55,
      theta = 0,
      phi = 75
    )
    rglwidget()
    # aa
  })
  
  ##################################### map
  basicmap <-
    leaflet(unique(worldpop$region))  %>% setView(lng = 5, lat = 25, zoom = 1.5)  %>%
    addTiles(options = tileOptions(maxZoom = 10, maxNativeZoom = 8),
             group = 'OSM')
  
  output$mapp <- renderLeaflet({
    basicmap
  })
}
#########################333 Run the application
shinyApp(ui = ui, server = server)
