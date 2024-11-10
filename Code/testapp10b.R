library("shiny")
library("shinyWidgets")
library("leaflet")
library("dplyr")
library("tmap")
library("sf")
library("ggplot2")
library("scales")

Point=sf::st_as_sf(data.frame(lng=-79.66403, lat=43.54972), 
                   coords = c("lng","lat"), crs=4326)
Territories.crop <- readRDS(here::here("Output/R_objects/Territories.crop.rds"))
CommunityData <- readRDS(here::here("Output/R_objects/CommunityData.rds"))
names(CommunityData) <- unlist(lapply(CommunityData, function(x) x$Name))

tmap_mode("view")

ui = tagList(
  navbarPage(
    theme = shinythemes::shinytheme("superhero"),
    tabPanel("Overview", "Reserve Allocation Maps"),
    tabPanel("Territories",
             mainPanel(width=8,
               leafletOutput("map"),
               DT::dataTableOutput(outputId = 'mShowTbl')
             ),
             sidebarPanel(width=4,
                          h3("Whose traditional territory is it?", style = "color:orange"),
                          p("Use this page to identify, and learn about, the Indigenous Peoples in whose traditional territory a selected location falls."),
                          br(),
                          h4("Instructions:", style = "color:orange"),
                          p("1. Use the pan and zoom in/out functions to find your location of interest on the map."),
                          p("2. Click on the location of interest."),
                          p("3. Click on the orange button 'Select new location'."),
                          p("4. Hover over the map to identify the traditional territories."),
                          p("5. Below the map, click on the name of the corresponding Indigenous People."),
                          p("6. This will open a separate browser window with further information and links.")
             ),
             sidebarPanel(width=4,
                          actionButton("use_clik_loc", "Select new location",
                                       class = "btn-primary btn-lg")
             ),

             sidebarPanel(width=4,
               sliderInput("zoom", "Zoom level:",
                           min = 3, max = 15, value = 5,
                           ticks=FALSE)
             ),
             sidebarPanel(width=4,
                          p("Source data: https://native-land.ca "),
             )
    ),
    tabPanel("Treaties", "This panel is intentionally left blank"),
    tabPanel("Reserves",
             mainPanel(width=8,
                       leafletOutput("map2")),
             sidebarPanel(width=4,
                          pickerInput(inputId = 'reserve',
                                      label = 'Community',
                                      choices = names(CommunityData),
                                      selected = names(CommunityData)[1],
                                      options = list(`style` = "btn-info")),
             mainPanel(width=4, 
                       plotOutput("pie")),
             mainPanel(width=4, 
                       plotOutput("stacked"))

             )
             
    ),
    tabPanel("Environment", "This panel is intentionally left blank")
  )
)


server <- function(input, output) {
  
  sliderValues <- reactive({data.frame(Name = c("Zoom"),
    Value = as.numeric(as.character(input$zoom)),
    stringsAsFactors = FALSE)
  })
  

  
  RV <- reactiveValues(lat=43.54972, lng=-79.66403, 
                       Point=sf::st_as_sf(data.frame(lng=-79.66403, lat=43.54972), 
                                          coords = c("lng","lat"), crs=4326),
                       tmp.Territories = Territories.crop[which(st_intersects(Point, Territories.crop, sparse = FALSE)), ] %>% 
                         mutate(Area=st_area(.)) %>% arrange(desc(Area)),
                       Reserve = 1)


  observeEvent(input$use_clik_loc, {
    last_click <- isolate(as.data.frame(input$map_click))
    RV$lat <- last_click$lat
    RV$lng <- last_click$lng
    RV$Point <- sf::st_as_sf(data.frame(lng=last_click$lng, lat=last_click$lat), 
                             coords = c("lng","lat"), crs=4326)
    RV$tmp.Territories <- Territories.crop[which(st_intersects(RV$Point, Territories.crop, sparse = FALSE)), ] %>% 
      mutate(Area=st_area(.)) %>% arrange(desc(Area))
  
    #test <<- RV$tmp.Territories
    #print(test)
    #print(RV$tmp.Territories)
  })
  
  # Territories:
  
   output$map <- renderLeaflet({
    leaflet(RV$tmp.Territories) %>%
      setView(lat = RV$lat, lng = RV$lng, zoom = input$zoom) %>%
      addProviderTiles("OpenTopoMap",group = "OpenTopoMap") %>%
      addMarkers(lng=RV$lng, lat=RV$lat) %>%
      addPolygons(fillColor=RV$tmp.Territories$color, color=RV$tmp.Territories$color,
                  weight=1, opacity=1, fillOpacity=0.5, label= ~Name,
                  highlightOptions = highlightOptions(
                    # Highlight stroke parameters
                    weight = 3, color = "white",
                    # Highlight fill parameters
                    fillColor = RV$tmp.Territories$color, fillOpacity = 0.1
                  ))
    
  })
     
     
   output$mShowTbl <- DT::renderDataTable({
     DT::datatable(as.data.frame(RV$tmp.Territories) %>% 
                     mutate(Territory=paste0("<a href='",RV$tmp.Territories$description,
                                             "' target='_blank'>",RV$tmp.Territories$Name,"</a>")) %>% 
                     arrange(Area) %>% select(Territory),
                   rownames = FALSE,
                   width = NULL,
                   height = NULL,
                   editable = FALSE,
                   selection = list(mode = "multiple", selected = c(), target = 'row',
                                    selectable=NULL),
                   escape=FALSE,
                   options = list(
                     scrollY = '325px',
                     class="compact",
                     paging = FALSE,
                     searching = FALSE,
                     ordering = FALSE,
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'steelblue', 'color': '#fff'});",   
                       "}")
                   )
     ) 
   })
   
   # Reserves:
   
   output$map2 <- renderTmap({
     
     tm_basemap(c("Esri.WorldTopoMap", "OpenTopoMap", "Esri.WorldImagery")) +
      
      tm_shape(CommunityData[[RV$Reserve]]$Territory) + 
       tm_sf(col="yellow", border.col="#ff3399", alpha=0.2, id="NAME1") +
      tm_shape(CommunityData[[RV$Reserve]]$Reserve) + 
       tm_sf(col=NA, border.col="#ff3399", alpha=0, id="NAME1") +
      tm_shape(CommunityData[[RV$Reserve]]$Soil.reserve, group="Soil suitability") + 
       tm_sf("CLI1", col="Color", border.col=NA, alpha=0.9, id="Label",
             legend.show=TRUE, popup.vars="Label")
     
     
   })
   
   output$pie <- renderPlot({
     
     ggplot(CommunityData[[RV$Reserve]]$Area.df[-2,], aes(x="", y=Area, fill=Type)) +
       geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
       geom_text(aes(y = Area/length(Area) + c(0, cumsum(Area)[-length(Area)]), 
                     label = percent(Area, accuracy=0.1)), size=5) + 
       scale_fill_viridis_d() +
       theme_void()
   })
   
   output$stacked <- renderPlot({
   })
     
     

}

shinyApp(ui, server)