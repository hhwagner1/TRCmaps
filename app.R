library(shiny)
library(leaflet)
library(dplyr)
library(tmap)
library(sf)
library(stringr)
library(ggplot2)
library("scales")
library("shinyWidgets")
library(shinythemes)
library(raster)


# Import and modify R objects 
# ---------------------------

Point=sf::st_as_sf(data.frame(lng=-79.66403, lat=43.54972), 
                   coords = c("lng","lat"), crs=4326)
Territories.crop <- readRDS(here::here("Output/R_objects/Territories.crop.rds"))

RCPdata <- readRDS(here::here("Output/R_objects/RCPdata.rds"))
Essential_Services <- readRDS(here::here("Output/R_objects/Essential_Services.rds"))
Essential_Services.F <- readRDS(here::here("Output/R_objects/Essential_Services2.rds"))
CommunityData <- readRDS(here::here("Output/R_objects/CommunityDataCombined.rds"))
names(CommunityData) <- unlist(lapply(CommunityData, function(x) x$Name))
Reserves <- Reduce(rbind, lapply(CommunityData[2:4], function(x) x$Fire$Reserve))
Reserves$Color <- c("red", "blue", "purple")
FireCounts <- readRDS(here::here("Output/R_objects/FireCounts .rds"))
MCFN.Territory <- Reduce(rbind, lapply(CommunityData[1], function(x) x$Territory))

# HW 29 Oct 2024:
# Explicitly load .tif files
infiles <- c(NA, "Fort_Al_fs.tif", NA, NA, "Peguis_FS.tif", "Halalt_fs.tif")
for(i in 1:length(CommunityData))
{
  if(!is.null(CommunityData[[i]]$Flood$Image))
  {
    CommunityData[[i]]$Flood$Image <- terra::rast(here::here(infiles[i]))
  }
}

# Create tables with external links
# ---------------------------------

FireLinks <- data.frame(
  #Community = c("Fort Albany", "Sturgeon Lake", "Lac Simon"),
  Community = names(CommunityData)[c(2:4)],
  Headline = c("Fort Albany wildfire evacuations", 
               "Sturgeon Lake wildfire damage", 
               "Lac Simon wildfire evacuations"),
  URL = c("https://www.cbc.ca/news/canada/sudbury/james-bay-coast-first-nations-fort-albany-evacuations-1.6884409", 
          "https://globalnews.ca/news/9688327/alberta-wildfires-40-structures-substantially-damaged-in-sturgeon-lake-cree-nation/", 
          "https://www.cbc.ca/news/canada/montreal/lebel-sur-quevillon-evacuation-wildfires-1.6864795")
)

FloodLinks <- data.frame(
  #Community = c("Fort Albany", "Peguis", "Halalt"),
  Community = names(CommunityData)[c(2,5,6)],
  Headline = c("Fort Albany Evacuations due to Flood Threat", 
               "Peguis Evacuees left in Limbo for Months after Flooding", 
               "Halalt Evacuation Preparations before Floods"),
  URL = c("https://www.baytoday.ca/local-news/flood-threat-spurs-evacuations-from-remote-northern-communities-6888185", 
          "https://www.cbc.ca/news/canada/manitoba/peguis-first-nation-flooding-evacuees-hotel-1.6725758", 
          "https://www.cheknews.ca/we-have-to-be-prepared-to-leave-incoming-king-tides-raise-flooding-fears-of-island-residents-1128819/")
)

Websites <- data.frame(
  #Community = c("Mississaugas of the Credit", "Fort Albany", "Sturgeon Lake", "Lac Simon", "Peguis", "Halalt")
  Community = names(CommunityData)[c(1:6)], 
  Headline = c("Mississaugas of the Credit First Nation", 
               "Fort Albany First Nation",
               "Sturgeon Lake Cree Nation",
               "Nation Anishnabe du Lac Simon",
               "Peguis First Nation", 
               "Halalt First Nation"),
  URL = c("https://mncfn.ca",
          "https://www.fortalbany.ca",
          "https://www.sturgeonlake.ca",
          "https://lacsimon.ca",
          "https://peguisfirstnation.ca",
          "https://halalt.org")
)

# User interface
# --------------

ui = tagList(
  navbarPage(
    theme = shinythemes::shinytheme("superhero"),
    tabPanel("Overview", "Reserve Allocation Maps"),
    
    # Home Page
    tabPanel("Home", 
             headerPanel("Home Page"),
             mainPanel(
               h3("Disclaimer", style = "color:orange"),
               htmlOutput("disclaimer.text2"), 
               
             ),
             sidebarPanel(width=4,
                          h4("Links to Community Websites", style = "color:orange"),
                          DT::dataTableOutput(outputId = 'WebTable')
             
)),
    
    # Territories:
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
                          p("Source data: https://native-land.ca"
                            ),
             )
    ),
    
    # Treaties:
    tabPanel("Treaties",
         mainPanel(width=8,
                   leafletOutput("treaty.map"),
                   DT::dataTableOutput(outputId = 'mShowTbl2')
        ),
        
        sidebarPanel(width=4,
                     h3("Treaties within the Mississaugas of the Credit First Nation Territory", style = "color:orange"),
                     p("Use this page to identify, and learn about, treaties in the Mississaugas of the Credit First Nation territory."),
                     br(),
                     h4("Instructions:", style = "color:orange"),
                     p("1. Use the pan and zoom in/out functions to view the treaty distributions on the map."),
                     p("2. Click on a treaty of interest."),
                     p("3. Click on the orange button 'Select Treaty Area'."),
                     p("4. Hover over the map to identify the treaty name."),
                     p("5. Below the map, click on the name of the corresponding treaty."),
                     p("6. This will open a separate browser window with further information and links.")
        ),
        
          sidebarPanel(width=4,
                     actionButton("map_shape_click", "Select Treaty Area",
                                  class = "btn-primary btn-lg")
        )),
    
    # Soil Quality:
    tabPanel("Reserves",
             fluidPage(
               fluidRow(
                 column(8, "Map of soil suitability classes on the reserve", 
                        leafletOutput("map2")),
                 column(4, "Select a community", 
                        pickerInput(inputId = 'reserve',
                                    label = NULL,
                                    choices = names(CommunityData),
                                    selected = names(CommunityData)[1],
                                    options = list(`style` = "btn-info")))),
               fluidRow(
                 column(8, "Relative frequency of soil suitability classes for territory vs. reserve",
                        plotOutput("stacked")),
                 column(4, "Relative size of reserve", 
                        plotOutput("pie"))
               )),
             
               sidebarPanel(width=8,
                            htmlOutput("CLI"),
               )
             ),

    # Fires:
    tabPanel("Fires", mainPanel(width=8, "Map of Projected Fire Counts on Reserves",
                                leafletOutput("Envmap"),
                                plotOutput("trendsfig")
                                
    ),
    sidebarPanel(width=4,
                 selectInput("climate", "Select climate change scenario", 
                             choices = c("RCP_2.6", "RCP_4.5", "RCP_8.5")),
                 selectInput("year", "Select projection period", 
                             choices = c("Year_2011_to_2040", 
                                         "Year_2041_to_2070", 
                                         "Year_2071_to_2100")),
                 actionButton("showfire", "Update map",
                              class = "btn-primary btn-lg")
    ),
    
  
    sidebarPanel(width=4,
                 DT::dataTableOutput(outputId = 'fireNewsTable'), 
    
),
    sidebarPanel(width=8,
                 htmlOutput("FireResource"),
)),
    
    # Flooding:
    tabPanel("Flooding", mainPanel(width=8, "Map of Flood Susceptibility Index on Reserves",
                                   leafletOutput("Floodmap")    
                                   
    ),
    sidebarPanel(width=4,
                 selectInput("flooding", "Select a Community",
                             choices = names(CommunityData)[c(2,5,6)]),
                 actionButton("showfloods", "Update map",
                              class = "btn-primary btn-lg")
    ),
    
    sidebarPanel(width=4,
                 DT::dataTableOutput(outputId = 'floodNewsTable')
                 
    ),
    
    sidebarPanel(width=8,
                 htmlOutput("FloodResource"),
    )
     ))
  
)

# Server
# ------

server <- function(input, output) {
  
  sliderValues <- reactive({data.frame(Name = c("Zoom"),
                                       Value = as.numeric(as.character(input$zoom)),
                                       stringsAsFactors = FALSE)
  })
  
  # Define holder for reactive values, initial values:
  RV <- reactiveValues(lat=43.54972, lng=-79.66403, 
                       Point=sf::st_as_sf(data.frame(lng=-79.66403, lat=43.54972), 
                                          coords = c("lng","lat"), crs=4326),
                       tmp.Territories = Territories.crop[which(st_intersects(Point, Territories.crop, sparse = FALSE)), ] %>% 
                         mutate(Area=st_area(.)) %>% arrange(desc(Area)),
                       #tmp.Treaties = MCFN.Treaties,
                       tmp.Treaties = CommunityData$MCFN$Treaties, # HW 29 Oct 2024
                       RCPmap = RCPdata[["RCP_2.6"]][["2040"]],
                       Reserve = 1,
                       Reserves.F = 2)

  
  # Disclaimer:
  output$disclaimer.text2 <- renderUI({
    HTML(paste("The maps on this app have been created based on publicly available data. Identifying the spatial scope of traditional territories and treaty agreements can be challenging and ambiguous, due to the extensive history within each community, treaty negotiations and land claims. Therefore, for instance, boundary locations should not be interpreted to be exact.",
               "We highlight certain First Nations communities based on proximity/relationship to UTM, and others based on recent news stories on environmental disasters that have affected these communities. While we cannot speak on behalf of these communities, we include links to their websites. We encourage users to consult these to learn more about the communities, including their priorities and initiatives.", 
               sep="<br/>"))
  })
  
  # Community Websites
  output$WebTable <- DT::renderDataTable({
    DT::datatable(Websites %>% 
                    dplyr::mutate(Communities=paste0("<a href='",URL,
                                              "' target='_blank'>", Headline,"</a>")) %>% 
                    dplyr::select(Communities),
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
  
  # Territories:
  
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
                    dplyr::mutate(Territory=paste0("<a href='",RV$tmp.Territories$description,
                                                   "' target='_blank'>",RV$tmp.Territories$Name,"</a>")) %>% 
                    dplyr::arrange(Area) %>% dplyr::select(Territory),
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
  
  # Treaties:
  
  observeEvent(input$map_shape_click, {
    polygon <- as.data.frame(input$map_shape_click)
    RV$lat <- polygon$lat
    RV$lng <- polygon$lng
    #MCFN.Treaties
    tmp.Treaties = CommunityData$MCFN$Treaties # HW 29 Oct 2024
  })
  
  
  output$treaty.map <- renderLeaflet({
    leaflet(RV$tmp.Treaties) %>%
      setView(lat = RV$lat, lng = RV$lng, zoom = input$zoom) %>%
      addProviderTiles("OpenTopoMap",group = "OpenTopoMap") %>%
      addPolygons(fillColor=RV$tmp.Treaties$color, color=RV$tmp.Treaties$color,
                  weight=1, opacity=1, fillOpacity=0.5, label= ~Name, 
                  highlightOptions = highlightOptions(
                    # Highlight stroke parameters
                    weight = 3, color = "white",
                    # Highlight fill parameters
                    fillColor = RV$tmp.Treaties$color, fillOpacity = 0.1
                  )) 
    
  })
  
  output$mShowTbl2 <- DT::renderDataTable({
    DT::datatable(as.data.frame(RV$tmp.Treaties) %>% 
                    dplyr::mutate(Treaties=paste0("<a href='",RV$tmp.Treaties$links,
                                                  "' target='_blank'>",RV$tmp.Treaties$Name2,"</a>")) %>%
                    dplyr::select(Treaties),
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
      
      tm_shape(CommunityData[[RV$Reserve]]$Territory, name="Territory") + 
      tm_sf(col="yellow", border.col="#ff3399", alpha=0.2, id="NAME1") +
      tm_shape(CommunityData[[RV$Reserve]]$Reserve, name="Reserve") + 
      tm_sf(col=NA, border.col="#ff3399", alpha=0, id="NAME1") +
      tm_shape(CommunityData[[RV$Reserve]]$Soil.reserve, name="Soil suitability") + 
      tm_sf("CLI1", col="Color", border.col=NA, alpha=0.9, id="Label",
            legend.show=TRUE, popup.vars="Label")
  })
  
  output$pie <- renderPlot({
    
    ggplot(CommunityData[[RV$Reserve]]$Area.df[-2,], aes(x="", y=Area, fill=Type)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
      geom_text(aes(y = Area/length(Area) + c(0, cumsum(Area)[-length(Area)]), 
                    label = percent(Area, accuracy=0.01)), size=5) + 
      scale_fill_viridis_d() +
      theme_void() + theme(legend.position = c(.5,1), 
                           legend.title=element_blank(),
                           text = element_text(size = 16))
  })
  
  output$stacked <- renderPlot({
    ggplot(CommunityData[[RV$Reserve]]$SoilTable, aes(fill=Label, y=Area, x=Type)) + 
      geom_bar(position="fill", stat="identity") + xlab(NULL) +
      scale_fill_manual(values=CommunityData[[RV$Reserve]]$Soilcomp$Color) + 
      geom_text(aes(y = label_y, label = CLI1), vjust = 0.5, colour = "black") +
      theme_light() + theme(text = element_text(size = 16))
  })
  
  # Canada Land Inventory Reference
  
  output$CLI <- renderUI({
    HTML(paste("Source data:",
               "https://native-land.ca",
               "https://open.canada.ca/data/en/dataset/0c113e2c-e20e-4b64-be6f-496b1be834ee", 
               sep="<br/>"))
  })
  
  # Fire maps
  
  observeEvent(input$showfire, {
    RV$Scenario = input$climate
    RV$Year = as.character(stringr::str_sub(input$year, -4, -1))
    RV$RCPmap = RCPdata[[RV$Scenario]][[RV$Year]]
  })
  
  output$Envmap <- renderTmap({ 
    tmap_mode("view")
    tm_basemap(leaflet::providers$OpenTopoMap, group='Topo') +
      tm_shape(RV$RCPmap) + tm_sf("FIRE_COUNT", alpha=0.7, 
                                  style="log10", palette="Reds",
                                  title="Fires per 10 km2") + 
      tm_shape(Reserves) + tm_sf(col="Color", id="NAME1") +
      tm_shape(Essential_Services) + tm_dots(size=0.1,col="yellow",shapeNA=NA) +
      tm_scale_bar() 
  })
  
  output$fireNewsTable <- DT::renderDataTable({
    DT::datatable(FireLinks %>% 
                    dplyr::mutate(News=paste0("<a href='",URL,
                                              "' target='_blank'>", Headline,"</a>")) %>% 
                    dplyr::select(News),
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
  
  output$trendsfig <- renderPlot(height=200,
                                 ggplot(FireCounts, aes(x=Year, y= FireCount, group=Scenario, col=Scenario)) + 
                                   geom_line() + facet_wrap(facets=~Community) + theme_light()
  )
  
  # Fire Data Reference
  
  output$FireResource <- renderUI({
    HTML(paste("Source Data:",
               "https://native-land.ca",
               "https://cfs.nrcan.gc.ca/fc-data-catalogue/read/15", 
               sep="<br/>"))
  })
  
  #Flooding
  
  observeEvent(input$showfloods, {
    RV$Reserves.F = which(names(CommunityData) == input$flooding)
  })
  
  output$Floodmap <- renderTmap({ 
    
    tmap_mode("view")
    tm_basemap(leaflet::providers$OpenTopoMap, group='Topo') +
      tm_shape(CommunityData[[as.numeric(RV$Reserves.F)]]$Flood$Image) + 
         tm_raster(palette = "Blues", alpha=0.9, title="Flood Susceptibility Index") + 
      tm_shape(CommunityData[[as.numeric(RV$Reserves.F)]]$Flood$Reserve) + tm_sf(col="orange", id="NAME1", alpha=0.4) +
      tm_shape(Essential_Services.F) + tm_dots(size=0.1,col="red2",shapeNA=NA) +
      tm_scale_bar() 
  })
  
  output$floodNewsTable <- DT::renderDataTable({
    DT::datatable(FloodLinks %>% 
                    dplyr::mutate(News=paste0("<a href='",URL,
                                              "' target='_blank'>", Headline,"</a>")) %>% 
                    dplyr::select(News),
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
  
  # Flooding Data Reference
  
  output$FloodResource <- renderUI({
    HTML(paste("Source Data:",
               "https://native-land.ca",
               "https://open.canada.ca/data/en/dataset/df106e11-4cee-425d-bd38-7e51ac674128", 
               sep="<br/>"))
  })
  
}

# Combine App
# -----------

shinyApp(ui, server)