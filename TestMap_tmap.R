library(shiny)
library(leaflet)

## 1. Define colours etc.
## ----------------------
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


## 2. Define user interface
## ------------------------

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  
  align='center',
  textAreaInput(inputId = 'mValueEntered',
                label = 'Enter coordinates',
                value = "43.548597, -79.662636"),
  
  actionButton(inputId = 'showTerritories',label = 'Enter'),
  DT::dataTableOutput(outputId = 'mShowTbl',width = '500px')
  
)

## 3. Define server logic
## ----------------------

# This is where all the R code goes

server <- function(input, output, session) {
  
  Map <- tm_basemap(leaflet::providers$OpenTopoMap, group='Topo')
  
  # Define the leaflet map:
  output$mymap <- renderTmap({
    Map + tm_shape(Regions %>% filter(juri_en == "Ontario")) + 
      tm_sf(border.col="blue", alpha=0) +
      tm_shape(AboriginalLands.sf %>% filter(JUR1=="ON")) + 
      tm_sf(col="red", border.col="red") 
  })
  
  
  rv <- reactiveValues(df=NULL)
  df <- list()
  
  observeEvent(input$showTerritories,{
    if (is.data.frame(df)==FALSE){
      df <- data.frame(Latitude=double(),
                       Longitude = double())}
    
  
  cc<- as.list(scan(text = input$mValueEntered, what = "", sep = ","))
  cc<-data.frame(mData=unlist(cc))
  
  df<<- df %>% add_row(
    Latitude=as.numeric(cc[1,1]),
    Longitude = as.numeric(cc[2,1]))
  
  rv$df <- df
  })
  

  
  output$mShowTbl <- DT::renderDataTable({
    DT::datatable(rv$df,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = TRUE,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
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
}

## 4. Run app
## ----------

shinyApp(ui, server)