# breaks are not unique means not enough data points (for quartiles)




library(shiny)
library(tidyverse)
library(skimr)
library(sf)
library(leaflet)
library(RColorBrewer)

merged_dat <- read_csv('/Users/joepopop/Desktop/GitHub/mychimyfuture/Final_Merged_Dataset.csv')

map_dat <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") %>% 
  rename(community_area = community) %>% 
  mutate(
    community_area = toupper(community_area),
    community_area = case_when(
    community_area == 'NEW CITY' ~ 'Back of the Yards',
    community_area %in% c('DOUGLAS', 'GRAND BOULEVARD', 'OAKLAND') ~ 'Bronzeville/South Lakefront',
    community_area %in% c('WEST GARFIELD PARK', 'EAST GARFIELD PARK') ~ 'GARFIELD PARK',
    community_area == 'SOUTH LAWNDALE' ~ 'Little Village',
    TRUE ~ community_area
    ))

# combined data
dat <- merged_dat %>% 
  left_join(map_dat, by = c('Geographic Cluster Name' = 'community_area'), multiple = 'all') %>% 
  mutate(
    `Geographic Cluster Name` = tools::toTitleCase(tolower(`Geographic Cluster Name`)),
    ) %>% 
  mutate_at(vars(contains("Percent") | contains("Unemployment Rate")),
            ~as.numeric(sub("%|\\$|,", "", .))/100) %>% 
  janitor::clean_names() %>% 
  rename_all(~ str_to_title(str_replace_all(., "_", " ")))


ui <- fluidPage(
  
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable",
                  "Select variable",
                  choices = dat %>%
                    select_if(is.numeric) %>%
                    select(-X1, -`Average Latitude`, -`Capacity Per Capita`) %>% 
                    names(),
                  selected = 'income'
                  ),
      selectInput('stem',
                  'STEM vs Non-STEM',
                  choices = unique(c(" ", dat$Stem)),
                  selected = character(0)
                  ),
      selectInput('free',
                  'Free vs Non-Free',
                  choices = unique(c(" ", dat$Free)),
                  selected = character(0)
      ),
      selectInput('meeting_type',
                  'Meeting Type',
                  choices = unique(c(" ", dat$`Meeting Type`)),
                  selected = character(0)
      ),
      checkboxInput('mean_median',
                    'Mean/Median',
                    value = FALSE)
      
    ),
    mainPanel(
      leafletOutput("plot"),
      leafletOutput("plot2"),
      textOutput("myText")
      )
  )
)


server <- function(input, output) {
  
  output$myText <- renderText({
    if_else(input$stem==" ", 'lol', 'lmao')
  })

  # generate map
  output$plot <- renderLeaflet({
  
    
    dat <- dat %>%
      filter(
        (Stem == input$stem | input$stem == " "),
        (Free == input$free | input$free == " "),
        (`Meeting Type` == input$meeting_type | input$meeting_type == " ")
      )
    
    leaflet_dat <- dat %>% 
      group_by(`Geographic Cluster Name`) %>% 
      mutate(`Capacity Per Capita` = if_else(input$mean_median, mean(`Capacity Per Capita`), median(`Capacity Per Capita`))) %>% 
      select(`Geographic Cluster Name`, `Capacity Per Capita`, Geometry) %>% 
      mutate(label = paste0(`Geographic Cluster Name`, ": ", `Capacity Per Capita`)) %>% 
      distinct() %>% 
      ungroup() %>% 
      st_as_sf()
    
    bins <- quantile(leaflet_dat$`Capacity Per Capita`, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal <- colorBin("YlOrRd", domain = leaflet_dat$`Mean Capacity Per Capita`, bins = bins)
    
    leaflet() %>%
      setView(lng = -87.69, lat = 41.87, zoom = 10) %>%
      addProviderTiles(
        "OpenStreetMap",
        group = "OpenStreetMap"
      ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) %>%
      addPolygons(
        data = leaflet_dat,
        fillColor= ~pal(`Capacity Per Capita`),
        fillOpacity = 0.2,
        color = "grey",
        weight = 1.5,
        label = leaflet_dat$label
      ) %>%
      addScaleBar("bottomleft") %>%
      addLegend(
        pal = pal,
        values = leaflet_dat$`Capacity Per Capita`,
        opacity = 0.7,
        title = paste(if_else(input$mean_median, 'Mean', 'Median'), "Capacity Per Capita"),
        position = "topright"
      )
    
    

    
    
  })
  
  output$plot2 <- renderLeaflet({
    
    dat <- dat %>%
      filter(
        (Stem == input$stem | input$stem == " "),
        (Free == input$free | input$free == " "),
        (`Meeting Type` == input$meeting_type | input$meeting_type == " ")
      )
    
    leaflet_dat2 <- dat %>% 
      group_by(`Geographic Cluster Name`) %>% 
      
      mutate(!!input$variable := if_else(input$mean_median, mean(.data[[input$variable]]), median(.data[[input$variable]]))) %>% 
      select(`Geographic Cluster Name`, !!input$variable, Geometry) %>% 
      mutate(label = paste0(`Geographic Cluster Name`, ": ", .data[[input$variable]])) %>% 
      distinct() %>% 
      ungroup() %>% 
      st_as_sf()
    
    bins <- quantile(leaflet_dat2[[input$variable]], probs = c(0, 0.25, 0.5, 0.75, 1))
    pal <- colorBin("YlOrRd", domain = leaflet_dat2[[input$variable]], bins = bins)
    
    leaflet() %>% 
      setView(lng = -87.69, lat = 41.87, zoom = 10) %>% 
      addProviderTiles(
        "OpenStreetMap",
        group = "OpenStreetMap"
      ) %>% 
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) %>% 
      addPolygons(
        data = leaflet_dat2,
        fillColor= ~pal(leaflet_dat2[[input$variable]]), 
        fillOpacity = 0.2,
        color = "grey",
        weight = 1.5,
        label = leaflet_dat2$label
      ) %>%
      addScaleBar("bottomleft") %>% 
      addLegend(
        pal = pal, 
        values = leaflet_dat2[[input$variable]],
        opacity = 0.7, 
        title = paste(if_else(input$mean_median, 'Mean', 'Median'), input$variable),
        position = "topright"
      )
    
  })
  
}

shinyApp(ui = ui, server = server)
