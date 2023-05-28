# breaks are not unique means not enough data points (for quartiles)




library(shiny)
library(bslib)
library(tidyverse)
library(skimr)
library(sf)
library(leaflet)
library(RColorBrewer)

# setwd('/Users/joepopop/Desktop/GitHub/mychimyfuture')
merged_dat <- read_csv('Final_Merged_Dataset copy.csv')

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
  
  theme = bs_theme(version = 4, bootswatch = "lux", primary = "white", bg = '#f1f1fd',fg = 'grey', ),
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      # selectInput("variable",
      #             "Variable 1",
      #             choices = unique(dat %>%
      #                                select_if(is.numeric) %>%
      #                                select(-X1, -`Average Latitude`) %>%
      #                                names()),
      #             selected = 'Capacity Per Capita'
      # ),
      # selectInput("variable2",
      #             "Variable 2",
      #             choices = unique(dat %>%
      #                                select_if(is.numeric) %>%
      #                                select(-X1, -`Average Latitude`) %>% 
      #                                names()),
      #             selected = 'Hardship Index Score'
      # ),
      selectInput('stem',
                  'STEM vs Non-STEM',
                  choices = unique(c("No Filter", dat$Stem)),
                  selected = "No Filter"
      ),
      selectInput('free',
                  'Free vs Non-Free',
                  choices = unique(c("No Filter", dat$Free)),
                  selected = "No Filter"
      ),
      selectInput('meeting_type',
                  'Meeting Type',
                  choices = unique(c("No Filter", dat$`Meeting Type`)),
                  selected = "No Filter"
      ),
      p("The main goal of these maps is to show that there are many underserved community areas. The leftmost map shows median capacity per capita is well spread out, but the areas that need these programs the most are concentrated in the south and west, as shown in the middle map.  The rightmost map highlights underserved areas (high hardship; low capacity) in yellow and overserved (low hardship; high capacity) areas in grey.  Filtering programs by the metrics on the left sidebar allows us to see that the large presence of underserved areas is visible in specific programs as well.")
      
    ),
    mainPanel(
      fluidRow(
        splitLayout(
          cellWidths = c("50%", "50%"),
          leafletOutput("plot"),
          leafletOutput("plot2"),
          leafletOutput("plot3")
        ))
    )
  )
)


server <- function(input, output) {
  
  
  leaflet_dat <- reactive({
    
    dat <- dat %>%
      filter(
        (Stem == input$stem | input$stem == " " | input$stem == "No Filter"),
        (Free == input$free | input$free == " " | input$free == "No Filter"),
        (`Meeting Type` == input$meeting_type | input$meeting_type == " "
         | input$meeting_type == "No Filter")
      )
    
    
    
    
    
    test <- dat %>% 
      group_by(`Geographic Cluster Name`) %>% 
      mutate(`Capacity Per Capita` = median(`Capacity Per Capita`)) %>% 
      select(`Geographic Cluster Name`, `Capacity Per Capita`, Geometry) %>% 
      mutate(label = paste0(`Geographic Cluster Name`, ": ", `Capacity Per Capita`)) %>% 
      distinct() %>% 
      ungroup() %>% 
      st_as_sf()
    
    # Calculate quantile breakpoints
    breaks <- quantile(test$`Capacity Per Capita`, probs = c(0, 0.5, 1))
    
    # Assign quartile bins as factor levels
    test$quartile1 <- cut(test$`Capacity Per Capita`, breaks = breaks, labels = FALSE, include.lowest = TRUE)
    
    # Convert quartile column to factor with appropriate levels
    test$quartile1 <- factor(test$quartile1, levels = 1:4)
    
    test
    
  })
  
  # generate map
  output$plot <- renderLeaflet({
    
    leaflet_dat <- leaflet_dat()
    bins <- quantile(leaflet_dat$`Capacity Per Capita`, probs = c(0, 0.5, 1))
    pal <- colorBin(c('grey', '#D12D35'), domain = leaflet_dat$`Capacity Per Capita`, bins = bins)
    
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
        fillColor= ~pal(leaflet_dat$`Capacity Per Capita`), 
        fillOpacity = 0.7,
        color = "white",
        weight = 1.5,
        label = leaflet_dat$label
      ) %>%
      addScaleBar("bottomleft") %>% 
      addLegend(
        pal = pal, 
        values = leaflet_dat$`Capacity Per Capita`,
        opacity = 0.7, 
        title = 'Median Capacity Per Capita',
        position = "topright"
      )
    
    
    
    
  })
  
  leaflet_dat2 <- reactive({
    
    dat <- dat %>%
      filter(
        (Stem == input$stem | input$stem == " " | input$stem == "No Filter"),
        (Free == input$free | input$free == " " | input$free == "No Filter"),
        (`Meeting Type` == input$meeting_type | input$meeting_type == " "
         | input$meeting_type == "No Filter")
      )
    
    test2 <- dat %>% 
      group_by(`Geographic Cluster Name`) %>% 
      select(`Geographic Cluster Name`, `Hardship Index Score`, Geometry) %>% 
      mutate(label = paste0(`Geographic Cluster Name`, ": ", `Hardship Index Score`)) %>% 
      distinct() %>% 
      ungroup() %>% 
      st_as_sf() %>% 
      mutate(quartile2 = ntile(`Hardship Index Score`, 4))    
    
    
    # Calculate quantile breakpoints
    breaks <- quantile(test2$`Hardship Index Score`, probs = c(0, 0.5, 1))
    
    # Assign quartile bins as factor levels
    test2$quartile2 <- cut(test2$`Hardship Index Score`, breaks = breaks, labels = FALSE, include.lowest = TRUE)
    
    # Convert quartile column to factor with appropriate levels
    test2$quartile2 <- factor(test2$quartile2, levels = 1:4)
    
    test2
    
  })
  
  output$plot2 <- renderLeaflet({
    
    leaflet_dat2 <- leaflet_dat2()
    
    bins <- quantile(leaflet_dat2$`Hardship Index Score`, probs = c(0, 0.5, 1))
    pal <- colorBin(c('grey', '#1A18A0'), domain = leaflet_dat2$`Hardship Index Score`, bins = bins)
    
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
        fillColor= ~pal(leaflet_dat2$`Hardship Index Score`), 
        fillOpacity = 0.6,
        color = "white",
        weight = 1.5,
        label = leaflet_dat2$label
      ) %>%
      addScaleBar("bottomleft") %>% 
      addLegend(
        pal = pal, 
        values = leaflet_dat2$`Hardship Index Score`,
        opacity = 0.7, 
        title = 'Hardship Index Score',
        position = "topright"
      )
    
  })
  
  leaflet_dat3 <- reactive({
    leaflet_dat3 <- st_join(leaflet_dat(), leaflet_dat2(), join = st_within) 
    leaflet_dat3 %>% 
      filter(quartile1 != quartile2) %>% 
      mutate(quartile2 = if_else(quartile2==1, 'Overserved', 'Underserved'))
    
  })
  output$plot3 <- renderLeaflet({
    
    
    leaflet_dat3 = leaflet_dat3()
    
    pal <- colorFactor(c('grey', 'gold'), domain = leaflet_dat3$quartile2)
    
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
        data = leaflet_dat3,
        color = "white",
        fillColor =  ~pal(leaflet_dat3$quartile2), 
        fillOpacity = 0.55,
        weight = 1.5,
        label = paste0(str_extract(leaflet_dat3$label.x, '[A-Za-z ]*'), ": ", leaflet_dat3$quartile2)
      ) %>%
      addScaleBar("bottomleft") %>% 
      addLegend(
        pal = pal, 
        values = leaflet_dat3$quartile2,
        opacity = 0.7, 
        title = "Program Allocation",
        position = "topright"
      )
    
    
  })
  
  # output$table <- renderDataTable({
  # 
  #   leaflet_dat3()
  # 
  # }
  
  # )
  
}

shinyApp(ui = ui, server = server)
