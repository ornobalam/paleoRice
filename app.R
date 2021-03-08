# paleoRice shiny app

# load libraries

library(dplyr)
library(ggplot2)
library(tmap)
library(shiny)
library(shinyWidgets)
library(leaflet)


# read in data

sl <- readRDS("arch_database.rds")
map <- readRDS("map.rds")
temp <- readRDS("temperature_data.rds")

# create functions for filtering data according to inputs

data_filter <- function(subregion = ".*", ricedetails = ".*" , 
                        wetpaddy = ".*", domestication = ".*",
                        from = 15000, to = 0,
                        datetype = "Start Date"){
  int <- sl
  int %>% dplyr::filter(grepl(subregion,Subregion), grepl(ricedetails,`Rice details`),
                        grepl(wetpaddy, `Wet/Paddy`), grepl(domestication, Domestication) ) -> int
  int <- cut(int[,datetype], breaks=seq(-15000,2000,by = 500))
  int <- as.data.frame(table(int))
  int$proxy <-   seq(-14500,2000,by = 500)
  int <- int %>% dplyr::mutate(bp = abs(proxy -2000)) %>% dplyr::filter(bp <= (from -500), bp >= (to) )
  
  int
}


map_filter <- function(subregion = ".*", ricedetails = ".*" , wetpaddy = ".*", 
                       domestication = ".*", from = 15000, to = 0, 
                       datetype = "Start Date"){
  int <- sl
  int <- int %>% dplyr::mutate(for_filter = abs(`Start Date` - 2000 ) )
  int %>% dplyr::filter(grepl(subregion,Subregion), grepl(ricedetails,`Rice details`),
                        grepl(wetpaddy, `Wet/Paddy`), grepl(domestication, Domestication),
                        for_filter <= from, for_filter >= to) -> int
  se <- st_as_sf(int)
  se
}




# define server logic

server <- function(input, output) {
  
  options(warn = -1)
  
  # Compute formula texts
  
  formulaText_sr <- reactive({
    input$subregion
  })
  
  
  formulaText_wp <- reactive({
    input$wetpaddy
  })
  
  formulaText_rd <- reactive({
    input$ricedetails
  })
  
  formulaText_do <- reactive({
    input$domestication
  })
  
  formulaText_from <- reactive({
    input$range[1]
  })
  
  formulaText_to <- reactive({
    input$range[2]
  })
  

  # Generate slider and plots
  
  output$SliderText <- renderText({my_range()})
  
  output$archPlot <- renderPlot({
    ggplot(data = data_filter(subregion = formulaText_sr(),
                              wetpaddy = formulaText_wp(),
                              ricedetails = formulaText_rd(),
                              domestication = formulaText_do(),
                              from = formulaText_from(),
                              to = formulaText_to()),
           aes( x = bp)) +
      geom_col(aes(y = Freq), fill = "#38598CFF") +
      geom_line(data = temp, aes(y = (Temperature + 0.6) * scaleFactor , x = bp), 
                lwd = 1, col = "#C03A83FF") +
      geom_ribbon(data = temp, aes(ymin = (min + 0.6) * scaleFactor, 
                                   ymax = (max + 0.6) * scaleFactor, x = bp),
                  fill = "#C03A83FF", alpha = 0.1) +
      scale_y_continuous(
        name = "Sites with start dates in last 500 years",
        
        
        sec.axis = sec_axis( ~./scaleFactor - 0.6  , name="Temperature (\u00B0C)")
      ) + 
      xlab("Years BP") +
      scale_x_reverse(breaks = seq(0,15000, by = 2000), limits = c(15000,0), expand = c(0,0)) +
      theme(axis.title.y = element_text(size = 18),
            axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.text.y.right  = element_text(size = 14),
            axis.ticks.y.right = element_blank(),
            panel.grid.major.y = element_blank(),  
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x =  element_line( size=.5, color="darkgrey" , linetype = 3 ),
            panel.grid.minor.x =  element_blank(),
            axis.line.y = element_line() 
      )
  })
  
  md_out <- rmarkdown::render("popup.md")
  
  observeEvent(input$about, {
    
    showModal(modalDialog(
      title = "",
      renderUI(HTML(readLines(md_out))),
      easyClose = TRUE
    ))
  } )
  
  output$archMap <- renderLeaflet({
    
    tmap_mode("view")
    
    main <- map_filter()
    sites <- map_filter(subregion = formulaText_sr(),
                        wetpaddy = formulaText_wp(),
                        ricedetails = formulaText_rd(),
                        domestication = formulaText_do(),
                        from = formulaText_from(),
                        to = formulaText_to())
    
    map <- tm_shape(map, bbox = st_bbox(main)) +
      tm_borders(lwd = 1) +
      tm_fill(col = "name", palette = viridis_pal(option = "C")(20)[15:20], legend.show = F, 
              id = "name", popup.vars = F) +
      tm_shape(sites) +
      tm_symbols(size = 0.5, col = "#38598CFF", popup.vars = c("Name","Local/Alt Name",
                                                               "Province","Country", "Local Period Name",
                                                               "Start Date",
                                                               "Est. Date Median","Finish Date","Sample Quality",
                                                               "Dating","Evidence for rice","Cultivation",
                                                               "Evidence for cultivation",
                                                               "Domestication","Evidence for domestication",
                                                               "Wet/Paddy","Evidence for wet/paddy field",
                                                               "References"
      ))  +
      tm_basemap(NULL)
    
    tmap_leaflet(map, mode="view", show=T)
    
    
    
  })
  
}


# define user interface function


ui <- fluidPage(
  
  # App title
  
   titlePanel(
              
    actionButton("about", "",icon = icon("question")   )
  
   ),
   

  #titlePanel( title=div(img(src="logo.png"), "paleoRice") ),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      img(src="logo.png", height='120px',width='200px'),
      
      
      
      
      
      # Inputs
      
      selectInput("subregion", "Subregion:",
                  c("all" = ".*",
                    "China & Northeast Asia" = "China & Northeast Asia",
                    "Indian subcontinent" = "Indian subcontinent",
                    "Island Southeast Asia & Taiwan" = "Island Southeast Asia & Taiwan",
                    "Mainland Southeast Asia" = "Mainland Southeast Asia" )),
      
      selectInput("wetpaddy", "Show only wet/paddy:",
                  c("no" = ".*",
                    "yes" = TRUE)),
      
      selectInput("ricedetails", "Evidence for rice:",
                  c("all" = ".*",
                    "grains" = "gr",
                    "phytoliths" = "ph",
                    "spikelet bases" = "sb",
                    "Whole spikelet bases" = "spkt",
                    "imprints in pottery/brick" = "imp"
                  )),
      
      selectInput("domestication","Show only domesticated:",
                  c("yes" = "TRUE",
                    "no" = ".*")),
      sliderTextInput("range", "Years BP:",
                      choices = seq(from = 16000, to = 0, by = -1000), 
                      selected = c(9000,0), from_max = 0, from_min = 16000,
                      to_max = 0, to_min = 16000,
                      dragRange = T), 
      
     
      
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
   
    
      leafletOutput("archMap"),
      plotOutput("archPlot")
       
    )
  )
)

# run app

shinyApp(ui, server)







