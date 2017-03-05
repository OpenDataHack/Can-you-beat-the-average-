country.data <- read.csv("country2.csv")
modes.of.transport <- c("Walking", "Cycle", "Driving", "Airplane", "Bus", "Train", "Tube")
cars.data <- read.csv("cars.csv")
dist <- function(from, to, mode) {
    if (mode == "Airplane") {
        library("ggmap")
        library("geosphere")
        distHaversine(geocode(from), geocode(to)) / 1000
    } else {
        library("gmapsdistance")
        gmapsdistance(gsub(" ", "+", from), 
                      gsub(" ", "+", to), 
                      mode = mode)$Distance / 1000
    }
}
Co2.dist <-  function(mode,dis=NULL,man=NULL,trans=NULL,fuel=NULL,year=NULL) {
    if(mode=="Walking") {
        co2e<-dis*200
    } else if(mode=="Cycle") {
        co2e<-dis*200
    }else if(mode=="Driving"){
        co2e<-cars.data[cars.data[,2]==man&cars.data[,3]==trans&cars.data[,4]==fuel&cars.data[,1]==year,5]
    } else if(mode=="Bus"){
        co2e<-dis*300
    }
    else if(mode=="Train"){
        co2e<-dis*300
    }
    else if(mode=="Tube"){
        co2e<-dis*300
    }
    else {
        if(dis<=500){
            co2e<-dis*0.16
        } else if(dis<=1000){
            co2e<-dis*0.13
        } else if(dis<=4000){
            co2e<-dis*0.11
        } else{
            co2e<-dis*0.14
        }
    }
    return(co2e)
}

# ui function -------------------------------------------------------------
ui <- dashboardPage(
    title = "Validation",
    
    header = dashboardHeader(title = "Carbon Footprint for your Journey", titleWidth = 700),
    
    sidebar = dashboardSidebar(disable = TRUE),
    
    body = dashboardBody(
        fluidRow(
            h4("Positive Outlook on your carbon footprint"),
            
            box(width = 6, title = "Select Your Country", status = "info", #background = "teal", 
                selectInput("selected.country", "Select Your Country", country.data$Country.Name),
                textInput("selected.from", "From", value = ""),
                textInput("selected.to", "To", value = ""),
                actionButton("calculate", label = "Calculate", icon = icon("calculator"))),
            
            box(width = 6, title = "Type of transport", status = "info", #background = "teal",  
                selectInput("transport", "Select Your Mode of Transport", modes.of.transport),
                conditionalPanel("input.transport == 'Driving'", 
                                 selectInput("car.manuf", "Select Your Car Manufacture", unique(cars.data$manufacturer)),
                                 uiOutput("carTrans"),
                                 uiOutput("carFuel"),
                                 uiOutput("carYear")
                ))),
        
        fluidRow(
            imageOutput("map.plot"),
            textOutput("text1")
        ), 
        
        fluidRow(
            textOutput("text2")
            
        )
        
    )
)

# Server function ---------------------------------------------------------
server <- function(input, output) {
    
    output$carTrans <- renderUI({
        selectInput("car.trans", "Select Your Car Transmission Type", sort(cars.data[cars.data$manufacturer == input$car.manuf, "transmission_type"]))
    })
    
    output$carFuel <- renderUI({
        selectInput("car.fuel", "Select Your Car Fuel Type", sort(cars.data[cars.data$manufacturer == input$car.manuf & 
                                                                                cars.data$transmission_type == input$car.trans, "fuel_type"]))
    })
    
    output$carYear <- renderUI({
        selectInput("car.year", "Select Your Car Year of Manufacturer", sort(cars.data[cars.data$manufacturer == input$car.manuf & 
                                                                                           cars.data$transmission_type == input$car.trans &
                                                                                           cars.data$fuel_type == input$car.fuel    , "year"]))
    })
    
    
    AvgCo2 <- reactive({
        country.data[country.data$Country.Name == input$selected.country, "co2"]
    })
    
    Dist <- reactive({
        dist(input$selected.from, input$selected.to, mode = input$selected.mode.transport)
    })
    
    # print(Dist())
    
    co2emission <- reactive({
        Co2.dist(input$selected.mode.transport, dis=Dist)
    })
    
    
     output$text1 <- renderText(paste0("This is your Average co2 emission", round(AvgCo2())))
     output$text2 <- renderText(paste0("This is your Average co2 emission", round(AvgCo2())))
                            # "and this is the distance", Dist(),
                       # "and this is your co2em", co2emission()))
                       # 
     # out
    # # observeEvent(input$calculate, 
    # #          
    # #  )
    
    output$map.plot <- renderImage({
        list(src = "Map.png")
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile("Map.png")
        
        png(outfile, width=800, height=600)
        mapParams <- mapCountryData(sPDF, nameColumnToPlot = "co2", colourPalette = rev(heat.colors(204)), numCats = 204)
        dev.off()
        
        # Return a list containing the filename
        list(src = outfile,
             contentType = 'image/png',
             width = 800,
             height = 600,
             alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    
    
}



runApp(list(ui = ui, server = server), launch.browser = TRUE)
shinyApp(ui, server)
