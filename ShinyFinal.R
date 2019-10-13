## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)

Uber = read.csv("/home/chen1614/Uber.csv")
Lyft = read.csv("/home/chen1614/Lyft.csv")
Distance = read.csv("/home/chen1614/Distance.csv", header = T, row.names = 1)

Uber = Uber[,2:ncol(Uber)]
Lyft = Lyft[,2:ncol(Lyft)]

#Lyft = read.csv("/home/ku19/R/Lyft.csv")

################################################
set.seed(122)
library("h2o")
#install.packages("h2o")
#Create partition: tr and te 

h2o.init()
h2o.clusterInfo()

U = Uber[1:5,2:ncol(Uber)]
L = Lyft[1:5,2:ncol(Lyft)]
print(str(Uber))

#convert to h2o frame
UberH = as.h2o(Uber)
LyftH = as.h2o(Lyft)


set.seed(123)

y = "y"
x = setdiff(names(UberH),y)
parts = h2o.splitFrame(data = UberH, ratios = 0.7, seed = 99)
Utrain = parts[[1]]
Utest = parts[[2]]

x = setdiff(names(LyftH),y)
parts = h2o.splitFrame(data = LyftH, ratios = 0.7, seed = 99)
Ltrain = parts[[1]]
Ltest = parts[[2]]

#Ugb = h2o.gbm(x, y ,training_frame =  Utrain)
#Lgb = h2o.gbm(x,y, training_frame = Ltrain)


###
# Shiny Input
day = list("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunaday")
des = list("Back.Bay", "Beacon.Hill", "Boston.University", "Fenway", "Financial.District", 
           "Haymarket.Square", "North.End", "North.Station", "Northeastern.University", "South.Station", "Theatre.District", "West.End")
CarType = list("Uber.X...Lyft", "Uber.XL...Lyft.XL", "Uber.Pool...Shared", "Black...Lux.Black", "Black.SUV...Lux.Black.XL")
rain = list("Yes", "No")
Uber_type = data.frame("Uber.X...Lyft" = "UberX", "Uber XL / Lyft XL" = "UberXL", "Uber Pool / Shared" = "UberPool", "Black / Lux Black" = "Black", "Black SUV / Lux Black XL" = "Black.SUV")
Lyft_type = data.frame("Uber.X...Lyft" = "Lyft", "Uber XL / Lyft XL" = "Lyft.XL", "Uber Pool / Shared" = "Shared", "Black / Lux Black" = "Lux.Black", "Black SUV / Lux Black XL" = "Lux.Black.XL")
print(str(Lyft_type))
print(Lyft_type["Uber.X...Lyft"])

ui <- dashboardPage( skin = "green",
                     dashboardHeader(title = "Boston Uber | Lyft Price"),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Rider", tabName = "Rider", icon = icon("th")),
                         menuItem("Driver", tabName = "Driver", icon = icon("dashboard"))
                       )
                     ),
                     dashboardBody(
                       # Boxes need to be put in a row (or column)
                       tabItems(
                         # First tab content
                         tabItem(tabName = "Rider", h2("Rider"),
                                 fluidRow(
                                   box(title = "Price Trend of Uber and Lyft",
                                       
                                       plotOutput( outputId = "plot1", height = 370),
                                       textOutput( outputId = "RPrice"),
                                       textOutput( outputId = "RPrice2")
                                   ),
                                   
                                   
                                   box(
                                     title = "When to take the ride?",
                                     selectInput(inputId = "Rday", label = "Week Of Day", choices = day, selected = "Monday"),
                                     sliderInput("Rslider", "Time", 0, 23, 12)
                                     
                                   ),
                                   box(
                                     title = "Location",
                                     selectInput(inputId = "Rstar", label = "Pick up Location", choices = des, selected = "Back.Bay"),
                                     selectInput(inputId = "Rdes", label = "Destination", choices = des, selected = "Boston.University")
                                     
                                   ),
                                   box(
                                     selectInput(inputId = "RcarType", label = "Car Type", choices = CarType, selected = "Uber X / Lyft")
                                   ), 
                                   box(
                                     title = "Weather",
                                     selectInput(inputId = "Rrain", label = "Rain", choices = rain, selected = "No"),
                                     sliderInput("Rtemp", "Temperature", 19, 60, 40),
                                     sliderInput("Rhumd", "Humidity", 0.45, 0.99, 0.7),
                                     sliderInput("Rwind", "Wind", 0, 18, 6),
                                     sliderInput("Rpressure", "Pressure", 900, 1100, 1010),
                                     sliderInput("Rcloud", "Cloud", 0, 1, 0.7)
                                     
                                   )
                                   
                                   
                                 )
                         ),
                         
                         # Second tab content
                         tabItem(tabName = "Driver",
                                 h2("Driver"),
                                 box(title = "Demend of Uber and Lyft by Pick Up Location",
                                     plotOutput( "plot2", height = 600)),
                                 box(title = "Demand of Uber and Lyft by Destination",
                                     plotOutput( "plot3", height = 600))
                                 
                                 
                         )
                       )
                     )
)

server <- function(input, output) {
  
  ###########
  Ugb = h2o.xgboost(x, y ,training_frame =  Utrain)
  Lgb = h2o.xgboost(x,y, training_frame = Ltrain)
  ###########
  Data2 = reactive({
    data.frame(
      temp = input$Rtemp,
      clouds = input$Rcloud,
      humidity = input$Rhumd,
      pressure = input$Rpressure,
      wind = input$Rwind
    )
  })
  
  UData = reactive({
    U[1:5, 1:ncol(U)] = 0
    #print(paste0("car_type.", Uber_type[input$RcarType]))
    if(input$Rrain == "Yes"){
      U["rain"] = 1
    }
    else{
      U["rain"] = 0
    }
    U["temp"] = input$Rtemp
    U["clouds"] = input$Rcloud
    U["humidity"] = input$Rhumd
    U["pressure"] = input$Rpressure
    U["wind"] = input$Rwind
    Udes = paste0("destination.",input$Rdes)
    U[Udes] = 1
    USor = paste0("source.",input$Rstar)
    U[USor] = 1
    UH = paste0("hour.",input$Rslider-3)
    UH2 = paste0("hour.",input$Rslider-2)
    UH3 = paste0("hour.",input$Rslider-1)
    UH4 = paste0("hour.",input$Rslider)
    UH5 = paste0("hour.",input$Rslider+1)
    U[1, UH] = 1
    U[2, UH2] = 1
    U[3, UH3] = 1
    U[4, UH4] = 1
    U[5, UH5] = 1
    UD = paste0("Day.",input$Rday)
    UCarT = paste0("car_type.", Uber_type[1,input$RcarType])
    U[UCarT] = 1
    U[UD] = 1
    
    U["distance"] = Distance[input$Rdes, input$Rstar]
    
    return(as.h2o(U))
  })
  
  LData = reactive({
    L[1:5, 1:ncol(L)] = 0
    if(input$Rrain == "Yes"){
      L["rain"] = 1
    }
    else{
      L["rain"] = 0
    }
    L["temp"] = input$Rtemp
    L["clouds"] = input$Rcloud
    L["humidity"] = input$Rhumd
    L["pressure"] = input$Rpressure
    L["wind"] = input$Rwind
    Ldes = paste0("destination.",input$Rdes)
    L[Ldes] = 1
    LSor = paste0("source.",input$Rstar)
    L[LSor] = 1
    LH = paste0("hour.",input$Rslider-3)
    LH2 = paste0("hour.",input$Rslider-2)
    LH3 = paste0("hour.",input$Rslider-1)
    LH4 = paste0("hour.",input$Rslider)
    LH5 = paste0("hour.",input$Rslider+1)
    L[1, LH] = 1
    L[2, LH2] = 1
    L[3, LH3] = 1
    L[4, LH4] = 1
    L[5, LH5] = 1
    LD = paste0("Day.",input$Rday)
    print(paste0("car_type.", Lyft_type[1,input$RcarType]))
    LCarT = paste0("car_type.", Lyft_type[1,input$RcarType])
    L[LCarT] = 1
    L[LD] = 1
    
    L["distance"] = Distance[input$Rdes, input$Rstar]
    
    print(L)
    
    return(as.h2o(L))
  })
  
  Upredtion = reactive({
    #print(UData())
    #print(h2o.predict(Ugb, UData()))
    #print(as.vector(as.list(h2o.predict(Ugb, UData()))))
    #Ugb = h2o.gbm(x, y ,training_frame =  Utrain)
    return(as.vector(as.list(h2o.predict(Ugb, UData()))))
  })
  
  
  Lpredition = reactive({
    #Lgb = h2o.gbm(x, y ,training_frame =  Utrain)
    print(as.vector(as.list(h2o.predict(Lgb, LData()))))
    return(as.vector(as.list(h2o.predict(Lgb, LData()))))
    
  })
  
  output$plot1 <- renderPlot({
    a = input$Rslider
    
    print(paste0("car_type.", Lyft_type[input$RcarType]))
    print(paste0("car_type.", Uber_type[input$RcarType]))
    plot(c(a-2,a-1,a,a+1,a+2) , Lpredition(), xlab = "Time", ylab = "Price", type = "o", col = "pink", ylim=(c(5,45)) )
    points(c(a-2,a-1,a,a+1,a+2), Upredtion(), col="black", pch="*")
    lines(c(a-2,a-1,a,a+1,a+2), Upredtion(), col="black")
  })
  
  output$plot2 <- renderPlot({
    a = input$Rstar
    
    p1 = ggplot( data = data, aes( x = source)) + geom_bar()
    p1 = p1 + theme(axis.text = element_text(angle = 45))
    p1
    
    
  })
  
  output$plot3 <- renderPlot({
    a = input$Rstar
    
    p1 = ggplot( data = data, aes( x = destination)) + geom_bar()
    p1 = p1 + theme(axis.text = element_text(angle = 45))
    p1
    
    
  })
  
  
  
  
  output$RPrice = renderText({
    a = paste0("Uber Price is ", as.character(Upredtion()[3]))
    return(a)
  })
  output$RPrice2 = renderText({
    b = paste0("Lyft Price is ", as.character(Lpredition()[3]))
    return(b)
  })
  
}

shinyApp(ui, server)