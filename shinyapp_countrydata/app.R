require(gdata)
library(readxl)
require(corrplot)
library(caret)
library(gridExtra)
if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/factoextra")
library(factoextra)
if(!require(rworldmap)) install.packages("rworldmap")
library(rworldmap)
if(!require(rworldxtra)) install.packages("rworldxtra")
library(rworldxtra)
library(shiny)
library(rsconnect)

# loading data into dataframes
df <- read_excel("CountryData.xlsx")
namesDF <- read_excel("CountryData.xlsx", sheet = 'seriesLabels')
givenNames <- namesDF[,c(3)]
givenNames <- unlist(givenNames)
df<- df[c(1:21)]
names(df)[3:21] <- givenNames
head(df,5)

set.seed(0)
# we impute missing values with a random forest
imputationModel <- preProcess(x = df[,-c(1,2)],method = "bagImpute")
imputatedData <- predict(object = imputationModel,newdata=df[,-c(1,2)])
imputatedData
length(imputatedData)
# Adding country names to the rows
countryNames <- df[,c(2)]
countryNames
newData  <- data.frame(countryNames,imputatedData)
dataColumns <- data.frame(newData[,c(-1)])

# Scaling the data and making sure country columns
scaledData = scale(dataColumns)
data <- data.frame(scaledData)
row.names(data) <- unlist(df[,c(2)])
data

ui <- fluidPage(
  verticalLayout(
    titlePanel("Country Cluster Analysis"),
    wellPanel(
      sliderInput("clusters", "Select Number of Clusters", min=2, max=9,
                  value = 3, step = 1)
    ),
    wellPanel(plotOutput("clusterPlot")),
    wellPanel(plotOutput("countryMapping"))
  )
)

server <- function(input, output) {  
  
  km.res <- reactive({
    kmeans(scaledData, input$clusters, nstart = 50)
  })
  
  output$clusterPlot <- renderPlot({
    
    
    fviz_cluster(km.res(), 
                 
                 data = data,
                 
                 palette= c("#4DAF4A", "#984EA3",
                            "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"), 
                 
                 ellipse.type = "euclid", 
                 
                 star.plot = TRUE, # Add segments from centroids to items
                 
                 repel = TRUE, # Avoid label overplotting (slow)
                 
                 ggtheme = theme_minimal()
                 
    )   
    
  })
  
  output$countryMapping <- renderPlot({
    
    
    cluster = as.numeric(km.res()$cluster)
    par(mfrow=c(1,1))
    spdf = joinCountryData2Map(data.frame(cluster,df$CountryName), joinCode="NAME", nameJoinColumn="df.CountryName",verbose = TRUE,mapResolution = "low")
    mapCountryData(spdf, nameColumnToPlot="cluster", catMethod="fixedWidth",colourPalette=c("#4DAF4A", "#984EA3",
                                                                                            "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"), addLegend = FALSE, lwd = 0.5)
    
  })
  
  
}

shinyApp(ui=ui, server=server) 