#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(
  
  function(input, output) {
  
    library(maptools)
    library(akima)
    
    pt.shp <- readShapePoints("ahearn_allobs/allobs")
     
  output$map <- renderPlot({
    
    pt <- switch(input$var.date, 
                 "Feb 16, 2005" = subset(pt.shp, DateTime_ == "2005-02-16"),
                 "Feb 17, 2005" = subset(pt.shp, DateTime_ == "2005-02-17"),
                 "Feb 18, 2005" = subset(pt.shp, DateTime_ == "2005-02-18"),
                 "Feb 23, 2005" = subset(pt.shp, DateTime_ == "2005-02-23"),
                 "Feb 28, 2005" = subset(pt.shp, DateTime_ == "2005-02-28"))
    
    pt.filled <- switch(input$var.var,
                        "Temperature" = interp(pt$X,pt$Y,pt$Temp,duplicate='mean'),
                        "Chlorophyl" = interp(pt$X,pt$Y,pt$Chlorophyl,duplicate='mean'),
                        "Dissolved Oxygen" = interp(pt$X,pt$Y,pt$DO__,duplicate='mean'),
                        "TDS" = interp(pt$X,pt$Y,pt$TDS,duplicate='mean'),
                        "Turbidity" = interp(pt$X,pt$Y,pt$Turbidity_,duplicate='mean'))
    
    pt.colors <- switch(input$var.var,
                        "Temperature" = cm.colors(24),
                        "Chlorophyl" = topo.colors(24),
                        "Dissolved Oxygen" = heat.colors(24),
                        "TDS" = terrain.colors(24),
                        "Turbidity" = topo.colors(24))
    
    pt.title <- switch(input$var.var,
                        "Temperature" = "Temperature [C]",
                        "Chlorophyl" = "Chlorophyl [mg/L]",
                        "Dissolved Oxygen" = "Dissolved Oxygen [mg/L]",
                        "TDS" = "TDS [mg/L]",
                        "Turbidity" = "Turbidity [NTU]")
    
    image(pt.filled, col=pt.colors,xlab="UTM X (m)",ylab="UTM Y (m)", main=pt.title)
    contour(pt.filled, add=T)
  })
  
})
