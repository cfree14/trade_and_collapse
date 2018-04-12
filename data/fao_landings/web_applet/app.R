

# Clear workspace
rm(list = ls())

# SETUP
################################################################################

# Run this when testing
# setwd("/Users/cfree/Dropbox/Chris/Rutgers/projects/trade_and_collapse/data/fao_landings/web_applet")

# Packages
library(plyr)
library(dplyr)
library(shiny)
library(shinyjs)
library(shinythemes)
library(RColorBrewer)

# Load catch data
load("data/1950_2017_FAO_landings_data_use.Rdata")
data$tl <- data$tl / 1000

# Read B/BMSY data
bbmsy <- read.csv("data/1950_2017_FAO_bbmsy_timeseries_full.csv", as.is=T)

# Major fishing areas
areas <- sort(unique(stocks$fao_area))

# Add nice stock id
stocks <- stocks %>% 
  mutate(stockid1=paste(fao_area, comm_name))
anyDuplicated(stocks$stockid1)

# Initial selection
area_init <- "Atlantic, Northwest"
# stocks_init <- stocks$comm_name[stocks$fao_area==area_init]
# select_init <- "Atlantic cod"

# INTERFACE CODE
################################################################################

# Parameters
horiz_line_style <- "border:solid 2px black"

# Interface
ui <- fluidPage(
  
  # Set theme
  # CSS Stylesheet: https://bootswatch.com/flatly/
  # Making new stylesheet: http://shiny.rstudio.com/articles/css.html
  theme=shinytheme("flatly"),
  
  # Page title
  titlePanel("FAO Stock Status Explorer"),
  
  # Page description
  HTML("<p>We estimated the status of 824 global fish and invertebrate stocks using FAO catch data and a recently developed catch-only superensemble model. Status estimates from the superensemble model (black line) are based on status estimates from four individual catch-only assessment models (colored lines and shading). To view the status of a stock, select a FAO area then a stock in the left-hand panel below.</p>"),
  hr(style=horiz_line_style),
  # br(),
  
  # Setup sidebar layout
  sidebarLayout(
    
    # Side panel: Select stock and plot catch
    sidebarPanel(
      
      # Area selector
      selectInput("selectArea",
                  "Select FAO area:",
                  choices=areas, selected=area_init,
                  multiple=F, selectize=T),
      
      # Stock selector
      selectInput("selectStock",
                  "Select stock from area:",
                  choices=NULL, 
                  multiple=F, selectize=T)
      
      # FAO fishing area map
      # img(src="fao_fishing_areas.jpg", align="left", width="100%")
      
    ),
    
    # Main panel: Plot status
    mainPanel(
      
      # Plot status
      plotOutput("statusPlot")
     
    )
    
  ),
  
  # Add methods
  hr(style=horiz_line_style),
  h4("Methods"),
  HTML("<p style='font-size:13px'>We constructed time series of stock status (B/B<sub>MSY</sub>) for 824 FAO fish stocks using a recently developed superensemble model (Anderson et al. 2017) that estimates B/B<sub>MSY</sub> from the B/B<sub>MSY</sub> predictions of four individual catch-only models and two spectral properties of the catch time series. This superensemble model has been shown to generate better predictions of status than other catch-only models (Anderson et al. 2017; Jensen & Free 2017) and was used to estimate the terminal year status of 785 FAO fish stocks (Rosenberg et al. 2017). We extended the analysis of Rosenberg et al. (2017) to estimate status from 1950-2015 for the 824 FAO fish stocks (area-species couples) meeting the following criteria: marine wild capture fisheries for finfish and invertebrates with taxonomic identification resolved to the species-level and with catch time series ≥20 yrs and ≥1000 mt of median annual catch.</p>

       <p style='font-size:13px'>For each stock, we used the superensemble model to estimate B/B<sub>MSY</sub> every year (year i) from the 15th year to the terminal year of the catch time series using: (1) the 0.20 and 0.05 spectral densities of the scaled catch time series (catch divided by maximum catch) from year 0 to year i and (2) B/B<sub>MSY</sub> predictions for year i from the four individual catch-only models applied to the full catch time series. We calculated the 0.20 and 0.05 spectral densities, which correspond to 5- and 20-year cycles respectively, using the spec.ar function in base R. We implemented the four individual catch-only models -- cMSY, SSCOM, COMSIR, and mPRM -- using the datalimited package in R (Anderson et al. 2016). In addition to catch time series, mPRM requires the classification of species into 17 life history categories and the other three models require estimates of resilience (i.e., the capacity to withstand exploitation). We classified life history based on taxonomy and derived resilience estimates from FishBase (Froese & Pauly 2017) and SeaLifeBase (Palomares & Pauly 2017) life history information.
       </p>"),
  
  # Add references
  hr(style=horiz_line_style),
  h4("References"),
  HTML("<p style='font-size:13px'><strong><u>Superensemble model (Anderson et al. 2017):</u></strong> Anderson, S.C, Cooper, A.B., Jensen, O.P., Minto, C., Thorson, J.T., Walsh, J.C., Afflerbach, J., Dickey-Collas, M., Kleisner, K.M., Longo, C., Osio, G.C., Ovando, D., Mosqueira, I., Rosenberg, A.A., Selig, E.R. (2017) Improving estimates of population status and trend with superensemble models. <i>Fish & Fisheries</i> 18(4): 732-741.</p>
       <p style='font-size:13px'><strong><u>Application to FAO stocks (Rosenberg et al. 2017):</u></strong> Rosenberg, A.A., Kleisner, K.M., Afflerbach, J., Anderson, S.C., Dickey‐Collas, M., Cooper, A.B., Fogarty, M.J., Fulton, E.A., Gutiérrez, N.L., Hyde, K.J.W., Jardim, E., Jensen, O.P., Kristiansen, T., Longo, C., Minte-Vera, C.V., Minto, C., Mosqueira, I., Chato Osio, G., Ovando, D., Selig, E.R., Thorson, J.T., Walsh, J.C., Ye, Y. (2017) Applying a new ensemble approach to estimating stock status of marine fisheries around the world. <i>Conservation Letters</i>: doi: 10.1111/conl.12363</p>")

)


# SERVER CODE
################################################################################

# Server
server <- function(input, output, session) {
  
  # Update stock selection based on selected area
  observeEvent(input$selectArea, {
    area <- input$selectArea
    stocks_area <- sort(stocks$comm_name[stocks$fao_area==area])
    updateSelectInput(session, "selectStock", choices=stocks_area)
  })
  
  # Plot status: stock <- "21ANG"
  output$statusPlot <- renderPlot({
    
    # Setup plots
    layout(matrix(data=c(1,1,2,3), nrow=2), width=c(0.7, 0.3))
    par(mar=c(4,5,0.3,0.5), mgp=c(3.2,1,0))
    
    # Get data
    area <- input$selectArea
    comm_name <- input$selectStock
    stockid1 <- paste(area, comm_name)
    stock <- stocks$stockid[stocks$stockid1==stockid1]
    cdata <- subset(data, stockid==stock)
    bbmsy1 <- subset(bbmsy, stockid==stock & !is.na(bbmsy_q50))

    # Plot status
    ####################################################
    
    # Setup empty plot
    xmin <- floor(min(cdata$year)/10)*10 
    xmax <- ceiling(max(cdata$year)/10)*10 
    ymax <- pmax(1.5, max(bbmsy1$bbmsy_q97.5, na.rm=T))
    plot(tl ~ year, cdata, bty="n", las=1, type="n", cex.lab=1.5, cex.axis=1.5,
         xlim=c(xmin, xmax), ylim=c(0, ymax), xlab="Year", ylab=expression("B / B"["MSY"]))
    
    # Loop through individual COMs and plot B/BMSY predictions
    coms <- c("mPRM", "cMSY-13", "COMSIR")
    com_names <- c("mPRM", "cMSY", "COMSIR", "SSCOM")
    colors <- brewer.pal(4, "Set1")
    tcolors <- rgb(t(col2rgb(colors))/255, alpha=0.4)
    for(i in 1:length(coms)){
      com <- coms[i]
      preds <- subset(bbmsy1, method==com)
      polygon(x=c(preds$year, rev(preds$year)), 
              y=c(preds$bbmsy_q2.5, rev(preds$bbmsy_q97.5)), col=tcolors[i], border=F)
      lines(x=preds$year, y=preds$bbmsy_q50, col=colors[i])
    }
    
    # Add lines for B/BMSY target and limit
    lines(x=c(xmin, xmax), y=c(0.5, 0.5), lty=3)
    lines(x=c(xmin, xmax), y=c(1, 1), lty=2)
    
    # Add legend
    ####################################################
    
    plot(1:10, 1:10, bty="n", type="n", xaxt="n", yaxt="n", xlab="", ylab="")
    legend(x=-4, y=10, legend=com_names, fill=tcolors, bty="n", cex=1.7,
           title=expression(bold("Assessment model")), xpd=NA)
    
    # Plot status
    ####################################################
    
    # Plot catch time series: stock <- "21ANG"
    sdata <- subset(data, stockid==stock)
    xmin <- floor(min(sdata$year)/10)*10 
    xmax <- ceiling(max(sdata$year)/10)*10
    ymax <- ceiling(max(sdata$tl)/10)*10
    plot(tl ~ year, sdata, bty="n", las=2, type="l", cex.lab=1.4, cex.axis=1.4,
         xlim=c(xmin, xmax), ylim=c(0, ymax), xlab="", ylab="Catch (1000s mt)")

  })
  
}

# LAUNCH APPLET
################################################################################

# Package applet
app <- shinyApp(ui=ui, server=server)






