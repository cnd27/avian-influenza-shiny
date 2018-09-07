# Load libraries and data
library(RColorBrewer)
library(spam)
library(sp)
library(maps)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(dichromat)
library(fields)
load("data/shinyHUC.RData")
load("data/shinySAMPLES.RData")
load("data/shinyQ.RData")
load("data/shinyQ200.RData")
sampCol <-c("grey60", brewer.pal(5, "BuGn"))
sampCol200 <-c("grey60", colorRampPalette(brewer.pal(7, "BuGn"))(200))
prevCol <-c("grey20","grey60", brewer.pal(5, "OrRd"))
prevCol200 <-c("grey20","grey60", colorRampPalette(brewer.pal(7, "OrRd"))(200))
mord = c(4,5,6,7,8,9,10,11,12,1,2,3)
M1 <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
M3 <- c("Dec - Feb","Mar - May", "Jun - Aug", "Sep - Nov")
S <- c("Summer breeding", "Fall migration", "Over-wintering")
allY <- seq(2007,2017,1)
allM1 <- paste(M1, as.character(rep(seq(2007,2017,1),each=12)),sep = " ")[4:122]
allM3 <- paste(M3, as.character(rep(seq(2007,2016,1),each=4)),sep = " ")
allS <- paste(S,as.character(rep(seq(2007,2016,1),each=3)))
allM3[4] <- paste(allM3[4],"/08", sep = "")
allM3[8] <- paste(allM3[8],"/09", sep = "")
allM3[12] <- paste(allM3[12],"/10", sep = "")
allM3[16] <- paste(allM3[16],"/11", sep = "")
allM3[20] <- paste(allM3[20],"/12", sep = "")
allM3[24] <- paste(allM3[24],"/13", sep = "")
allM3[28] <- paste(allM3[28],"/14", sep = "")
allM3[32] <- paste(allM3[32],"/15", sep = "")
allM3[36] <- paste(allM3[36],"/16", sep = "")
allM3[40] <- paste(allM3[40],"/17", sep = "")
allS[3] <- paste(allS[3],"/08", sep = "")
allS[6] <- paste(allS[6],"/09", sep = "")
allS[9] <- paste(allS[9],"/10", sep = "")
allS[12] <- paste(allS[12],"/11", sep = "")
allS[15] <- paste(allS[15],"/12", sep = "")
allS[18] <- paste(allS[18],"/13", sep = "")
allS[21] <- paste(allS[21],"/14", sep = "")
allS[24] <- paste(allS[24],"/15", sep = "")
allS[27] <- paste(allS[27],"/16", sep = "")
allS[30] <- paste(allS[30],"/17", sep = "")
"%ni%" <- Negate("%in%")

ui <- tagList(dashboardPage(
  dashboardHeader(title = "Surveillance of Avian Influenza in the USA",titleWidth = 420,
                  dropdownMenuOutput("dropdownmenuh"),dropdownMenuOutput("dropdownmenu")
                  ), 
  dashboardSidebar(disable = TRUE),
  dashboardBody(useShinyjs(),
                tags$head(includeHTML("analytics.html")),
                tags$style(type = "text/css", "
      .irs-bar {background:#3c8dbc;border-top-color: #3c8dbc; border-bottom-color: #3c8dbc;}
      .irs-bar-edge {background:#3c8dbc;border-color: #3c8dbc;}
      .irs-single {background:#3c8dbc;font-size: 10pt;font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif;}
      .irs-grid-text {font-size: 8pt; color: black;font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif;}
      .irs-max {font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif; color: black; font-size: 10pt;}
      .irs-min {font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif; color: black; font-size: 10pt;}
    "), tags$head(tags$style(HTML('
      .form-group, .selectize-control {
           margin-bottom: 2px;
           margin-top: 2px;
      }
      .box-body {
          padding-bottom: 10px;
          padding-top: 2px;
          padding-right: 18px;
      }
     [class*="col-lg-"],[class*="col-md-"],
    [class*="col-sm-"],[class*="col-xs-"]{
    padding-right:7px;
    padding-left:13px;
    padding-bottom:0px;
    padding-top:0px;
    margin-bottom:0px
}
.content {
    padding: 15px;
padding-top: 10px;
padding-bottom: 0px;
}
'))),
                fluidRow(
                    column(10,
                           fluidRow(
                             box(collapsible = TRUE,title="Sample number and prevalence",solidHeader = TRUE,
                                 width = 12, status = "primary",
                                 fluidRow(
                                   column(6, 
                                          fluidRow(column(10,align = "center", h4(htmlOutput("timeTitle")))),
                                          plotOutput("SamplesMap",  click = "sampClick", height = "35vh"),
                                          plotOutput("PrevMap",  click = "sampClick", height = "35vh")
                                          ),
                                   column(6,
                                          fluidRow(align = "center", h4(htmlOutput("shedTitle"))),
                                          plotOutput("SamplesPlot", height = "35vh"),
                                          plotOutput("PrevPlot", height = "35vh")
                                          )
                                 ),
                                 fluidRow(align = "center", textOutput("slidertext")),
                                 fluidRow(column(10,offset=1,uiOutput("slider")))
                             ))
                           ),
                    column(2,
                           box(collapsible = TRUE,title=span(icon("wrench")," Options"),solidHeader = TRUE,
                               width = 12, status = "primary",
                               radioButtons("birdGroup", "Bird Group:", choices = c("All"=1,"Dabbling Ducks"=2,
                                                                                    "Diving Ducks"=3,"Anserinae"=4),
                                            selected = 1, inline = FALSE),
                               radioButtons("periodt", "Time interval:", choices = c("1 month"=1,"3 months"=2,"Season"=3, "Year" = 4),
                                            selected = 3, inline = FALSE),
                               radioButtons("averaget", "Compare by:", choices = c("Individual years"=1,"Over all years"=2),
                                            selected = 1, inline = FALSE),
                               actionButton("optionstext", "See more options"),
                               htmlOutput("space"),
                               uiOutput("options"),
                               uiOutput("options2"),
                               uiOutput("options3")
                           )
                           )
                  )
  )
),
tags$footer("Web app created by Christopher Davis, University of Warwick.", align = "center", style = "
              width:100%;
              height:20px;   /* Height of the footer */
              color: white;
              background-color: #3c8dbc;")
)

server <- function(session, input, output) {
  
  output$dropdownmenu = renderMenu({
    dropdownMenu(
      type = "notifications", 
      icon = " About",
      badgeStatus = NULL,
      headerText = "This dashboard is a visualization of surveillance data for avian influenze in the USA.",
      notificationItem("Built using Shiny", icon = icon("external-link"),
                       href = "http://shiny.rstudio.com/")
    )
  })
  output$dropdownmenuh = renderMenu({
    dropdownMenu(
      type = "messages", 
      icon = span(icon("question-circle")," Help"),
      badgeStatus = NULL,
      headerText = "Loading help..."
    )
  })
  
  onclick("dropdownmenuh", showModal(modalDialog(
    title = "Help",
    "Choose a bird group, time interval and whether to consider each year individually
      or a sum over all years for the given interval. Then move the slider or press the
      play button to view the change in time.", br(), br(),

    "Clicking a watershed will display a histogram against time. Choose the 'see all' 
      box to view the average over all watersheds. The dashed red line marks
      the current time.", br(), br(),

    "Seasons are given by:",br(), "Summer breeding - May to August", br(),
    "Fall migration - August to December", br(), "Over-wintering - December to February",
    br(), br(), "Note that seasons are overlapping and so some data is duplicated in this view.")))
  
  output$slidertext <- renderText({
    if (as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) != 8){
      paste('Slide to change time:')
    }
  })
  
  output$timeTitle <- renderText({
    if (as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
      paste("<b>Total</b>")
    } else {
      paste("<b>Time: ", input$time, "</b>")
    }
    })
  
  output$shedTitle <- renderText({
    if (!is.na(sampShedName())){
      paste("<b>Watershed: ", sampShedName(), "</b>")
    } else if (TRUE){
      paste("<b>Watershed</b>")
    } else {
      paste("<b>Watershed</b>")
    }
  })

  output$space <- renderUI({
    if (input$optionstext %% 2){
      tags$div(HTML("<br>"))
    }
  })

  output$options <- renderUI({
    if (input$optionstext %% 2){
      radioButtons("colours", "Color display:", choices = c("Legend"=1,"Colorbar"=2),selected = 1)
    }
  })
  
  output$options2 <- renderUI({
    if (input$optionstext %% 2){
      checkboxInput("markers", "View place markers", value = TRUE)
    }
  })
  
  output$options3 <- renderUI({
    if (input$optionstext %% 2){
      checkboxInput("timeline", "View time line", value = TRUE)
      if (as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        disabled(checkboxInput("timeline", "View time line", value = TRUE))
      } else {
        checkboxInput("timeline", "View time line", value = TRUE)
      }
    }
  })
  
  daten <- reactive({(
    switch(as.integer(input$periodt) + 4*(as.integer(input$averaget)-1),
           which(input$time==allM1),
           which(input$time==allM3),
           which(input$time==allS),
           which(input$time==allY),
           which(input$time==M1),
           which(input$time==M3),
           which(input$time==S),
           NULL))})
  birdg <- reactive({as.integer(input$birdGroup)})
  birdgs <- reactive({8*(birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1)})
  
  mapSampCols <- reactive({
    if (length(input$colours)==0 || input$colours == 1){
      if((birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        sampCol[as.integer(cut(Samples[[birdgs()]],
                               breaks=c(0, Q[[birdgs()]]),include.lowest = TRUE, right = FALSE))]
      }
      else {
        sampCol[as.integer(cut(Samples[[birdgs()]][,daten()],
                               breaks=c(0, Q[[birdgs()]]),include.lowest = TRUE, right = FALSE))]
      }
    } else {
      if((birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        sampCol200[as.integer(.bincode(Samples[[birdgs()]],
                                       breaks=c(0, Q200[[birdgs()]]),include.lowest = TRUE, right = FALSE))]
      }
      else {
        sampCol200[as.integer(.bincode(Samples[[birdgs()]][,daten()],
                                       breaks=c(0, Q200[[birdgs()]]),include.lowest = TRUE, right = FALSE))]
      }
    }
  })

  histSampCols <- reactive({
    if (length(input$colours)==0 || input$colours == 1){
      if((birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        sampCol[as.integer(cut(Samples[[birdgs()]][sampShedID()],
                               breaks=c(0, Q[[birdgs()]]),include.lowest = TRUE, right = FALSE))]
      }
      else {
        sampCol[as.integer(cut(Samples[[birdgs()]][sampShedID(),],
                               breaks=c(0, Q[[birdgs()]]),include.lowest = TRUE, right = FALSE))]
      }
    } else {
      if((birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        sampCol200[as.integer(.bincode(Samples[[birdgs()]][sampShedID()],
                                       breaks=c(0, Q200[[birdgs()]]),include.lowest = TRUE, right = FALSE))]
      }
      else {
        sampCol200[as.integer(.bincode(Samples[[birdgs()]][sampShedID(),],
                                       breaks=c(0, Q200[[birdgs()]]),include.lowest = TRUE, right = FALSE))]
      }
    }
  })
  
  mapPrevCols <- reactive({
    if (length(input$colours)==0 || input$colours == 1){
      if((birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        cc <- prevCol[as.integer(cut(Samples[[birdgs()+32]],
                                     breaks=c(0,0.001,0.2,0.4,0.6,0.8,1.0),include.lowest = TRUE, right = FALSE))+1]}
      else {
        cc <- prevCol[as.integer(cut(Samples[[birdgs()+32]][,daten()],
                                     breaks=c(0,0.001,0.2,0.4,0.6,0.8,1.0),include.lowest = TRUE, right = FALSE))+1]}
    } else {
      if((birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        cc <- prevCol200[as.integer(cut(Samples[[birdgs()+32]],
                                        breaks=c(0,0.001,seq(0.005,1.0,0.005)),include.lowest = TRUE, right = FALSE))+1]}
      else {
        cc <- prevCol200[as.integer(cut(Samples[[birdgs()+32]][,daten()],
                                        breaks=c(0,0.001,seq(0.005,1.0,0.005)),include.lowest = TRUE, right = FALSE))+1]}
    }
    cc[is.na(cc)] <- prevCol[1]
    return(cc)
  })
  
  histPrevCols <- reactive({
    if (length(input$colours)==0 || input$colours == 1){
      if((birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        cc <- prevCol[as.integer(cut(Samples[[birdgs()+32]][sampShedID()],
                                     breaks=c(0,0.001,0.2,0.4,0.6,0.8,1.0),include.lowest = TRUE, right = FALSE))+1]}
      else {
        cc <- prevCol[as.integer(cut(Samples[[birdgs()+32]][sampShedID(),],
                                     breaks=c(0,0.001,0.2,0.4,0.6,0.8,1.0),include.lowest = TRUE, right = FALSE))+1]}
    } else {
      if((birdg()-1)+as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
        cc <- prevCol200[as.integer(cut(Samples[[birdgs()+32]][sampShedID()],
                                        breaks=c(0,0.001,seq(0.005,1.0,0.005)),include.lowest = TRUE, right = FALSE))+1]}
      else {
        cc <- prevCol200[as.integer(cut(Samples[[birdgs()+32]][sampShedID(),],
                                        breaks=c(0,0.001,seq(0.005,1.0,0.005)),include.lowest = TRUE, right = FALSE))+1]}
    }
    cc[is.na(cc)] <- prevCol[1]
    return(cc)
  })
  
  source_coords <- reactiveValues(xy=data.frame(x=c(1,1),  y=c(1,1)))
  
  observeEvent(input$sampClick, {
    source_coords$xy[2,] <- c(input$sampClick$x, input$sampClick$y)
  })  

  sampShed <- reactive({
    d <- data.frame(x=source_coords$xy[2,1],y=source_coords$xy[2,2])
    xy <- d[,c(1,2)]
    s <- SpatialPointsDataFrame(coords = xy, data = d,
                                   proj4string = CRS("+proj=utm +zone=14 +datum=WGS84"))
    proj4string(s) <- CRS("+proj=utm +zone=14 +datum=WGS84")
    proj4string(huc4plot) <- CRS("+proj=utm +zone=14 +datum=WGS84")
    indata <- over(s,huc4plot)
    return(indata)
    })
  
  sampShedID <- reactive({
    ss <- sampShed()
    return(match(ss$OBJECTID,huc4plot$OBJECTID))
  })
  
  sampShedName <- reactive({
    ss <- sampShed()
    return(ss$NAME)
  })
  
  sampShedcoords <- reactive({
    ss <- sampShed()
    if (!is.na(sampShed()[1])){
      return(huc4plot@polygons[[which(huc4plot$OBJECTID==ss$OBJECTID)]]@labpt)
    }
    else{
      c(source_coords$xy[2,1],source_coords$xy[2,2])
    }
  })

  output$SamplesMap <- renderPlot({
    op <- par(mar = c(0,0,0,0))
    plot(huc4plot,col=mapSampCols(),xlim=c(-1e6,4e6),
         ylim=c(2900000,6000000))
    if (length(input$colours)==0 || input$colours == 1){
      if (birdgs() %ni% c(17,18,19,20,21,22,23)){
        legend("right",title = "Number of samples:",
               legend = c("0", paste(as.character(Q[[birdgs()]][1]),"-",as.character(Q[[birdgs()]][2]-1)),
                          paste(as.character(Q[[birdgs()]][2]),"-",as.character(Q[[birdgs()]][3]-1)),
                          paste(as.character(Q[[birdgs()]][3]),"-",as.character(Q[[birdgs()]][4]-1)),
                          paste(as.character(Q[[birdgs()]][4]),"-",as.character(Q[[birdgs()]][5]-1)),
                          paste(as.character(Q[[birdgs()]][5]),"-",as.character(Q[[birdgs()]][6]))), fill=sampCol)
      } else if (birdgs() == 17){
        legend("right",title = "Number of samples:",
               legend = c("0", paste(as.character("1")),
                          paste(as.character("2")),
                          paste(as.character((Q[[birdgs()]][3])),"-",as.character((Q[[birdgs()]][4]-1))),
                          paste(as.character((Q[[birdgs()]][4])),"-",as.character((Q[[birdgs()]][5]-1))),
                          paste(as.character((Q[[birdgs()]][5])),"-",as.character((Q[[birdgs()]][6])))), fill=sampCol)
      } else {
        legend("right",title = "Number of samples:",
               legend = c("0", paste(as.character("1")),
                          paste(as.character((Q[[birdgs()]][2])),"-",as.character((Q[[birdgs()]][3]-1))),
                          paste(as.character((Q[[birdgs()]][3])),"-",as.character((Q[[birdgs()]][4]-1))),
                          paste(as.character((Q[[birdgs()]][4])),"-",as.character((Q[[birdgs()]][5]-1))),
                          paste(as.character((Q[[birdgs()]][5])),"-",as.character((Q[[birdgs()]][6])))), fill=sampCol)
      }
    } else {
      image.plot(legend.only=TRUE,zlim=c(1,240), col=c(rep("grey60",40),sampCol200[2:201]), 
                 axis.args = list(at=seq(0.5,240.5,length.out = 7), labels=c(0,Q[[birdgs()]])),
                 legend.lab = "Number of samples",legend.line = 2.9)
    }
    if (is.null(input$markers)){
      points(sampShedcoords()[1],sampShedcoords()[2], col = 'red', pch = 4, cex = 1.8, lwd = 3)
    } else if (input$markers){
      points(sampShedcoords()[1],sampShedcoords()[2], col = 'red', pch = 4, cex = 1.8, lwd = 3)
    } else {
    }
    par(op)
  })
  
  output$PrevMap <- renderPlot({
    op <- par(mar = c(0,0,0,0))
    plot(huc4plot,col=mapPrevCols(),xlim=c(-1e6,4e6),
         ylim=c(2900000,6000000),main=NULL)
    if (length(input$colours)==0 || input$colours==1){
      legend("right",title = "Prevalence (%):",
             legend = c("N/A", "0","0 - 20","20 - 40","40 - 60","60 - 80","80 - 100"), fill=prevCol)
    } else {
      image.plot(legend.only=TRUE,zlim=c(1,280), col=c(rep("grey20",40),rep("grey60",40),prevCol200[3:202]), 
                 axis.args = list(at=seq(0.5,280.5,length.out = 8), labels=c("N/A",0,0.1,20,40,60,80,100)),
                 legend.lab = "Prevalence (%)",legend.line = 2.9)
    }
    if (is.null(input$markers)){
      points(sampShedcoords()[1],sampShedcoords()[2], col = 'red', pch = 4, cex = 1.8, lwd = 3)
    } else if (input$markers){
      points(sampShedcoords()[1],sampShedcoords()[2], col = 'red', pch = 4, cex = 1.8, lwd = 3)
    } else {
    }
    par(op)
  })
  
  output$SamplesPlot <- renderPlot({
    op <- par(mar = c(3.5,4,1,1))
    if (is.na(sampShedName())){
      if (input$periodt == 4){
        if (input$averaget == 2){
          barplot(0,main = NULL,xlab = "", ylab = "Number of samples",ylim = c(0,10.5), 
                  space = 0, names.arg = c("Total"))
          axis(1, at=c(0,1), labels=FALSE, tick = TRUE)
          text(0.45,5,"Click on the map to select a watershed")
        } else {
          barplot(rep(0,length(tvals())),main = NULL,xlab = "", ylab = "Number of samples",
                  space = 0, col = histSampCols(), ylim = c(0,10.5), names.arg = tvals(), xaxt = "n")
          axis(1, at=c(seq(0.5,10.5,1)), labels=seq(2007,2017,1), tick = FALSE)
          axis(1, at=c(seq(0,11,1)), labels=FALSE, tick = TRUE)
          title(xlab = "Time", line=2.5)
          text(0.45*length(tvals()),5,"Click on the map to select a watershed")
        }
      } else {
        if (input$averaget == 1){
          barplot(rep(0,length(tvals())),main = NULL,xlab = "", ylab = "Number of samples",
                  space = 0, col = histSampCols(), ylim = c(0,10.5), names.arg = tvals(), xaxt = "n")
          if (input$periodt == 3) {
            axis(1, at=c(4/3,seq(25/6,30,3)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(8/3,30,3)), labels=FALSE, tick = TRUE)
          } else if (input$periodt == 2){
            axis(1, at=c(5/3,seq(16/3,40,4)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(10/3,40,4)), labels=FALSE, tick = TRUE)
          } else {
            axis(1, at=c(4.5,seq(15,120,12)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(9,120,12)), labels=FALSE, tick = TRUE)
          }
        } else {
          barplot(rep(0,length(tvals())),main = NULL,xlab = "", ylab = "Number of samples",
                  space = 0, col = histSampCols(), ylim = c(0,10.5), names.arg = tvals())
          if (input$periodt == 3) {
            axis(1, at=c(seq(0,3,1)), labels=FALSE, tick = TRUE)
          } else if (input$periodt == 2){
            axis(1, at=c(seq(0,4,1)), labels=FALSE, tick = TRUE)
          } else {
            axis(1, at=c(seq(0,12,1)), labels=FALSE, tick = TRUE)
          }
        }
        title(xlab = "Time", line=2.5)
        text(0.45*length(tvals()),5,"Click on the map to select a watershed")
      }
    } else {
      if (input$periodt == 4){
        if (input$averaget == 2){
          if (Samples[[birdgs()]][sampShedID()]>0){
            barplot(Samples[[birdgs()]][sampShedID()],main = NULL,xlab = "", ylab = "Number of samples",
                    space = 0, col = histSampCols(), names.arg = c("Total"), ylim = c(0,round(1.1*Samples[[birdgs()]][sampShedID()])))
            axis(1, at=c(0,1), labels=FALSE, tick = TRUE)
          } else {
            barplot(0,main = NULL,xlab = "", ylab = "Number of samples",ylim = c(0,10.5), names.arg = c("Total"))
            axis(1, at=c(0,1), labels=FALSE, tick = TRUE)
          }
        } else {
          if (max(Samples[[birdgs()]][sampShedID(),])>0){
            barplot(Samples[[birdgs()]][sampShedID(),],main = NULL,xlab = "", ylab = "Number of samples",
                    space = 0, col = histSampCols(), names.arg = tvals(), xaxt = "n", ylim = c(0,round(1.1*max(Samples[[birdgs()]][sampShedID(),]))))
            axis(1, at=c(seq(0.5,10.5,1)), labels=seq(2007,2017,1), tick = FALSE)
            axis(1, at=c(seq(0,11,1)), labels=FALSE, tick = TRUE)
            lines(c(0,11),c(0,0))
          } else {
            barplot(Samples[[birdgs()]][sampShedID(),],main = NULL,xlab = "", 
                    ylab = "Number of samples",ylim = c(0,10.5),space = 0, xaxt = "n")
            axis(1, at=c(seq(0.5,10.5,1)), labels=seq(2007,2017,1), tick = FALSE)
            axis(1, at=c(seq(0,11,1)), labels=FALSE, tick = TRUE)
            lines(c(0,11),c(0,0))
          }
        }
      } else {
        if (max(Samples[[birdgs()]][sampShedID(),])>0){
          if (input$averaget == 1){
            barplot(Samples[[birdgs()]][sampShedID(),],main = NULL,xlab = "", ylab = "Number of samples",
                    space = 0, col = histSampCols(), names.arg = tvals(), xaxt = "n", ylim = c(0,round(1.1*max(Samples[[birdgs()]][sampShedID(),]))))
            if (input$periodt == 3) {
              axis(1, at=c(4/3,seq(25/6,30,3)), labels=seq(2007,2016,1), tick = FALSE)
              axis(1, at=c(seq(8/3,30,3)), labels=FALSE, tick = TRUE)
              lines(c(0,30),c(0,0))
            } else if (input$periodt == 2){
              axis(1, at=c(5/3,seq(16/3,40,4)), labels=seq(2007,2016,1), tick = FALSE)
              axis(1, at=c(seq(10/3,40,4)), labels=FALSE, tick = TRUE)
              lines(c(0,40),c(0,0))
            } else {
              axis(1, at=c(4.5,seq(15,120,12)), labels=seq(2007,2016,1), tick = FALSE)
              axis(1, at=c(seq(9,120,12)), labels=FALSE, tick = TRUE)
              lines(c(0,120),c(0,0))
            }
          } else {
            barplot(Samples[[birdgs()]][sampShedID(),],main = NULL,xlab = "", ylab = "Number of samples",
                    space = 0, col = histSampCols(), names.arg = tvals(), ylim = c(0,round(1.1*max(Samples[[birdgs()]][sampShedID(),]))))
            if (input$periodt == 3) {
              axis(1, at=c(seq(0,3,1)), labels=FALSE, tick = TRUE)
              lines(c(0,3),c(0,0))
            } else if (input$periodt == 2){
              axis(1, at=c(seq(0,4,1)), labels=FALSE, tick = TRUE)
              lines(c(0,4),c(0,0))
            } else {
              axis(1, at=c(seq(0,12,1)), labels=FALSE, tick = TRUE)
              lines(c(0,12),c(0,0))
            }
          }
          title(xlab = "Time", line=2.5)
        } else {
          if (input$averaget == 1){
            barplot(Samples[[birdgs()]][sampShedID(),],main = NULL,xlab = "", 
                    ylab = "Number of samples",ylim = c(0,10.5),space = 0, xaxt = "n")
            if (input$periodt == 3) {
              axis(1, at=c(4/3,seq(25/6,30,3)), labels=seq(2007,2016,1), tick = FALSE)
              axis(1, at=c(seq(8/3,30,3)), labels=FALSE, tick = TRUE)
              lines(c(0,30),c(0,0))
            } else if (input$periodt == 2){
              axis(1, at=c(5/3,seq(16/3,40,4)), labels=seq(2007,2016,1), tick = FALSE)
              axis(1, at=c(seq(10/3,40,4)), labels=FALSE, tick = TRUE)
              lines(c(0,40),c(0,0))
            } else {
              axis(1, at=c(4.5,seq(15,120,12)), labels=seq(2007,2016,1), tick = FALSE)
              axis(1, at=c(seq(9,120,12)), labels=FALSE, tick = TRUE)
              lines(c(0,120),c(0,0))
            }
          } else {
            barplot(Samples[[birdgs()]][sampShedID(),],main = NULL,xlab = "", 
                    ylab = "Number of samples",ylim = c(0,10.5),space = 0)
            if (input$periodt == 3) {
              axis(1, at=c(seq(0,3,1)), labels=FALSE, tick = TRUE)
              lines(c(0,3),c(0,0))
            } else if (input$periodt == 2){
              axis(1, at=c(seq(0,4,1)), labels=FALSE, tick = TRUE)
              lines(c(0,4),c(0,0))
            } else {
              axis(1, at=c(seq(0,12,1)), labels=FALSE, tick = TRUE)
              lines(c(0,12),c(0,0))
            }
          }
          title(xlab = "Time", line=2.5)
        }
      }
      if (is.null(input$timeline)){
        abline(v = daten()-0.5, lty = 2, lwd = 2, col = 'red')
      } else if (input$timeline){
        abline(v = daten()-0.5, lty = 2, lwd = 2, col = 'red')
      } else {
      }
    }
    par(op)
    })
  
  output$PrevPlot <- renderPlot({
    op <- par(mar = c(3.5,4,1,1))
    if (is.na(sampShedName())){
      if (input$periodt == 4){
        if (input$averaget == 2){
          barplot(0,xlab = "", ylab = "Prevalence (%)",
                  space = 0, names.arg = c("Total"), ylim = c(0,105))
          axis(1, at=c(0,1), labels=FALSE, tick = TRUE)
          text(0.5,50,"Click on the map to select a watershed")
        } else {
          barplot(rep(0, length(tvals())),xlab = "", ylab = "Prevalence (%)",
                  space = 0, col = histPrevCols(), names.arg = tvals(), ylim = c(0,105), xaxt = "n")
          axis(1, at=c(seq(0.5,10.5,1)), labels=seq(2007,2017,1), tick = FALSE)
          axis(1, at=c(seq(0,11,1)), labels=FALSE, tick = TRUE)
          title(xlab = "Time", line=2.5)
          text(0.45*length(tvals()),50,"Click on the map to select a watershed")
        }
      } else {
        if (input$averaget == 1){
          barplot(rep(0, length(tvals())),xlab = "", ylab = "Prevalence (%)",
                  space = 0, col = histPrevCols(), names.arg = tvals(), ylim = c(0,105), xaxt = "n")
          if (input$periodt == 3) {
            axis(1, at=c(4/3,seq(25/6,30,3)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(8/3,30,3)), labels=FALSE, tick = TRUE)
          } else if (input$periodt == 2){
            axis(1, at=c(5/3,seq(16/3,40,4)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(10/3,40,4)), labels=FALSE, tick = TRUE)
          } else {
            axis(1, at=c(4.5,seq(15,120,12)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(9,120,12)), labels=FALSE, tick = TRUE)
          }
        } else {
          barplot(rep(0, length(tvals())),xlab = "", ylab = "Prevalence (%)",
                  space = 0, col = histPrevCols(), names.arg = tvals(), ylim = c(0,105))
          if (input$periodt == 3) {
            axis(1, at=c(seq(0,3,1)), labels=FALSE, tick = TRUE)
          } else if (input$periodt == 2){
            axis(1, at=c(seq(0,4,1)), labels=FALSE, tick = TRUE)
          } else {
            axis(1, at=c(seq(0,12,1)), labels=FALSE, tick = TRUE)
          }
        }
        title(xlab = "Time", line=2.5)
        text(0.5*length(tvals()),50,"Click on the map to select a watershed")
      }
    } else {
      if (input$periodt == 4){
        if (input$averaget == 2){
          barplot(100*Samples[[birdgs()+32]][sampShedID()],xlab = "", ylab = "Prevalence (%)",
                  space = 0, col = histPrevCols(), names.arg = c("Total"), ylim = c(0,105))
          lines(c(0,1),c(0,0))
          axis(1, at=c(0,1), labels=FALSE, tick = TRUE)
        } else {
          barplot(100*Samples[[birdgs()+32]][sampShedID(),],xlab = "", ylab = "Prevalence (%)",
                  space = 0, col = histPrevCols(), names.arg = tvals(), ylim = c(0,105), xaxt = "n")
          axis(1, at=c(seq(0.5,10.5,1)), labels=seq(2007,2017,1), tick = FALSE)
          axis(1, at=c(seq(0,11,1)), labels=FALSE, tick = TRUE)
          lines(c(0,11),c(0,0))
        }
      } else {
        if (input$averaget == 1){
          barplot(100*Samples[[birdgs()+32]][sampShedID(),],xlab = "", ylab = "Prevalence (%)",
                  space = 0, col = histPrevCols(), names.arg = tvals(), ylim = c(0,105), xaxt = "n")
          if (input$periodt == 3) {
            axis(1, at=c(4/3,seq(25/6,30,3)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(8/3,30,3)), labels=FALSE, tick = TRUE)
            lines(c(0,30),c(0,0))
          } else if (input$periodt == 2){
            axis(1, at=c(5/3,seq(16/3,40,4)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(10/3,40,4)), labels=FALSE, tick = TRUE)
            lines(c(0,40),c(0,0))
          } else {
            axis(1, at=c(4.5,seq(15,120,12)), labels=seq(2007,2016,1), tick = FALSE)
            axis(1, at=c(seq(9,120,12)), labels=FALSE, tick = TRUE)
            lines(c(0,120),c(0,0))
          }
        } else {
          barplot(100*Samples[[birdgs()+32]][sampShedID(),],xlab = "", ylab = "Prevalence (%)",
                  space = 0, col = histPrevCols(), names.arg = tvals(), ylim = c(0,105))
          if (input$periodt == 3) {
            axis(1, at=c(seq(0,3,1)), labels=FALSE, tick = TRUE)
            lines(c(0,3),c(0,0))
          } else if (input$periodt == 2){
            axis(1, at=c(seq(0,4,1)), labels=FALSE, tick = TRUE)
            lines(c(0,4),c(0,0))
          } else {
            axis(1, at=c(seq(0,12,1)), labels=FALSE, tick = TRUE)
            lines(c(0,12),c(0,0))
          }
        }
        title(xlab = "Time", line=2.5)
      }
      if (is.null(input$timeline)){
        abline(v = daten()-0.5, lty = 2, lwd = 2, col = 'red')
      } else if (input$timeline){
        abline(v = daten()-0.5, lty = 2, lwd = 2, col = 'red')
      } else {
      }
    }
    par(op)
  })
  
  output$slider <- renderUI({
    if (as.integer(input$periodt) + 4*(as.integer(input$averaget)-1) == 8){
      NULL
    } else {
      sliderTextInput("time",NULL,tvals(),grid=FALSE,force_edges = TRUE, animate = 
                        animationOptions(interval = 1500, loop = FALSE, 
                                         playButton = span(icon("play")," Play"),
                                         pauseButton = span(icon("pause")," Pause")))
    }
})
  
  tvals <- reactive({
    switch(as.integer(input$periodt) + 4*(as.integer(input$averaget)-1),
      allM1,
      allM3,
      allS,
      allY,
      M1,
      M3,
      S,
      NULL)
  })
  
  
  observeEvent({
    input$periodt
    input$averaget
  },{
    if (as.integer(input$periodt) + 4*(as.integer(input$averaget)-1)==8) {
      disable("timeline")
    } else {
      enable("timeline")
    }
  })
  
}

shinyApp(ui, server)

