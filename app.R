options(shiny.maxRequestSize=10*1024^2)
rm(list = ls())
library(shiny)
library(readr)
library(ggplot2)
source('assessment.R')
source('javascript.R')


ui <- fluidPage(
  
  titlePanel("Biodiversity Indicator Integration"),
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Choose input file'),
      selectInput('sepname','Column Separator:',c("Comma","Semi-colon","Tab")),

            withTags({
        div(class="header", checked=NA,
            h4("Instructions"),
            p("Select the file containing input data for the assessment.
              The file must be in text format and column headers must be included.
              The required columns are:"),
            ul(
              li("Group"),
              li("Indicator"),
              li("Threshold"),
              li("Status"),
              li("Reference"),
              li("Bad")
            ),
            p("The following columns are optional:"),
            ul(
              li("SpeciesGroup"),
              li("Species"),
              li("SpatialAssessmentUnit")
            ),
            p("The assesssment is made per Spatial Assessment Unit. If no Spatial Assessment Unit is specified, all indicators are combined in a single assessment."),
            p("Within a spatial assessment unit, indicators are aggregated within Group (e.g. Birds, Fish, etc.)"),
            p("If SpeciesGroup is specified (e.g. Wading Birds), then they will be aggregated at this level before Group (eg. Birds)"),
            p("If Species is specified, then indicators will be aggregated at this level before SpeciesGroup"),
            p("Contact:", HTML("<a href='https://niva-denmark.dk' target='_blank'>https://niva-denmark.dk</a>"))
        )
        
      })
      
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data", tableOutput("InDatatable")),
      tabPanel("Species", 
               uiOutput("Species2")),  
      tabPanel("SpeciesGroups", 
               uiOutput("SpeciesGroups2")),  
      tabPanel("Groups", 
               uiOutput("Groups2")),  
      tabPanel("Assessment Units", 
               uiOutput("AssessmentUnits")),
      tabPanel("Download",
               downloadButton('downloadIndicators', 'Download Indicator Results'),
               downloadButton('downloadGroups', 'Download Group Results'),
               downloadButton('downloadAssessmentUnits', 'Download Assessment Unit Results')
               
      )
    ) # tabset panel
  )
  )  
  
) #fluid page

server <- function(input, output, session) {
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script))
  }, FALSE)
  
  output$caption <- renderText(input$num)
  
  
  addResourcePath("data","./data/")
  
  sepchar<-reactive({
    sep<-","
    if(input$sepname=="Semi-colon"){sep<-";"}
    if(input$sepname=="Tab"){sep<-"\t"}
    return(sep)
  })
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    #sepchar<-","
    #if(input$sepname=="Semi-colon"){sepchar<-";"}
    #if(input$sepname=="Tab"){sepchar<-"\t"}
    
    dfencode<-guess_encoding(infile$datapath,n_max=-1)
    cat(paste0(dfencode$encoding[1],"\n"))

        filedata<-read.table(infile$datapath, sep=sepchar(),
                         encoding=dfencode$encoding[1], header=T, stringsAsFactors=F)
    
    return(filedata)
  })
  
  InData <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,1)    #Individual indicator results
    return(out)
  })
  Indicators <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,2)
    return(out)
  })
  IndicatorsDownload <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,0)
    return(out)
  })
  Species <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,3)
    return(out)
  })  
  SpeciesGroups <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,4)
    return(out)
  })
  Groups <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,5)
    return(out)
  })
  AssessmentUnits <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,6)
    return(out)
  })
  
  
  CHASEplot<- reactive({
    QE<-QEdata()
    
    ymax=max(QE$ConSum,na.rm=TRUE)
    ymax=ceiling(ymax)
    if(ymax>5 & ymax<10){ymax=10}
    if(ymax>1 & ymax<5){ymax=5}
    
    if (is.null(QE)){return(NULL)}
    
    levels<-data_frame(factor(c("High","Good","Moderate","Poor","Bad"),
                              levels=c("High","Good","Moderate","Poor","Bad")),
                       c(0.0,0.5,1,5,10),
                       c(0.5,1,5,10,ymax))
    names(levels)[1] <- 'Status'
    names(levels)[2] <- 'ymin'
    names(levels)[3] <- 'ymax'
    
    levels2<-levels
    levels$x<-0.5
    levels2$x<-0.5+max(as.numeric(QE$Waterbody))
    
    levels<-rbind(levels, levels2)
    
    levels<-levels[levels$ymin<=ymax,]
    ymax2=max(levels$ymax,na.rm=TRUE)
    levels[levels$ymax==ymax2,]$ymax<-ymax    
    Palette1=c("#3399FF", "#66FF66", "#FFFF66","#FF9933","#FF6600" )
    
    p<-ggplot(data=QE,x=Waterbody,y=ConSum) + theme_bw() +
      geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum,shape=Matrix, ymin=0)) +
      geom_ribbon(data=levels,aes(x=x,ymin=ymin,ymax=ymax,fill=Status),alpha=0.5) +
      geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum,shape=Matrix, ymin=0)) +
      scale_fill_manual(name="Status", values=Palette1)+
      xlab('Waterbody')+ylab('Contamination Sum')
    return(p)
  })
  
  output$downloadAssessmentUnits <- downloadHandler(
    filename = function() { paste0('Results_Assessment_Units.csv') },
    content = function(file) {
      write.table(AssessmentUnits(), file, sep=sepchar(),row.names=F,na="")
    })
  output$downloadGroups <- downloadHandler(
    filename = function() { paste0('Results_Groups.csv') },
    content = function(file) {
      write.table(Groups(), file, sep=sepchar(),row.names=F,na="")
    })
  output$downloadIndicators <- downloadHandler(
    filename = function() { paste0('Results_Indicators.csv') },
    content = function(file) {
      write.table(IndicatorsDownload(), file, sep=sepchar(),row.names=F,na="")
    })
  
  
  output$InDatatable <- renderTable({return(InData())},na="")
  output$Indicators<- renderTable({return(Indicators())},na="")
  output$Groups<- renderTable({return(Groups())},na="")
  output$SpeciesGroups<- renderTable({return(SpeciesGroups())},na="")
  output$Species<- renderTable({return(Species())},na="")
  output$AssessmentUnits<- renderTable({return(AssessmentUnits())},na="")
  
  output$plot <- renderPlot({return(CHASEplot())})
  output$QEtable <- renderTable({return(QEspr())})
  output$Groups2 <- renderUI({
    list(
      tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
      , tableOutput("Groups")
    )})
  output$SpeciesGroups2 <- renderUI({
    list(
      tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
      , tableOutput("SpeciesGroups")
    )})
  output$Species2 <- renderUI({
    list(
      tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
      , tableOutput("Species")
    )})
  
  
  
  }

shinyApp(ui=ui, server=server)
