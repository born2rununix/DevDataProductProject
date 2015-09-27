suppressWarnings(library(shiny))
suppressWarnings(library(XML))
suppressWarnings(library(reshape2))
suppressWarnings(library(ggplot2))

extractYears <- function() {
  atlURL<-"http://weather.unisys.com/hurricane/atlantic"
  ##
  tables <- readHTMLTable(atlURL, header=FALSE)
  ##
  tmp <- sapply(tables[[2]], function(xx) as.numeric(gsub('[^0-9]', '', xx)))
  ##
  years = sort(na.omit(as.vector(tmp)))
  # years
}

selectedYear <- function(stormUrl,year) {

  stormTables <- readHTMLTable(stormUrl, header=TRUE)
  
  stmp <- as.data.frame(stormTables[[1]])
  stmp$Name <- as.character(stmp$Name)
  vName = list()
  sType = list()
  for (ndx in 1 : length(stmp$Name)) {
    l <- unlist(strsplit(stmp$Name[ndx],' '))
    if (is.null(l)) {
      print("Empty list")
    } else if (length(l) == 2) {
      vName <- append(vName, l[2])
      sType <- append(sType, l[1])
    } else if (length(l) >= 3) {
      vName <- append(vName, l[3])
      sType <- append(sType, paste(l[1],l[2]))
    }
  }
  stmp$Name <- unlist(vName)
  stmp$StormType <- unlist(sType)
  stmp$StormType <- as.factor(stmp$StormType)
  stmp$Date <- as.character(stmp$Date)
  stmp$Pres <- as.numeric(stmp$Pres)
  stmp$Wind <- as.numeric(stmp$Wind)
  
  ## Date Range
  lEndDate <- list()
  lStartDate <- list()
  for (ndx in 1 : length(stmp$Date)) {
    l <- unlist(strsplit(stmp$Date[ndx],'-'))
    if (is.null(l)) {
      print("Empty list")
      
    } else {
      if (length(l) == 2) {
        str2 <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", l[2], perl=TRUE)
        endDate <- unlist(strsplit(str2,' '))
        if (length(endDate) < 2) {
           print("Missing End Month???")
        } else {
          str1 <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", l[1], perl=TRUE)
          startDate <- unlist(strsplit(str1,' '))
          if (length(startDate) == 1) {
            ## Need month from 2
            lStartDate <- append(lStartDate, paste(startDate[1],endDate[2],year,sep=''))
          } else {
            lStartDate <- append(lStartDate, paste(startDate[1],startDate[2],year,sep=''))
          }
          lEndDate <- append(lEndDate, paste(endDate[1],endDate[2],year,sep=''))
        } 
      }
    }
  }
  stmp$startDate <- as.Date(unlist(lStartDate), "%d%b%Y")
  stmp$endDate <- as.Date(unlist(lEndDate), "%d%b%Y")
  
  keeps <- c("Name", "Wind", "Pres", "Cat", "StormType", "startDate", "endDate")
  df <- stmp[,keeps,drop=FALSE]
  df
}

shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    extractYears()
  })
  
  # Combine the selected variable into a link to populate new data frame
  selectedData <- reactive({
    if ( is.null(input$year)) {
      return( as.data.frame(c("Name", "Wind", "Pres", "Cat", "StormType", "startDate", "endDate") ))
    }
    stormUrl <- paste("http://weather.unisys.com/hurricane/atlantic/",
                       isolate(input$year), "/index.html", sep='')
    df <- selectedYear(stormUrl, isolate(input$year))
    df
  })
  
  output$year <- renderUI({
    if (is.null(dataInput())) {
      return()
    }
    # select 2003 as a starting year
    selectInput('year', 
                paste("Select From Years :", min(dataInput()),"to",max(dataInput())), 
                choices = dataInput(), selected = "2003", selectize=FALSE)
  })

  dataSummary <- reactive({
    if (is.null(selectedData())) {
      return()
    }
    s <- summary(selectedData())
    s
  })
  
  output$summary <- renderPrint({ # renderUI({
    if (is.null(dataSummary())) {
       return()
    }
    dataSummary()
  })  
  
  output$view <- renderTable({ #renderUI({
    if (is.null(selectedData())) {
      return()
    }
    obs <- selectedData()
    head(obs, n = dim(obs)[1])
  })  
  
  output$plot1 <- renderPlot({
    if (is.null(selectedData()) | dim(selectedData())[2] <= 1) {
      return()
    }
    
    stmp <- selectedData()
    bx_stmp <- stmp[order(stmp$Cat),]
    
    tasks <- bx_stmp$Name
    dfr <- data.frame(
      name        = factor(tasks, levels = tasks),
      start.date  = bx_stmp$startDate,
      end.date    = bx_stmp$endDate,
      Category = bx_stmp$Cat
    )
    mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))
    
    ggplot(mdfr, aes(value, name, colour = Category)) + 
      geom_line(size = 4) +
      xlab(NULL) + 
      ylab(NULL)
  })
})
