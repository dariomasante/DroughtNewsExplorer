library(shiny)
library(shinydashboard)

ui <- dashboardPage( skin='yellow', # UI ----
  dashboardHeader(title = 'Drought news explorer'),
  dashboardSidebar(
    hr(),
    sidebarMenu(id="tabs",
                menuItem("Analysis", tabName="plots", icon=icon("line-chart"), selected=TRUE),
                menuItem("Full table", tabName = "fullTable", icon=icon("table")),
                menuItem("Map", icon = icon("file-text-o"),
                        tabName = "map"# menuSubItem("server.R", tabName = "server", icon = icon("angle-right")) #
                )
    ),
    hr(),
    conditionalPanel("input.tabs=='plots'",
                     fluidRow( #title='News aggregator:',
                       column(1),column(10,
                                        actionButton("harvest", "Search news")),
                       column(1),column(10,
                              "Select news aggregators:",
                              #checkboxInput("db", "GDO database", FALSE),
                              checkboxInput("emm", "European Media Monitor", TRUE),
                              checkboxInput("gn", "Google news", FALSE),
                              checkboxInput("ga", "Google alerts", FALSE)
                       ),
                       column(1),column(10,
                              radioButtons("lang", "Language:", c("English only"="en","All languages"="all"), selected='en')
                       ),
                       dateInput('from',
                                 label = 'From:',
                                 value = Sys.Date() - 15
                       ),
                       dateInput('to',
                                 label = 'Until:',
                                 value = Sys.Date()
                       ),
                       textInput("txt", "Must contain (optional, space separated):", ""),
                       textInput("txtNot", "Must not contain (optional - those relating to sport already included):", "")
                     )
    ),
    conditionalPanel(
      "input.tabs=='fullTable'",
      fluidRow( column(1),column(10, downloadButton('downloadTable', 'Download')) ),
      fluidRow( column(1),column(10, actionButton('send2db', 'Copy into database')) )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plots",
              box(  width = NULL, 
                column(width = 3,
                  fluidRow(
                    radioButtons("titleText", "Search inside:", c("Titles only"="title","First paragraph"="intro"), selected='intro'),
                    sliderInput("freq",
                                "Minimum word frequency:",
                                min = 1,  max = 30, value = 5),
                    sliderInput("max",
                                "Maximum Number of Words:",
                                min = 1,  max = 50,  value = 25),
                    actionButton("updateCharts", "Update charts")
                  )

                ),
                column(width = 9,
                       tabBox( width = NULL,
                               tabPanel(h5("Wordcloud"),  plotOutput("plotWC")
                               ),
                               tabPanel(h5("Word frequency"),  plotOutput("plotFreq")
                               ),
                               tabPanel(h5("Associated words"),  
                                        fluidRow(plotOutput("plotAssoc"))
                               ),
                               tabPanel(h5("Word correlations"),
                                        fluidRow(htmlOutput("txtAssoc"))
                               )
                       )
                       
                    ),
                collapsible = TRUE, title="Text analysis", status = "warning", solidHeader = TRUE, color='orange'),
        fluidRow(
          box( width = NULL, status = "warning", solidHeader = TRUE, title="Ranked news", collapsible = TRUE,                
               fluidRow( column(1, downloadButton('dwnSel', 'Download selected rows'))),
               br(),
               DT::dataTableOutput("prettyTable") #tableOutput("table")
          )
        )
      ),
      tabItem(tabName = "fullTable",
              fluidRow( width = NULL, solidHeader = TRUE, title="Table", tableOutput("table"))
      ),
      tabItem(tabName = "map", box() )
    )
  )
)



server <- function(input, output, session) {
  library(tm)
  library(RCurl)
  library(XML)
  library(wordcloud)
  # library(ggplot2)
  
  harvest_galerts = function(link=NULL,tb=NULL){ # Google Alerts This alert was set up with a personal google account
    if(is.null(link)){
      link = "https://www.google.com/alerts/feeds/" # Left blank on purpose, get your own token
    }
    gurl = getURL(link)
    doc = xmlParse(gurl, asText = TRUE)
    
    ## Parse and data frame
    ## Get date
    Date = xpathSApply(doc, "//x:entry/x:published", xmlValue, namespaces=c(x = "http://www.w3.org/2005/Atom"))
    Date = sapply(strsplit(Date, 'T'), '[[', 1)
    Date = format(as.Date(Date), "%d/%m/%Y")
    ## Get title
    title = xpathSApply(doc, "//x:entry/x:title", xmlValue, namespaces=c(x = "http://www.w3.org/2005/Atom"))
    title = tolower(gsub("<.*?>", "", title))
    ## Get intro content
    intro = xpathSApply(doc, "//x:entry/x:content", xmlValue, namespaces=c(x = "http://www.w3.org/2005/Atom"))
    intro = tolower(gsub("<.*?>", "", gsub("&.*?;", "", intro)))   # remove html tags etc
    ## Get link
    link = xpathSApply(doc, "//x:entry/x:link", xmlGetAttr, 'href', namespaces=c(x = "http://www.w3.org/2005/Atom"))
    link = substring(link, 43) # hardcoded to remove the google bit from url
    
    ## Remove off topic based on keywords
    rm_id = !(grepl('-team', title) | grepl('-game', title) | grepl('ictory', title)
              | grepl('playoff', title) | grepl('asket', title) | grepl('-cup', title) | grepl('-sport', link)
              | grepl('-win-', title) | grepl('nba', title) | grepl('ournament', title) | grepl('ncaa', title))
    ## Make table 
    tab = cbind(Date, link, title, intro)[rm_id, ]
    if(!is.null(tb)){
      tab = rbind(tb,tab)
    }
    return(tab)
  }
  
  harvest = function(link){ # returns a table with columns date, link, title
    gurl = getURL(link)
    doc = xmlParse(gurl, asText = TRUE)
    Date = strsplit(xpathSApply(doc, "//channel/item/pubDate", xmlValue), ' ')
    Date = sapply(Date, function(d){
      paste(d[c(4,3,2)], collapse='-')
    })
    # Date = as.Date(strptime(Date,format="%Y-%b-%d"))
    # dt_id = Date > Date[1] - 7 # Select only fresher than a week
    # Date = format((Date), "%d/%m/%Y")
    title = xpathSApply(doc, "//channel/item/title", xmlValue)
    #keep_id = grepl('drought', title) | grepl('Drought', title)
    link = xpathSApply(doc, "//channel/item/link", xmlValue)
    intro = xpathSApply(doc, "//channel/item/description", xmlValue)
    addTb = cbind(Date, link, title, intro)#[keep_id & dt_id, ]
    if(nrow(addTb) > 0){
      return(addTb)
    }
  }
  
  harvest_EMM = function(link=NULL,keywords=c('drought','rain'),notWord=NULL,tb=NULL,from=Sys.Date()-15,to=Sys.Date(),lan='en'){ # Harvest from european media monitor
    #   Sys.setlocale("LC_TIME", "en_US.UTF-8")
    if(is.null(link)){
      key = paste(keywords, collapse='%2C')
      if(is.null(notWord)){
        notWord = c('football','tennis','finals','tournament','victory','playoff','nba',
                    'ncaa','league','sponsor','championship','hurricane','nfl','quarterback')
      } else {
        notWord = strsplit(notWord, ' ')[[1]]
      }
      fromTo = paste0('dateto=',to,'T23%3A59%3A59Z&datefrom=',from,'T00%3A00%3A00Z')
      link = paste0('http://emm.newsbrief.eu/rss/rss?language=',lan,'&not=',paste(notWord, collapse='+'),
                    '&type=search&mode=advanced&all=',key,'&duplicates=false&',fromTo)
    }
    addTb = harvest(link)
    if(!is.null(tb)){
      if(nrow(tb) > 0 | nrow(addTb) > 0){
        tb = rbind(tb, addTb)
      }
      return(tb)
    }
    if(nrow(addTb) > 0){
      return(addTb)
    }
  }
  
  harvest_gnews = function(link=NULL,keywords=c('drought','rain'),tb=NULL){ # Harvest from google news
    if(is.null(link)){
      key = paste(keywords, collapse='+')
      link = paste0('https://news.google.com/news/rss/search/section/q/',key,'?gl=US&hl=en&ned=us')
    }
    addTb = harvest(link)
    addTb[,4] = addTb[,3] # Fix as there is no paragraph accessible rom rss
    if(!is.null(tb)){
      if(nrow(tb) > 0 | nrow(addTb) > 0){
        tb = rbind(tb, addTb)
      }
      return(tb)
    }
    if(nrow(addTb) > 0){
      return(addTb)
    }
  }
  
  termFactory = function(tb, ignoreWords=NULL, field='title'){
    tb = tb[!grepl('<U+', tb[ ,field]), ]
    df_text <- data.frame(doc_id=1:nrow(tb), text=tb[ ,field])
    text <- Corpus(DataframeSource(df_text)) # VCorpus(VectorSource(txt))
    text <- tm_map(text, removePunctuation)
    text <- tm_map(text, removeNumbers)
    text <- tm_map(text, tolower)
    text <- tm_map(text, function(x) removeWords(x, c(stopwords("english"),ignoreWords=ignoreWords)))
    text <- tm_map(text, stripWhitespace)
    #text <- tm_map(text, PlainTextDocument)
    return(TermDocumentMatrix(text))
    #return(DocumentTermMatrix(text))
  }
  
  make_wc = function(tb, minFreq=3, ignoreWords=c('drought','rain'), field='title', wordN = 25){
    tdm = termFactory(tb, ignoreWords=ignoreWords, field=field)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m),decreasing=TRUE)
    df <- data.frame(word = names(v),freq=v)
    #pal2 <- brewer.pal(8,"Dark2")
    wordcloud(df$word,df$freq, scale=c(8,.2),min.freq=minFreq,
              max.words=wordN, random.order=FALSE, rot.per=.15)#, colors=pal2)
  }
  
  summaryTerms = function(dm, minFreq=5, field='title', ignoreWords=c('drought','rain'), plt=FALSE){
    if(!'TermDocumentMatrix' %in% class(dm)){
      tm = termFactory(dm, ignoreWords=ignoreWords, field=field)
    } else {
      tm = dm
    }
#    m <- removeSparseTerms(as.matrix(tm), 0.1)
    m <- as.matrix(tm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v), freq=v, stringsAsFactors = FALSE)
    #par(mfrow=c(2,1))
    sel = d[d$freq >= minFreq, ][1:input$max,]
    # ggp = ggplot(sel, aes(x=as.factor(word))) + geom_bar() + theme_minimal()
    # return(ggp)
    barplot(sel$freq, names.arg = sel$word, las=2, ylab = "Word frequencies")
    if(plt == TRUE){ # NEED FIX
      full = data.frame(title=paste(dm[,'title'], collapse='. '),
                        intro=paste(dm[,'intro'], collapse='. '), stringsAsFactors=FALSE)
      ftm = termFactory(full, ignoreWords=c('drought','rain'), field=field)
      plot(ftm, terms = findFreqTerms(ftm, lowfreq = minFreq), corThreshold = 0.2, weighting = TRUE)
    }
  }
  
  rankNews = function(tb, field='intro', highrankWords=NULL, goodWords=NULL, penaltyWords=NULL){
    if(is.null(highrankWords)){
      highrankWords = c('water','stricken','crisis','farmers','fire','crop','security','impact','hosepipe','climate','dry','poverty',
                        'food', 'refugee', 'aid', 'reservoir', 'transport', 'power', 'energy', 'supply', 'humanitarian', 'famine', 
                        'migration','hydro','waterway')
    }
    if(is.null(goodWords)){
      goodWords = c('winter','hit','weather','wheat','extreme','heatwave','conditions','grip','crippl','hottest',
                    'scorche','driest','warmest','livestock','pasture','hay','hot','prices','forecast','temperature',
                    'ban','soil','parched','tap','amid','cope','cattle','heat wave','nino','snowpack','snow',
                    'lake','levels','alert','river','poor')
    }
    if(is.null(penaltyWords)){
      penaltyWords = c('flood','pray','garden','winner','heavy','inundated','flash flood')
    }
    if(!field %in% colnames(tb)){ field = 'title' }
    score = sapply(tb[,field], function(tt){
      tt = tolower(tt)
      s = sum(sapply(highrankWords, function(x) { grepl(x, tt) }) * 2)
      s = sum(s, sapply(goodWords, function(x) { grepl(x, tt) }))
      s = sum(s, sapply(penaltyWords, function(x) { grepl(x, tt) }) * -2)
      names(s) = 1:length(s)
      return(s)
    })
    keyW = sapply(tb[,field], function(tt){
      tt = tolower(tt)
      w = c(highrankWords[sapply(highrankWords, function(x) { any(grepl(x, tt)) })], 
            goodWords[sapply(goodWords, function(x) { any(grepl(x, tt)) })])
      return(
        paste(w,collapse=' ')
      )
    })
    names(keyW) = 1:length(keyW)
    o = order(score, decreasing = TRUE)
    return( list(tb = tb[o, ], keyW = keyW[o], score = score[o]) )
  }
  
  observeEvent(input$send2db, {
    showNotification("This function is not active at the moment.")
  })
  
  observeEvent(input$harvest, {
    showNotification("News harvesting will take a few seconds, please wait until table is loaded.")
  })
  
  keys = function(kw){
    if(nchar(kw) > 0){
      return(strsplit(kw, ' ')[[1]])
    } else {
      return(c('drought','rain'))
    }
  }
  
  r <- eventReactive(input$harvest, { # News harvest must be launched by button after user choice ----
      tab = NULL
      # if(input$db){
      #   tab = read.csv('DroughtNews_GoodLinks.csv')
      # }
      
      if(input$emm){
        tab = harvest_EMM(keywords=keys(input$txt), notWord=input$txtNot, from=input$from, to=input$to, lan=input$lang)
      }
      if(input$gn){
        tab = harvest_gnews(tb=tab, keywords=keys(input$txt))
      }
      if(input$ga){
        tab = harvest_galerts(tb=tab)
      }
      return(tab[!duplicated(tab[,3]), ])
  })

  pltWC = eventReactive( input$updateCharts, {
    make_wc(r(), minFreq=input$freq, ignoreWords=keys(input$txt), field=input$titleText, wordN=input$max)
  })
  output$plotWC <- renderPlot({
    pltWC()
  }) 
  
  pltFreq = eventReactive( input$updateCharts, {
      summaryTerms(r(), minFreq=input$freq, field=input$titleText, ignoreWords=keys(input$txt))
  })
  output$plotFreq <- renderPlot({
    pltFreq()
  })
  
  pltAssoc = eventReactive( input$updateCharts, {
    dtm = termFactory(r(), field=input$titleText, ignoreWords=keys(input$txt))
    plot(dtm, terms = findFreqTerms(dtm, lowfreq = input$freq), corThreshold = 0.3)
  })
  output$plotAssoc = renderPlot({
    pltAssoc()
  })

  textAssoc = eventReactive( input$updateCharts, {
    dtm = termFactory(r(), field=input$titleText, ignoreWords=keys(input$txt))
    lst = findAssocs(dtm, terms = findFreqTerms(dtm, lowfreq = input$freq), corlimit = 0.5)
    lst = lst[lapply(lst,length)>0]
    out = sapply(1:length(lst), function(i){
      paste( paste0('<b>',names(lst[i]),'</b>'), paste(names(lst[i][[1]]), lst[[i]], collapse=' | '), sep='<br/>')
    })
    return(HTML(paste(out, collapse='<br/>')))
  })
  output$txtAssoc <- renderUI({
    textAssoc()
  })
  
  output$dwnSel = downloadHandler('table_selection.csv', 
                                  content = function(file) {
                                    if(length(input$prettyTable_rows_selected)>0){
                                      s = input$prettyTable_rows_selected
                                    } else {
                                      s = input$prettyTable_rows_all
                                    }
                                    write.csv(r()[s, ], file, row.names = FALSE)
                                  })
  
  output$prettyTable = DT::renderDataTable({
    rk = rankNews(r(), field=input$titleText)
    tb = rk$tb
    Keywords = rk$keyW; Link = paste0('<a href=',tb[,2],' target="_blank" class="btn btn-primary">Open link</a>')
    cbind(tb[,c(1,3,4)], Keywords, Link)
  }, options = list(pageLength = 5), escape=FALSE)

  output$table <- renderTable({ 
    r()
  })
  
  output$downloadTable <- downloadHandler("table.csv",
                                          content = function(file) {
                                            write.csv(r(), file, row.names = FALSE)
                                          })
  #session$onSessionEnded(stopApp)
}


shinyApp(ui, server)
