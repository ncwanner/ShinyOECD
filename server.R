library(shiny)
library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(shiny)
library(tm)
library(jsonlite)
library(ggplot2)
library(leaflet)
library(twitteR)

api_key <- "xeFOm8tLy40H3EGxI1NSv0ZB8" 
api_secret <- "8Jp49pRXO9NdPDTE6Ui7zriHIt1c8t1pIh5zcTscsMsPAuUdR3" 
access_token <- "3613146256-tHF1v8Y6tFwGdAOlVBx7bM8aeXMXPxTbChsZe49" 
access_token_secret <- "xpuXIgVNvdjhNgchygKM3430iPeocGqjXGW3jlaHGLy0p" 
options(httr_oauth_cache=T)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

options(digits=2)

# From http://Data.okfn.org/Data/datasets/geo-boundaries-world-110m
geojson <- readLines("./Data/countries.geojson", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

# Default styles for all features
geojson$style = list(
  weight = 1,
  color = "#555555",
  opacity = 1,
  fillOpacity = 0.8
)

Recipients_Map = Recipients_Map %>%
  select(ISO3_CODE,
         Indicator5a_1,
         Indicator5a_2,
         Indicator6_1,
         Indicator6_2,
         Indicator9b,
         Year) %>%
  mutate(Recipients_Donors=1)

Donors_Map = Donors_Map %>%
  select(ISO3_CODE,
         Indicator5a_1,
         Indicator5a_2,
         Indicator6_1,
         Indicator6_2,
         Indicator9b,
         Year) %>%
  mutate(Recipients_Donors=2)

Donors_Map = Donors_Map[!(Donors_Map$Year==2005&Donors_Map$ISO3_CODE=="EGY"),]

Recipients_Graphs = select(Recipients_Graphs,
                           Name,
                           Year,
                           Number,
                           Indicator5a_1,
                           Indicator5a_2,
                           Indicator5b,
                           Indicator6_1,
                           Indicator6_2,
                           Indicator9b,
                           Recipients_Donors,
                           Total_Scheduled_Disbursements,
                           Total_Disursements_Gov_Sector,
                           Total_Funds_On_Budget
)

Donors_Graphs = select(Donors_Graphs,
                       Name,
                       Year,
                       Number,
                       Indicator5a_1,
                       Indicator5a_2,
                       Indicator5b,
                       Indicator6_1,
                       Indicator6_2,
                       Indicator9b,
                       Recipients_Donors,
                       Total_Scheduled_Disbursements,
                       Total_Disursements_Gov_Sector,
                       Total_Funds_On_Budget
)

Recipients_Donors_Graphs = full_join(Recipients_Graphs,Donors_Graphs,by=colnames(Recipients_Graphs))

server <- function(input, output) {
  
  Table1= reactive({
    
    Table1 = Data
    
    if (input$Recipients1 != "All") {
      
      Table1 <- Table1[Table1$Recipient %in% input$Recipients1,]
      
    }
    
    if (input$Donors1 != "All") {
      
      Table1 <- Table1[Table1$Donor %in% input$Donors1,]
      
    }
    
    if (input$Year1 != "All") {
      
      Table1 <- Table1[Table1$Year %in% input$Year1,]
      
    }
    
    Table1
    
  })
  
  output$table1 = renderDataTable({Table1()})
  
  output$downloadData1 <- downloadHandler(
    filename = "Recipients_Donors.csv",
    content = function(file) {
      write.csv(Table1(), file)})
  
  
  Table2 = reactive({
    
    Table2 = Recipients
    
    if (input$Recipients2 != "All") {
      
      Table2 <- Table2[Table2$Recipient %in% input$Recipients2,]
      
    }
    
    if (input$Year2 != "All") {
      
      Table2 <- Table2[Table2$Year %in% input$Year2,]
      
    }
    
    Table2
    
  })
  
  output$table2 = renderDataTable({Table2()})
  
  output$downloadData2 <- downloadHandler(
    filename = "Recipients.csv",
    content = function(file) {
      write.csv(Table2(), file)})
  
  Table3 = reactive({
    
    Table3 = Donors
    
    if (input$Donors2 != "All") {
      
      Table3 <- Table3[Table3$Donor %in% input$Donors2,]
      
    }
    
    if (input$Year3 != "All") {
      
      Table3 <- Table3[Table3$Year %in% input$Year3,]
      
    }
    
    Table3
    
  })
  
  output$table3 = renderDataTable({Table3()})
  
  output$downloadData3 <- downloadHandler(
    filename = "Donors.csv",
    content = function(file) {
      write.csv(Table3(), file)})
  
  Updated = reactive({
    
    setProgress(message = "Calculating...")

    Recipients_temp = Recipients_Map  %>%
      filter(Year==input$input_map_year)

    Donors_temp = Donors_Map %>%
      filter(Year==input$input_map_year)

    Recipients_Donors = full_join(Recipients_temp,Donors_temp,by=colnames(Recipients_temp))

    if (input$input_map_indicator==1) {
  
      Recipients_Donors = Recipients_Donors %>%
        mutate(Indicator=Indicator5a_1)
  
    }

    if (input$input_map_indicator==2) {
  
      Recipients_Donors = Recipients_Donors %>%
        mutate(Indicator=Indicator5a_2)
  
    }

    if (input$input_map_indicator==3) {
  
      Recipients_Donors = Recipients_Donors %>%
        mutate(Indicator=Indicator6_1)
  
    }

    if (input$input_map_indicator==4) {
  
      Recipients_Donors = Recipients_Donors %>%
        mutate(Indicator=Indicator6_2)
  
    }

    if (input$input_map_indicator==5) {
  
      Recipients_Donors = Recipients_Donors %>%
        mutate(Indicator=Indicator9b)
  
    }

    Recipients_Donors = Recipients_Donors %>%
      filter(Indicator != Inf,
             !is.na(Indicator),
            !is.na(ISO3_CODE)
      )

    for (i in 1:length(geojson$features)) {
  
      ISO3_CODE = geojson$features[[i]]$properties$iso_a3
      ISO3_CODE = data.frame(ISO3_CODE)
      ISO3_CODE$ISO3_CODE = as.character(ISO3_CODE$ISO3_CODE)
  
      geojson$features[[i]]$properties$Indicator = left_join(data.frame(ISO3_CODE),Recipients_Donors,by=c("ISO3_CODE"))$Indicator
      geojson$features[[i]]$properties$Recipient_Donor = left_join(data.frame(ISO3_CODE),Recipients_Donors,by=c("ISO3_CODE"))$Recipients_Donors
      
      if (is.na(geojson$features[[i]]$properties$Recipient_Donor)) {
        
        geojson$features[[i]]$properties$Recipient_Donor = 0
        
      }
      
    }
    
    Indicator <- sapply(geojson$features, function(feat) {
      feat$properties$Indicator*100
    })
    
    # Color by per-capita GDP using quantiles
    pal1 <- colorNumeric("Greens", Indicator)
    pal2 <- colorNumeric("Blues", Indicator)
    # Add a properties$style list to each feature
    geojson$features <- lapply(geojson$features, function(feat) {
      if (feat$properties$Recipient_Donor==1) {
        feat$properties$style <- list(
          fillColor = pal1(
            feat$properties$Indicator*100
          )
        )}
      else if (feat$properties$Recipient_Donor==2) {
        feat$properties$style <- list(
          fillColor = pal2(
            feat$properties$Indicator*100
          )
        )}
      
      else(
        feat$properties$style <- list(
          fillColor = pal1(NA)
        )
      )
      feat
    }
    )
    
    Updated = list(geojson,pal1,pal2,Indicator)
    
  })
  
  output$map = renderLeaflet({    
    withProgress({
      setProgress(message = "Calculating...")
        leaflet() %>% 
          addGeoJSON(Updated()[[1]]) %>%
          setView(lng=0,lat=40,zoom=2) %>%
          addLegend("bottomright", pal = Updated()[[2]], values = Updated()[[4]],
                    title = "Recipients",
                    opacity = 1) %>%
          addLegend("bottomleft", pal = Updated()[[3]], values = Updated()[[4]],
                    title = "Donors",
                    opacity = 1
          )
    })})
  
  Parameters = reactive({
    
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Calculating...")
        mach_tweets = searchTwitter(input$word, n=50, lang="en")
        mach_text = sapply(mach_tweets, function(x) x$getText())
        mach_text <- sapply(mach_text,function(row) iconv(row, "latin1", "ASCII", sub=""))
        
        # create a corpus
        
        mach_corpus = Corpus(VectorSource(mach_text))
        
        
        
        
        # create document term matrix applying some transformations
        
        tdm = TermDocumentMatrix(mach_corpus,
                                 
                                 control = list(removePunctuation = TRUE,
                                                
                                                stopwords = c("machine", "learning", stopwords("english")),
                                                
                                                removeNumbers = TRUE, tolower = TRUE))
        
        # define tdm as matrix
        
        m = as.matrix(tdm)
        
        # get word counts in decreasing order
        
        word_freqs = sort(rowSums(m), decreasing=TRUE) 
        
        # create a Data frame with words and their frequencies
        
        dm = data.frame(word=names(word_freqs), freq=word_freqs)
        
        WordCloud = dm
        
      })
    })
  })
  
  # plot wordcloud
  
  output$plot = renderPlot({
    wordcloud(Parameters()$word, Parameters()$freq, random.order=FALSE,colors=brewer.pal(8, "Dark2"),
              scale=c(6,1))
  })
  
  output$graph = renderPlot({
    
    Data_Graph = Recipients_Donors_Graphs
    
    if (input$recipients_donors_graph != 0) {
      
      Data_Graph <- filter(Data_Graph,
                           Recipients_Donors==input$recipients_donors_graph)
      
    }
    
    if (input$input_graph_year != "All") {
      
      Data_Graph <- filter(Data_Graph,
                           Year==input$input_graph_year)
      
    }
    
    if (input$input_graph_indicator==1) {
      
      Data_Graph = Data_Graph %>%
        mutate(Indicator=Indicator5a_1,
               weighting = Total_Scheduled_Disbursements)
      
    }
    
    if (input$input_graph_indicator==2) {
      
      Data_Graph = Data_Graph %>%
        mutate(Indicator=Indicator5a_2,
               weighting = Total_Disursements_Gov_Sector)
      
    }
    
    if (input$input_graph_indicator==3) {
      
      Data_Graph = Data_Graph %>%
        mutate(Indicator=Indicator6_1,
               weighting = Total_Scheduled_Disbursements)
      
    }
    
    if (input$input_graph_indicator==4) {
      
      Data_Graph = Data_Graph %>%
        mutate(Indicator=Indicator6_2,
               weighting = Total_Funds_On_Budget)
      
    }
    
    if (input$input_graph_indicator==5) {
      
      Data_Graph = Data_Graph %>%
        mutate(Indicator=Indicator9b,
               weighting = Total_Disursements_Gov_Sector)
      
    }
    
    Plot = ggplot(Data_Graph, aes(x = Indicator, weight = weighting)) +
      geom_histogram(binwidth=.025) +
      xlab("Indicator") +
      ylab("Weighted Density") +
      theme(axis.text.y = element_blank(),
            text = element_text(size=20)) + 
      xlim(0,1) +
      labs(title = "Weighted Histogram")
    
    Plot
    
  })
  
  }