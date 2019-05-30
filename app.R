library(shiny)
library(shinyWidgets)
library(arules)
library(arulesViz)
library('DT')
library('devtools')
library('plyr')
library('dplyr')
library('tidyverse')
library('ggplot2')
library('chron')
library('plotly')
library('shinythemes')
library(readxl)
library(htmlwidgets)
library(scales)
library(RColorBrewer)

#Read the file
retail = read.csv("retail.csv", stringsAsFactors = F)
#Read Market Basket File
tr <- read.transactions("market_basket_type.csv", sep = ',')

#Change Format
retail$Total <- as.numeric(as.factor(retail$Total))
retail$Date <- as.Date(retail$Date)
data.Quantity <- aggregate(Quantity ~ Product.Type + Country + Product.Name, data=retail, sum)
data.Total <- aggregate(Total ~ Product.Type + Country + Product.Name, data=retail, sum)

# merge two data frames by Product.Type and Country
dqt <- merge(data.Quantity,data.Total,by=c("Product.Type","Country","Product.Name"))

#view(dqt)
retail$Time <- sort(retail$Time, decreasing = FALSE)
retail$Month <- format(as.Date(retail$Date), "%m")

#Create UI
ui <- tagList(pageWithSidebar(
  
  headerPanel("Uncover Insights: Analyzing Product Behaviour and Market Basket Analysis for retail Industry Globally within 6 months."),
  
  shinythemes::themeSelector(),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "ROI: Return of Insights", #Group's title
    tabPanel("Overview", #First Navigation Tab
             sidebarPanel(
               pickerInput("CountryInput1","Select Country:", choices=sort(unique(retail$Country)), options = list(`actions-box` = TRUE),multiple = T) #Enable multi-selection slicer
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Sales Overview", plotOutput("overview1", width = 1000, height = 500)) #First Tab
               )
             )
    ),
    tabPanel("Time Analysis", #Second Navigation Tab
             sidebarPanel(
               dateRangeInput("DateInput", sort(unique(retail$Date)), min = min(retail$Date), max = max(retail$Date), range(min(retail$Date), max(retail$Date)), label = "Date range:"), #Date Range
               pickerInput("CountryInput","Select Country:", choices=sort(unique(retail$Country)), options = list(`actions-box` = TRUE),multiple = T) #Country list
               
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Sales by Time Overview", #First tab for Time Analysis Nav. Tab
                          plotOutput("plot5", width = 800, height = 300), #plot output
                          br(),
                          br(),
                          plotOutput("plot4", width = 800, height = 300), #plot output
                          br(),
                          br()
                 ),
                 tabPanel("Customer Segmentation", #Second tab for Time Analysis Nav. Tab
                          plotOutput("plot6", width = 800, height = 300),
                          br(),
                          plotOutput("plot3", width = 800, height = 300)
                 )
               )
             )
    ),
    tabPanel("Product Analysis", #Third Navigation Tab
             sidebarPanel(
               pickerInput("CountryInput3","Select Country:", choices=sort(unique(dqt$Country)), options = list(`actions-box` = TRUE),multiple = T), #To select country
               pickerInput("TypeInput1", "Select Product(s):", choices=sort(unique(dqt$Product.Type)), options = list(`actions-box` = TRUE),multiple = T) #To select product
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Product Overview", #First tab for Product Analysis Nav. Tab
                          br(),
                          plotlyOutput("plot", width = 800, height = 800),
                          br(),
                          plotlyOutput("plot10", width = 800, height = 800)
                          
                 ),
                 tabPanel("Data Table", dataTableOutput("table")) #Second tab for Product Analysis Nav. Tab
                 #tabPanel("Tab 3", "This panel is intentionally left blank")
               )
             )
    ),
    
    tabPanel("Market Basket", #Fourth Navigation Tab
             sidebarPanel(
               selectInput("TypeInput2", "Select Product Type(s):", choices = tr@itemInfo$labels, multiple = FALSE),
               sliderInput("supp", label = "Support:", min = 0, max = 0.53, value = 0.001, step = 1/10000),
               sliderInput("conf", label = "Confidence:", min = 0, max = 0.53, value = 0.001, step = 1/10000)
             ),
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel('Market Basket (LHS Variable)', dataTableOutput("lhsTable")),
                           tabPanel('Market Basket (RHS Variable)', dataTableOutput("rhsTable"))
               )
             )
    )
    
  )
)
)



#Create Server
server <- function(input, output, session) {
  
  # Filter for Sales Overview
  
  filtered12 <- reactive({ 
    retail %>%
      filter (Country %in% input$CountryInput1)
  })
  
   # Output for Sales Overview
  output$overview1 <- renderPlot({
    ggplot(filtered12(), aes(x = Country, y=Total)) +
      geom_bar(stat = "identity", color="gold", fill="gold") + 
      theme(axis.title.x =  element_blank())+
      labs(title="Revenue based on country")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
   # Filter for Time Analysis
  filtered2 <- reactive({
    retail %>%
      filter (Date >= input$DateInput[1],
              Date <= input$DateInput[-1],
              Country %in% input$CountryInput) %>% 
      group_by(Country, Time) %>% 
      summarise(numOfCust = n_distinct(CustomerID))
  })
  
  filtered3 <- reactive({
    retail %>%
      filter (Date >= input$DateInput[1],
              Date <= input$DateInput[-1],
              Country %in% input$CountryInput) %>% 
      group_by(Country, Time) %>% 
      summarise(Revenuebytime = sum(Total))
  })
  
  
  filtered5 <- reactive({
    retail %>%
      filter (Country %in% input$CountryInput) %>% 
      group_by(Country, Month) %>% 
      summarise(Revenuebymonth = sum(Total))
  })
  
  filtered6 <- reactive({
    retail %>%
      filter (Country %in% input$CountryInput) %>% 
      group_by(Country, Month) %>% 
      summarise(numOfcustbymonth = n_distinct(CustomerID))
  })
  
  #Output for Time Analysis
  output$plot3 <- renderPlot({
    ggplot(filtered2(), aes(x=Time, y=numOfCust, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Customer Count Across Time")+
      xlab("Time") + ylab("Number of customers") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })
  
  output$plot4 <- renderPlot({
    ggplot(filtered3(), aes(x=Time, y=Revenuebytime, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Revenue Across Time") +
      xlab("Time") + ylab("Revenue") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })
  
  output$plot5 <- renderPlot({
    ggplot(filtered5(), aes(x=Month, y=Revenuebymonth, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Monthly Revenue") +
      xlab("Month") + ylab("Revenue") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })
  
  output$plot6 <- renderPlot({
    ggplot(filtered6(), aes(x=Month, y=numOfcustbymonth, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Monthly Customer Count")+
      xlab("Month") + ylab("Number of customers") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_y_continuous(labels = scales::comma)
  })
 
   # Filter for Product Analysis
  filtered <- reactive({
    dqt %>%
      filter (Country %in% input$CountryInput3,
              Product.Type %in% input$TypeInput1)
  }) 
  
  filtered7 <- reactive({
    dqt %>%
      filter (Country %in% input$CountryInput3,
              Product.Type %in% input$TypeInput1)%>%
      group_by(Product.Type,Country) %>% 
      summarise(sumTotal=sum(Total))
    
  })
  
  filtered8 <- reactive({
    dqt %>%
      filter (Country %in% input$CountryInput3,
              Product.Type %in% input$TypeInput1)%>% 
      group_by(Product.Type,Country) %>% 
      summarise(sumQuantity=sum(Quantity))
    
  })
  
  # Plot Output for Total Revenue
  
  colourCount = length(unique(retail$Country))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  output$plot <- renderPlotly({
    
    ggplot(filtered7(), aes(reorder(Product.Type,sumTotal), y=sumTotal, fill=Country)) +
      geom_bar(stat="identity",color="black", width=0.5)+
      theme_classic(15)+
      #geom_text(aes(label=Total), vjust=1.6, size=3.5)+
      labs(title="Total revenue of product sold based on Country")+ 
      xlab("Type of Product") + ylab("Total Revenue") + scale_fill_manual(values = getPalette(colourCount))+
      coord_flip() + scale_y_continuous(labels = scales::comma)
  })
  
  # plot Output for Total Quantity
  
  output$plot10 <- renderPlotly({
    ggplot(filtered8(), aes(reorder(Product.Type,sumQuantity), y=sumQuantity, fill=Country)) +
      geom_bar(stat="identity" ,color="black", width=0.5) + 
      theme_classic(15)+
      #geom_text(aes(label=Quantity), vjust=1.6, size=3.5)+
      labs(title="Quantity of product sold based on Country")+ 
      xlab("Type of Product") + ylab("Quantity") +  scale_fill_manual(values = getPalette(colourCount))+ coord_flip()
  })
  
  #Table Output
  
  output$table <- renderDataTable({
    filtered () 
  })
  
   # Output for Market Basket LHS
  output$lhsTable <- renderDataTable({
    rules <- apriori(tr, parameter = list(supp=input$supp, conf=input$conf),appearance = list(lhs=input$TypeInput2,default="rhs"))
    ruledf = data.frame(
      rule = paste(labels(lhs(rules)),paste(labels(rhs(rules)),'\n',sep = ''),sep = " =>\n "),
      rules@quality)
    #inspect(sort(rules, by = "lift"))
    ruledf$rule <- as.character(ruledf$rule)
    ruledf <- ruledf %>% filter(str_detect(rule,"\\{\\}") == FALSE)
  })
  
  # Output for Market Basket RHS
  output$rhsTable <- renderDataTable({
    rules <- apriori(tr, parameter = list(supp=input$supp, conf=input$conf),appearance = list(rhs=input$TypeInput2,default="lhs"))
    ruledf1 = data.frame(
      rule = paste(labels(lhs(rules)),paste(labels(rhs(rules)),'\n',sep = ''),sep = " =>\n "),
      rules@quality)
    #inspect(sort(rules, by = "lift"))
    ruledf1$rule <- as.character(ruledf1$rule)
    ruledf1 <- ruledf1 %>% filter(str_detect(rule,"\\{\\}") == FALSE)
  })
  
}

shinyApp(ui, server)
