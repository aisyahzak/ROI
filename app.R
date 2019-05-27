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
#library('rpanel')
library('chron')
library('plotly')
library('shinythemes')
library(readxl)
library(htmlwidgets)
library(scales)

#setwd("C:/Users/aisyahzak/Desktop")
retail = read.csv("retail.csv", stringsAsFactors = F)
tr <- read.transactions("market_basket_type.csv", sep = ',')

retail$Total <- as.numeric(as.character(retail$Total))
retail$Date <- as.Date(retail$Date)
data.Quantity <- aggregate(Quantity ~ Product.Type + Country + Product.Name, data=retail, sum)
data.Total <- aggregate(Total ~ Product.Type + Country + Product.Name, data=retail, sum)
# merge two data frames by Product.Type and Country
dqt <- merge(data.Quantity,data.Total,by=c("Product.Type","Country","Product.Name"))
retail$Time <- sort(retail$Time, decreasing = FALSE)
retail$Month <- format(as.Date(retail$Date), "%m")

ui <- tagList(pageWithSidebar(
  
  headerPanel("Uncover Insights: Analyzing Product Behaviour and Market Basket Analysis for retail Industry Globally within 6 months."),
  
  shinythemes::themeSelector(),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "ROI: Return of Insights",
    tabPanel("Overview",
             sidebarPanel(
               pickerInput("CountryInput1","Select Country:", choices=sort(unique(retail$Country)), options = list(`actions-box` = TRUE),multiple = T)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Sales Overview", plotOutput("overview1", width = 1000, height = 500))
               )
             )
    ),
    tabPanel("Time Analysis",
             sidebarPanel(
               dateRangeInput("DateInput", sort(unique(retail$Date)), min = min(retail$Date), max = max(retail$Date), range(min(retail$Date), max(retail$Date)), label = "Date range:"),
               pickerInput("CountryInput","Select Country:", choices=sort(unique(retail$Country)), options = list(`actions-box` = TRUE),multiple = T)
               
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Sales by Time Overview", plotOutput("plot5", width = 800, height = 300),
                          br(),
                          br(),
                          plotOutput("plot4", width = 800, height = 300),
                          br(),
                          br()
                 ),
                 tabPanel("Customer Segmentation", 
                          plotOutput("plot6", width = 800, height = 300),
                          br(),
                          plotOutput("plot3", width = 800, height = 300)
                 )
               )
             )
    ),
    tabPanel("Product Analysis", 
             sidebarPanel(
               pickerInput("CountryInput3","Select Country:", choices=sort(unique(dqt$Country)), options = list(`actions-box` = TRUE),multiple = T),
               pickerInput("TypeInput1", "Select Product(s):", choices=sort(unique(dqt$Product.Type)), options = list(`actions-box` = TRUE),multiple = T)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Product Overview",
                          br(),
                          plotOutput("plot", width = 800, height = 800),
                          br(),
                          plotOutput("plot10", width = 800, height = 800)
                          
                 ),
                 tabPanel("Data Table", dataTableOutput("table"))
                 #tabPanel("Tab 3", "This panel is intentionally left blank")
               )
             )
    ),
    
    tabPanel("Market Basket",
             sidebarPanel(
               selectInput("TypeInput2", "Select Product Type(s):", choices = tr@itemInfo$labels, multiple = FALSE),
               sliderInput("supp", label = "Support:", min = 0, max = 1, value = 0.001, step = 1/10000),
               sliderInput("conf", label = "Confidence:", min = 0, max = 1, value = 0.001, step = 1/10000)
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




server <- function(input, output) {

  # Plot Output for Total Revenue
  
  filtered12 <- reactive({ 
    retail %>%
      filter (Country %in% input$CountryInput1)
    })
  
  output$overview1 <- renderPlot({
    ggplot(filtered12(), aes(x = Country, y=Total)) +
      geom_bar(stat = "identity", color="gold", fill="gold") + 
      theme(axis.title.x =  element_blank())+
      labs(title="Revenue based on country")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

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
  filtered <- reactive({
    #if (is.null(input$TypeInput)) {
    # return(NULL)
    #}    
    dqt %>%
      filter (Country %in% input$CountryInput3,
              Product.Type %in% input$TypeInput1)
  }) 
  
  # Plot Output for Total Revenue
  
  output$plot <- renderPlot({
    
    ggplot(filtered(), aes(reorder(Product.Type,Total), y=Total, fill=Country)) +
      geom_bar(stat="identity", width=0.5)+
      theme_classic(15)+
      #geom_text(aes(label=Total), vjust=1.6, size=3.5)+
      labs(title="Total revenue of product sold based on Country")+ 
      xlab("Type of Product") + ylab("Total Revenue") + scale_fill_hue(c=45, l=80)+ coord_flip()
  })
  
  # plot Output for Total Quantity
  
  output$plot10 <- renderPlot({
    ggplot(filtered(), aes(reorder(Product.Type,Quantity), y=Quantity, fill=Country)) +
      geom_bar(stat="identity" , width=0.5) + 
      theme_classic(15)+
      #geom_text(aes(label=Quantity), vjust=1.6, size=3.5)+
      labs(title="Quantity of product sold based on Country")+ 
      xlab("Type of Product") + ylab("Quantity") + scale_fill_hue(c=45, l=80)+ coord_flip()
  })
  
  #Table Output
  
  output$table <- renderDataTable({
    filtered () 
  })
  
  output$lhsTable <- renderDataTable({
    rules <- apriori(tr, parameter = list(supp=input$supp, conf=input$conf),appearance = list(lhs=input$TypeInput2,default="rhs"))
    ruledf = data.frame(
      rule = paste(labels(lhs(rules)),paste(labels(rhs(rules)),'\n',sep = ''),sep = " =>\n "),
      rules@quality)
    #inspect(sort(rules, by = "lift"))
    ruledf$rule <- as.character(ruledf$rule)
    ruledf <- ruledf %>% filter(str_detect(rule,"\\{\\}") == FALSE)
  })
  
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
