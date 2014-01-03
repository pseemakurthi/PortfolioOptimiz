library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Input and Submit"),

  sidebarPanel(

#   checkboxInput("HDFC Bank", "HDFC", FALSE),
#   checkboxInput("TCS", "TCS", FALSE),
#   checkboxInput("RIL", "RIL", FALSE),
#   checkboxInput("ITC", "ITC", FALSE),
#   checkboxInput("TataMotors", "Tata Motors", FALSE),
#   checkboxInput("DrReddy", "Dr Reddy's", FALSE),
#   checkboxInput("COAL", "Coal India", FALSE),
#   checkboxInput("LT", "Larsen & Turbo", FALSE),
#   checkboxInput("Ultratech", "Ultratech", FALSE),
#   checkboxInput("Airtel", "Bharthi Airtel", FALSE),
#   checkboxInput("TataSteel", "Tata Steel", FALSE),

   selectInput("stock1", "Select Stock 1:", 
                choices = c("HDFC", "TCS", "RIL", "ITC", "Tata Motors", "Dr Reddy", "Coal India", "L&T", "UltraTech", "Airtel", "Tata Steel")),

   selectInput("stock2", "Select Stock 2:", 
                choices = c("HDFC", "TCS", "RIL", "ITC", "Tata Motors", "Dr Reddy", "Coal India", "L&T", "UltraTech", "Airtel", "Tata Steel")),

   selectInput("stock3", "Select Stock 3:", 
                choices = c("HDFC", "TCS", "RIL", "ITC", "Tata Motors", "Dr Reddy", "Coal India", "L&T", "UltraTech", "Airtel", "Tata Steel")),

   selectInput("stock4", "Select Stock 4:", 
                choices = c("HDFC", "TCS", "RIL", "ITC", "Tata Motors", "Dr Reddy", "Coal India", "L&T", "UltraTech", "Airtel", "Tata Steel")),

   selectInput("stock5", "Select Stock 5:", 
                choices = c("HDFC", "TCS", "RIL", "ITC", "Tata Motors", "Dr Reddy", "Coal India", "L&T", "UltraTech", "Airtel", "Tata Steel")),


#   numericInput("frmDate", "From Date:", 20110101),
#   numericInput("toDate", "To Date:", 20131101),

    dateRangeInput("dateRange", "Date Range", start = "2011-01-01", end = "2013-11-01",
    min = NULL, max = NULL, format = "yyyy-mm-dd",
    startview = "month", weekstart = 0, language = "en",
    separator = " to "),

   selectInput("interval", "Stock Interval:", 
                choices = c("Daily", "Weekly", "Montly")),

    sliderInput("invest", "Initial Investment:", 
                min = 0, max = 100000, value = 0, step = 5000,
                format="#,##0", locale="us", animate=TRUE),

   submitButton("Submit")


  ),

  mainPanel(


    tabsetPanel(
      tabPanel("PlotS", plotOutput("allstocksplot"), plotOutput("allstocksboxplot"),
                        h3(textOutput("caption1")), plotOutput("stock1plot"), 
                        h3(textOutput("caption2")), plotOutput("stock2plot"),
                        h3(textOutput("caption3")), plotOutput("stock3plot"),
                        h3(textOutput("caption4")), plotOutput("stock4plot"),
                        h3(textOutput("caption5")), plotOutput("stock5plot")
              ),
      tabPanel("Freq & Prob Distributions", dataTableOutput("allstocksCorMat"),
                                            dataTableOutput("stock1ProbMat"),
                                            dataTableOutput("stock2ProbMat"),
                                            dataTableOutput("stock3ProbMat"), 
                                            dataTableOutput("stock4ProbMat"),
                                            dataTableOutput("stock5ProbMat")
              ),
      tabPanel("Risk & Return", plotOutput("riskreturnplot")
              ),
      tabPanel("Risk & Return (Monte-Carlo)", plotOutput("riskreturnMCplot")
              ),
      tabPanel("VaR-Each Stock", dataTableOutput("varEachStock")
              ),
      tabPanel("VaR-Portfolio", dataTableOutput("varPortfolio")
              )
    )

#    h3(textOutput("caption1")),

#    plotOutput("stock1plot"),

#    h3(textOutput("caption2")),

#    plotOutput("stock2plot"),

#    h3(textOutput("caption3")),

#    plotOutput("stock3plot"),

#    h3(textOutput("caption4")),

#    plotOutput("stock4plot"),

#    h3(textOutput("caption5")),

#    plotOutput("stock5plot")




  )
))
