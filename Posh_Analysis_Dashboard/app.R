#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app was made by Christian Guthrie for his wife.
#

# Styled with Styler, linted with lintr
library(shiny)
library(DT)
library(shinydashboard)
library(scales)
library(googlesheets4)
library(ggplot2)
library(formattable)


# Read in the data
#   - Had to rename columns to be one word
#   - Skipped first row since we are renaming columns
#   - Used 'col_types' mainly to specify Date columns (c=char, d=double, D=Date, i=int)
expenses <- read_sheet("https://docs.google.com/spreadsheets/u/1/d/1DOzDyygNayNuatQe6_xf4MF9jnkANXmkjmAp3d-v5AI/edit?usp=sharing_eip&ts=5fdf99d4",
                       col_names = c(
                           "Item_Description", "Cost", "Sale_Price", "Net", "Listed_Date", "Sold_Date",
                           "Days_To_Sell", "Brand", "Type", "Gender", "Platform"
                       ),
                       col_types = "cdddDDicccc",
                       skip = 1
)

# Used to separate variable choices in the dropdown
continuous_variables <- c("Cost" = "Cost", "Sale Price" = "Sale_Price", "Net" = "Net", "Days To Sell" = "Days_To_Sell")
discrete_variables <- c(
    "Listed Date" = "Listed_Date", "Sold Date" = "Sold_Date", "Brand" = "Brand",
    "Type" = "Type", "Gender" = "Gender", "Platform" = "Platform"
)



# Define UI for Posh Analysis Application
ui <- dashboardPage(
    
    # Application title
    dashboardHeader(title = "Posh Analysis Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview",
                     tabName = "overview", icon = icon("dashboard"),
                     badgeLabel = "new", badgeColor = "green"
            ),
            menuItem("Data", tabName = "data", icon = icon("briefcase")),
            menuItem("Winnie", tabName = "winnie", icon = icon("home"))
        )
    ),
    
    # Define each menuItem's content
    dashboardBody(
        tabItems(
            # Pic of Winnie
            tabItem(
                tabName = "winnie",
                img(src = "winnie.jpeg", height = "50%", width = "50%")
            ),
            # Main page
            tabItem(
                tabName = "overview",
                fluidRow(
                    valueBoxOutput("cost"),
                    valueBoxOutput("sale"),
                    valueBoxOutput("net")
                ),
                fluidRow(
                    box(width = 4, radioButtons("radio",
                                                label = "",
                                                choices = c("Discrete", "Continuous")
                    ))
                ),
                fluidRow(
                    conditionalPanel(
                        "input.radio == 'Discrete'",
                        box(
                            width = 12, height = 100,
                            fluidRow(
                                column(4, selectInput("discrete_col",
                                                      "Variable:",
                                                      discrete_variables,
                                                      selected = "Gender"
                                ))
                            )
                        )
                    ),
                    conditionalPanel(
                        "input.radio == 'Continuous'",
                        box(
                            width = 12, height = 200,
                            fluidRow(
                                column(4, selectInput("continuous_col",
                                                      "Variable:",
                                                      continuous_variables,
                                                      selected = "Days_To_Sell"
                                ))
                            ),
                            fluidRow(
                                column(8, sliderInput("binwidth",
                                                      "Number of bins:",
                                                      min = 0,
                                                      max = 30,
                                                      value = 1,
                                                      step = 5
                                ))
                            )
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 12, plotOutput("histogram")
                    )
                ),
                fluidRow(
                    box(
                        width = 12, verbatimTextOutput("summary")
                    )
                )
            ),
            # Easy way to look through data without going to google doc
            tabItem(
                tabName = "data",
                dataTableOutput("dataTable")
            )
        )
    )
)



# Define server logic required to run app
server <- function(input, output) {
    
    # Creates 2-way table for discrete Overview page
    createTable <- reactive({
        newTable <- table(expenses[[input$discrete_col]], !is.na(expenses$Sale_Price))
        colnames(newTable) <- c("Unsold", "Sold")
        prop.table(newTable, margin = 1)
    })
    
    # Outputs discrete 2-way table
    output$table <- renderPrint(
        createTable()
    )
    
    
    # Totals for the Overview boxes below
    totalCost <- sum(expenses$Cost, na.rm = TRUE)
    totalSalePrice <- sum(expenses$Sale_Price, na.rm = TRUE)
    totalNet <- sum(expenses$Net, na.rm = TRUE)
    # totalProfit <- sum(expenses$Sale_Price, na.rm = TRUE) * 0.8
    
    # Overview top row boxes to show total cost, sale price, and net gain
    output$cost <- renderValueBox(
        valueBox(
            scales::dollar(totalCost), "Cost",
            icon = icon("thumbs-down", lib = "glyphicon"),
            color = "red"
        )
    )
    output$sale <- renderValueBox(
        valueBox(
            scales::dollar(totalSalePrice), "Sale Price",
            icon = icon("hourglass", lib = "glyphicon"),
            color = "yellow"
        )
    )
    output$net <- renderValueBox(
        valueBox(
            scales::dollar(totalNet), "Net",
            icon = icon("thumbs-up", lib = "glyphicon"),
            color = "green"
        )
    )
    
    
    # Histogram based on Discrete/Continuous radio button
    output$histogram <- renderPlot(
        switch(input$radio,
               "Continuous" = ggplot(
                   expenses,
                   aes_string(x = input$continuous_col)
               ) +
                   geom_histogram(binwidth = input$binwidth, fill = "blue") +
                   stat_bin(binwidth = input$binwidth, geom = "text", colour = "black", size = 3.5, aes(label = ..count..)) +
                   xlab(input$continuous_col) +
                   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                   scale_x_continuous(breaks = round(seq(min(expenses[[input$continuous_col]], na.rm = TRUE),
                                                         max(expenses[[input$continuous_col]], na.rm = TRUE),
                                                         by = input$binwidth
                   ), 1)),
               
               "Discrete" = ggplot(
                   expenses,
                   aes_string(x = input$discrete_col)
               ) +
                   stat_count(fill = "blue") +
                   xlab(input$discrete_col) +
                   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                   geom_text(stat = "count", aes(label = ..count..), vjust = -1, size = 4)
        )
    )
    
    
    # 2-way table for discrete or 5 number summary for continuous based on radio button
    output$summary <- renderPrint(
        switch(input$radio,
               "Continuous" = summary(expenses[, input$continuous_col]),
               "Discrete" = createTable()
        )
    )
    
    
    # DataTable for Data page
    output$dataTable <- renderDataTable(
        as.datatable(
            formattable(
                expenses, list(
                    Cost = color_tile("white", "red"),
                    "Sale Price" = color_tile("white", "green"),
                    "Days To Sell" = color_tile("white", "yellow"),
                    Gender = formatter("span", style = x ~ style(color = ifelse(x == "Male", "blue", "hotpink"))),
                    Platform = formatter("span", style = x ~ style(color = ifelse(x == "Poshmark", "maroon", "lightblue")))
                )
            )
        )
    )
}

# Run the application
shinyApp(ui = ui, server = server)
