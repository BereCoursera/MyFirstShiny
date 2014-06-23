library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Kernel density plots for mpg"),
        sidebarPanel(
                checkboxGroupInput("chkGear", "Gears",
                                   c("3 Gears" = "3",
                                     "4 Gears" = "4",
                                     "5 Gears" = "5"),c("3","4","5")),
                checkboxGroupInput("chkCylinders", "Cylinders",
                                   c("4 Cylinders" = "4",
                                     "6 Cylinders" = "6",
                                     "8 Cylinders" = "8"),c("4","6","8"))
                
        ),
        
        mainPanel(
                #includeHTML("graph.js"),
                h4('You choose'),
                verbatimTextOutput("caption"),
                plotOutput('newHist')
        )
))
