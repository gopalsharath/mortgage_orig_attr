#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Mortgage attributes across top 20 metros"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("attr", "Attribute:",
                        choices = tmp_attr[,1]),
        hr(),
        helpText("Freddie Mac Single Family Loan Level Dataset, 2020:Q1 -  2020:Q3")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("AttPlot")
        )
    )
))
