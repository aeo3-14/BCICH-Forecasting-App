library(shinythemes)
library(shinyjs)
library(readxl)
library(shinymanager)
library(shinyjs)
library(shinyWidgets)
library(shiny)
library(DT)
# Define UI
# Change language
set_labels(
  language = "en",
  "Please authenticate" = "Log In",
  "Username:" = "Username:",
  "Password:" = "Password:",
  "Login"= "Log In"
)

ui <-navbarPage("BCICH Patient Admission Forecasting App",
                tags$head(tags$style(HTML('.progress-bar {background-color: red;}'))),
                tags$head(tags$style(HTML("
                  .main-panel {
                    margin-left: 20px;
                  }
                "))),
                theme = shinytheme("flatly"),
                useShinyjs(),
                tabPanel("Forecast",
                         sidebarLayout(
                           sidebarPanel(
                             
                             selectInput("patient_type", "Select Patient Type:",
                                         choices = c("Medical Patient", "Obstetrics Patient")),
                            
                             numericInput("num_years", HTML("Number of Ahead Months to Forecast:<br>(Min: 1, Max: 84)"), value = 1, min = 1, max = 84),
                             tags$p("Note: Short term forecast is the recommended forecast"),
                             actionButton("forecast_button", "Start Forecast",style = "display: block; margin-left: auto; margin-right: 0;"),
                             
                             width = 3  # Adjust this value to resize the sidebar
                           ),
                           hidden(  # Initially hide the main panel
                             mainPanel(
                               width = 8, 
                               id = "main_panel",  # Add an ID to the main panel for reference
                               
                              
                               conditionalPanel(
                                 condition = "output.progress_text == 'Forecast completed!'",
                                 downloadButton("download_forecast", "Download Forecasted Values",style = "margin-bottom: 20px;"),
                                 downloadButton("downloadPDF", "Download PDF Report",style = "margin-bottom: 20px;"),
                               ),
                               textOutput("progress_text"),
                               plotOutput("forecast_plot"),
                               dataTableOutput("forecast_table"),
                               
                             ) 
                           )),),
                
                tabPanel("Dataset", 
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("patient_type_dataset", "Select Patient Type:",
                                         choices = c("Medical Patient", "Obstetrics Patient")),
                             width = 3
                           ),
                           # Initially hide the main panel
                           mainPanel(
                             width = 8, 
                             
                             id = "main_panel",  # Add an ID to the main panel for reference
                             downloadButton("download_dataset", "Download Dataset Values",style = "margin-bottom: 20px;"),
                             actionButton("add_button", "Add New Value",style = "margin-bottom: 20px;"),
                             actionButton("update_button", "Upload New File",style = "margin-bottom: 20px;"),
                             uiOutput("dynamic_ui",style = "margin-bottom: 20px;"),
                             plotOutput("dataset_plot"),
                             dataTableOutput("dataset_table"),
                           ) 
                         ),
                ),
                tabPanel("Test", 
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("patient_type_dataset_for_test", "Select Patient Type:",
                                         choices = c("Medical Patient", "Obstetrics Patient")),
                             airDatepickerInput("training_start_date",
                                                label = "Select Training Start Date:",
                                                value = "1987-01",
                                                #maxDate = "2022-12",
                                                minDate = "1987-01",
                                                view = "months", #editing what the popup calendar shows when it opens
                                                minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy-MM",
                                                
                                              
                             ),
                             airDatepickerInput("training_end_date",
                                                label = "Select Training End Date:",
                                                value = "2008-12",
                                                # maxDate = "2016-08-01",
                                                minDate = "1987-01",
                                                view = "months", #editing what the popup calendar shows when it opens
                                                minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy-MM"
                             ),
                             airDatepickerInput("testing_start_date",
                                                label = "Select Testing Start Date:",
                                                value = "2009-01",
                                                # maxDate = "2016-08-01",
                                                minDate = "1987-01",
                                                view = "months", #editing what the popup calendar shows when it opens
                                                minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy-MM"
                             ),
                             airDatepickerInput("testing_end_date",
                                                label = "Select Testing End Date:",
                                                value = "2015-12",
                                                # maxDate = "2016-08-01",
                                                minDate = "1987-01",
                                                view = "months", #editing what the popup calendar shows when it opens
                                                minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy-MM"
                             ),
                             
                             
                             
                             #dateInput("training_start_date", "Select Training Start Date:", value = "1987-01-01", format = "mm-yyyy"),
                             #dateInput("training_end_date", "Select Training End Date:", value = "2008-12-01", format = "mm-yyyy"),
                             #dateInput("testing_start_date", "Select Testing Start Date:", value = "2009-01-01", format = "mm-yyyy"),
                             #dateInput("testing_end_date", "Select Testing End Date:", value = "2015-12-01", format = "mm-yyyy"),
                            
                             actionButton("start_test", "Start Test",style = "display: block; margin-left: auto; margin-right: 0;"),
                             width = 3
                           ),
                           # Initially hide the main panel
                           mainPanel(
                             width = 9, 
                             plotOutput("forecast_plot_for_test"),
                             tableOutput("performance_table")
                           ) 
                         ),
                )
                
                
                
)

# Wrap UI with secure_app
ui <- secure_app(ui, enable_admin = TRUE, new_text = "Log In")

