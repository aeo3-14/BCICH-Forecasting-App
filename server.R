library(openxlsx)
library(writexl)
library(shinyalert)
library(shinymanager)
library(keyring)
library(forecast)
library(zoo)
library(xts)
library(gridExtra) 
library(ggplot2)
library(grid)
# define some basic credentials (on data.frame)
credentials <- data.frame(
  user = c("bcich", "bcichadmin"), # mandatory
  password = c("Hospital101", "Admin158"), # mandatory
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)
# Set Database Encryption Key
#keyring::key_set("R-shinymanager-key", "obiwankenobi")
services <- "R-shinymanager-key"
usernames <- "obiwankenobi"
passwords <- "martin"  # replace 'your_password' with your actual password
keyring_create("system", password="secret")
keyring_unlock(keyring = "system", password = "secret") 
key_set_with_value(service=services, username=usernames, password=passwords)

# Initialize the Database
create_db(
  credentials_data = credentials,
  sqlite_path = "database.sqlite",
  passphrase = key_get(services, usernames)
  
)
# Define server logic
server <- function(input,output, session) {
  
  
  
  # Define reactiveValurocesss to store forecast results
  forecast_values <- reactiveValues()
  data <- reactiveValues()
  train_values <- reactiveValues()
  
  # Read data
  medical_data <- reactive({
    req(input$patient_type)
    data=read_excel("data/Medical Patient.xlsx")  # Replace "medical_data.xlsx" with your medical patient dataset file path
    
    data$date <- as.Date(data$date, format = "%m/%Y") # Adjust the format according to your actual date format
    return(data)
    
  })
  
  obstetrics_data <- reactive({
    req(input$patient_type)
    data=read_excel("data/Obstetrics Patient.xlsx")  # Replace "obstetrics_data.xlsx" with your obstetrics patient dataset file path
    
    data$date <- as.Date(data$date, format = "%m/%Y") # Adjust the format according to your actual date format
    return(data)
  })
  
  # Read data for dataset display
  medical_data_dataset <- reactiveFileReader(
    intervalMillis = 1000, # Check for changes every 5 seconds
    filePath = "data/Medical Patient.xlsx",
    session = session,
    readFunc = function(file) {
      data = read_excel(file)
      data$date <- as.Date(data$date, format = "%m/%Y") # Adjust the format according to your actual date format
      return(data)
    }
  )
  
  obstetrics_data_dataset <- reactiveFileReader(
    intervalMillis = 1000, # Check for changes every 5 seconds
    filePath = "data/Obstetrics Patient.xlsx",
    session = session,
    readFunc = function(file) {
      data = read_excel(file)
      data$date <- as.Date(data$date, format = "%m/%Y") # Adjust the format according to your actual date format
      return(data)
    }
  )
  
  
  
 
  # Create forecast function for Medical Patient using ANN
  forecast_medical <- function(data, num_years, save_model = TRUE) {
    # Load the model if it exists
    if (file.exists("nnetar_model.rds")) {
      fit_m <- readRDS("nnetar_model.rds")
    } else {
      # Train the model if it doesn't exist
      ts_data <- ts(data$"Medical.Patient", start = c(1987, 1), frequency = 12)
      fit_m <- nnetar(log10(ts_data), p = 2, P = 1, repeats = 500, size = 4, learningrate = 0.001)
      # Save the model
      saveRDS(fit_m, "nnetar_model.rds")
    }
    
    forecast_values <- forecast(fit_m, h = num_years)
    forecast_values$mean <- 10^forecast_values$mean
    forecast_values$x <- 10^forecast_values$x
    
    return(forecast_values)
  }
  
  
  # Create forecast function for Obstetrics Patient using Simple Exponential Smoothing
  forecast_obstetrics <- function(data, num_years) {
    ts_data <- ts(data$"Obstetrics.Patient",start = c(1987, 1), frequency = 12)
    fit_ob <- ses(log10(ts_data),alpha = 0.01, h =  num_years,level=95)
    forecast_values <- forecast(fit_ob)
    forecast_values$mean <- 10^forecast_values$mean
    forecast_values$lower <- 10^forecast_values$lower
    forecast_values$upper <- 10^forecast_values$upper
    forecast_values$x <- 10^forecast_values$x
    
    return(forecast_values)
  }
  
  # Function to perform forecast based on user selection
  observeEvent(input$forecast_button, {
    withProgress(message = "Forecasting in progress...", value = 0, {
      incProgress(0.2)
      if (input$patient_type == "Medical Patient") {
        forecast_values$forecast <- forecast_medical(medical_data(), input$num_years)
        
      } else {
        forecast_values$forecast <- forecast_obstetrics(obstetrics_data(), input$num_years)
      }
     
      Sys.sleep(3) # Simulating some processing time
      incProgress(1, detail = "Forecast completed.")
    })
  })
 
  # Create plot
  output$forecast_plot <- renderPlot({
    plot_function(input$patient_type, forecast_values)
  })
  
    

  
  plot_function <- function(patient_type, forecast_values) {
    if (patient_type == "Medical Patient") {
      plot_obj <- plot(forecast_values$forecast, type = "l", xlab = "Date", ylab = "Monthly Values", main = "Forecast of Medical Patient Admission using ANN", col = "black", panel.first = rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#D4F1F4"))
      legend("topleft", legend = c("Forecasted Values","Training Values/Actual Values"), col = c("blue","black"), lty = 1, cex = 0.8)
    } else {
      plot_obj <- plot(forecast_values$forecast, type = "l", xlab = "Date", ylab = "Monthly Values", main = "Forecast of Obstetrics Patient Admission using Simple Exponential Smoothing", col = "black", panel.first = rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#D4F1F4"))
      legend("topleft", legend = c("Forecasted Values","Training Values/Actual Values"), col = c("blue","black"), lty = 1, cex = 0.8)
    }
    return(plot_obj)
  }
  
  
  
  
  
  
  # Display progress text
  output$progress_text <- renderText({
    if (is.null(forecast_values$forecast)) {
      "Waiting for forecast to start..."
    } else {
      "Forecast completed!"
    }
  })
  
  
  # Display forecasted values in a table with dates
  output$forecast_table <- renderDataTable({
    table_function(forecast_values)
  }, options = list(pageLength = 10))  # Set the number of rows to display to 12
  
  table_function <- function(forecast_values) {
    if (!is.null(forecast_values$forecast)) {
      # Get the dates directly from the forecast object
      forecast_dates <- as.Date(as.yearmon(time(forecast_values$forecast$mean), "%Y.%m"))
      # Format the dates to show only month and year, then convert to character
      forecast_dates <- as.character(format(forecast_dates, "%m/%Y"))
      forecast_data <- data.frame(Date = forecast_dates, Forecasted_Values = round(forecast_values$forecast$mean))
      return(forecast_data)
    }
  }
  
  
  # Download forecasted values
  output$download_forecast <- downloadHandler(
    filename = function() {
      if (input$patient_type == "Medical Patient") {
        paste("medical_forecasted_values.csv", sep = "")
        
      } else {
        paste("obstetric_forecasted_values.csv", sep = "")
      }
      
    },
    content = function(file) {
      req(input$forecast_button)
      # Get the dates directly from the forecast object
      forecast_dates <- as.Date(as.yearmon(time(forecast_values$forecast$mean), "%Y.%m"))
      # Format the dates to show only month and year, then convert to character
      forecast_dates <- as.character(format(forecast_dates, "%m/%Y"))
      forecast_data <- data.frame(Date = forecast_dates, Forecasted_Values = round(forecast_values$forecast$mean))
      write.csv(forecast_data, file, row.names = FALSE)
    }
  )
  
  
  output$downloadPDF <- downloadHandler(
    filename = function() {
      "plot_output.pdf"
    },
    content = function(file) {
      pdf(file, width = 10, height = 10)  # Adjust width and height as needed
      
      # Set up the layout for the plot and table with margin
      
      
      # Plot the plot
      plot_function(input$patient_type, forecast_values)
      
      # Add space between the plot and the table
      
      par(mfrow = c(2, 1), mar = c(4, 4, 4, 4))  # 2 rows, 1 column, adjust margin as needed
      plot.new()
      # Print the table with adjusted margin
      grid.table(table_function(forecast_values), vp = viewport(height = 0.5)) # Adjust the height parameter as needed
      
      # Close the PDF device
      dev.off()
    }
  )
  

  
  
  output$download_dataset <- downloadHandler(
    filename = function() {
      if (input$patient_type_dataset == "Medical Patient") {
        paste("medical_dataset.csv", sep="")
        
      } else {
        paste("obstetrics_dataset.csv", sep="")
      }
    },
    content = function(file) {
      if (input$patient_type_dataset == "Medical Patient") {
        write.csv(medical_data_dataset(), file, row.names = FALSE)
      } else {
        write.csv(obstetrics_data_dataset(), file, row.names = FALSE)
      }
    }
  )
  
  
  
  
  observe({
    if (!is.null(forecast_values$forecast)) {
      shinyjs::enable("download_forecast")
      shinyjs::show("main_panel")
    }
  })
  
  
  
  # Display dataset
  output$dataset_table <- renderDataTable({
    if (input$patient_type_dataset == "Medical Patient") {
      data<-medical_data_dataset()
       
      
      
    } else {
      
      data<-obstetrics_data_dataset()
      
    }
    data$date <- format(data$date, "%Y-%m")
    return(data)
    
    
    
  }, options = list(pageLength = 10))  # Set the number of rows to display to 12
  
  
  # Reactive value to store the dataset
  dataset <- reactiveVal(data.frame(Date = as.Date(character()), Medical_Patient = numeric()))
  
  observeEvent(input$ok_add, {
    # Path to the Excel file
    # Write the updated dataset back to the Excel file
    if (file.exists("nnetar_model.rds")) {
      file.remove("nnetar_model.rds")
    }
    removeModal()
    if (input$patient_type_dataset == "Medical Patient") {
      
      filePath <- "data/Medical Patient.xlsx"
      
      
    } else {
      
      filePath <- "data/Obstetrics Patient.xlsx"
    }
    
    # Reactive value to store the dataset
    dataset <- reactiveVal(read_excel(filePath))
    
    
    # Read the current dataset from the Excel file
    currentData <- read_excel(filePath)
    
    # Create a new entry with the input values
    if (input$patient_type_dataset == "Medical Patient") {
      newEntry <- data.frame(date = input$newDate, "Medical Patient" = input$newPatientValue)
    } else {
      newEntry <- data.frame(date = input$newDate, "Obstetrics Patient" = input$newPatientValue)  
    }
    
    # Add the new entry to the existing dataset
    updatedData <- rbind(currentData, newEntry)
    
    # Update the reactive dataset value
    dataset(updatedData)
    
    
    
    # Write the updated dataset back to the Excel file
    tryCatch({
      write.xlsx(updatedData, filePath)
      if (input$patient_type_dataset == "Medical Patient") {
        shinyalert("Successfully", "Added new values to Medical Patient", type = "success")
      } else {
        shinyalert("Successfully", "Added new values to Obstetrics Patient", type = "success")
      }
      # If write.xlsx succeeds, show a success notification
      
      #showNotification("Added new values successfully!", type = "message")
    }, error = function(e) {
      # If write.xlsx fails, show an error notification
      if (input$patient_type_dataset == "Medical Patient") {
        shinyalert("Ops", "Failed to add new data to Medical Patient", type = "error")
      } else {
        shinyalert("Ops", "Failed to add new data to Obstetrics Patient", type = "error")
      }
      
      # showNotification("Failed to add data: ", e$message, type = "error")
    })
    updateNumericInput(session, "newPatientValue", value = 1)
    updateDateInput(session, "newDate", value = Sys.Date())
  })
  
  new_data <- reactiveVal()
  
  observeEvent(input$file1, {
    if (is.null(input$file1))
      return(NULL)
    
    df <- read_excel(input$file1$datapath)
    names(df)[1] <- "date"
    if (input$patient_type_dataset == "Medical Patient") {
      names(df)[2] <- "Medical.Patient"
    } else {
      names(df)[2] <- "Obstetrics.Patient"
      
    }
    new_data(df)
  })
  
  
  observeEvent(input$ok_over_write, {
    removeModal()
    if (file.exists("nnetar_model.rds")) {
      file.remove("nnetar_model.rds")
    }
    if (is.null(new_data()))
      return(NULL)
    if (input$patient_type_dataset == "Medical Patient") {
      tryCatch({
        write_xlsx(new_data(), "data/Medical Patient.xlsx") # Overwrite the file
        # If write.xlsx succeeds, show a success notification
        shinyalert("successfully", "Updated Medical Patient Patient dataset.", type = "success")
        #showNotification("Updated Medical Patient dataset successfully!", type = "message")
      }, error = function(e) {
        # If write.xlsx fails, show an error notification
        shinyalert("Ops", "Failed to update dataset", type = "error")
        #showNotification("Failed to update dataset: ", e$message, type = "error")
      })
      
      
      
      
    } else {
      tryCatch({
        write_xlsx(new_data(), "data/Obstetrics Patient.xlsx") # Overwrite the file
        # If write.xlsx succeeds, show a success notification
        shinyalert("successfully", "Updated Obstetrics Patient dataset.", type = "success")
        # showNotification("Updated Obstetrics Patient dataset successfully!", type = "message")
      }, error = function(e) {
        # If write.xlsx fails, show an error notification
        shinyalert("Ops", "Failed to update dataset", type = "error")
        #showNotification("Failed to update dataset: ", e$message, type = "error")
      })
      
    }
    output$dynamic_ui <- renderUI({
      tagList(
        fileInput("file1", "Choose Excel File", multiple = FALSE, accept = c(".xlsx")),
        tags$hr(),
        actionButton("overwrite", "Update Dataset File")
      )
    })
  })
  
  # Create plot for dataset
  output$dataset_plot <- renderPlot({
    if (input$patient_type_dataset == "Medical Patient") {
   
      plot(medical_data_dataset(), type = "l", xlab = "Date", ylab = "Monthly Values", main = "Medical Patient Admission Volume", col = "black",  panel.first = rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#D4F1F4"))
      legend("topleft", legend = c("Forecasted Values","Training Values/Actual Values"), col =c("blue","black"), lty = 1, cex = 0.8)
      
    } else {
      plot(obstetrics_data_dataset(), type = "l", xlab = "Date", ylab = "Monthly Values", main = "Obstetrics Patient Admission Volume", col = "black",panel.first = rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#D4F1F4"))
      legend("topleft", legend = c("Forecasted Values","Training Values/Actual Values"), col =c("blue","black"), lty = 1, cex = 0.8)
    }
    
  })
  
  observeEvent(input$submitBtn, {
    newDate <- input$newDate
    newPatientValue <- input$newPatientValue
    
    showModal(modalDialog(
      title = "Confirmation",
      paste0("New Date: ", newDate, "\n New Patient Value: ", newPatientValue, "\n\n  Are you sure you want to add these values?"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_add", "Confirm")
      )))
  })
  
  observeEvent(input$overwrite, {
    showModal(modalDialog(
      title = "Confirmation",
      "Are you sure you want to update the dataset file?",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_over_write", "Confirm")
      )
    ))
  })
  
  
  observeEvent(input$add_button, {
    output$dynamic_ui <- renderUI({
      tagList(
        #dateInput("newDate", "Date (YYYY/MM)", value = Sys.Date()),
        airDatepickerInput("newDate",
                           label = "Date (YYYY/MM)",
                           value = "2015-12",
                           # maxDate = "2016-08-01",
                           minDate = "1987-01",
                           view = "months", #editing what the popup calendar shows when it opens
                           minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                           dateFormat = "yyyy-MM"
        ),
        
        if (input$patient_type_dataset == "Medical Patient") {
          numericInput("newPatientValue", "Medical Patient Admission Volume", value = 1)
        } else {
          
          numericInput("newPatientValue", "Obstetrics Patient Admission Volume", value = 1)
        },
        
        actionButton("submitBtn", "Add")
      )
    })
  })
  
  
  observeEvent(input$update_button, {
    output$dynamic_ui <- renderUI({
      tagList(
        fileInput("file1", "Choose Excel File", multiple = FALSE, accept = c(".xlsx")),
        tags$hr(),
        
        actionButton("overwrite", "Update Dataset File")
        
        
      )
    })
  })
  training_start=reactiveValues()
  training_end=reactiveValues()
  testing_start=reactiveValues()
  testing_end=reactiveValues()
  
  observeEvent(input$start_test, {
    training_start <- as.Date(paste(input$training_start_date, sep = "-"))
    training_end <- as.Date(paste(input$training_end_date, sep = "-"))
    testing_start <- as.Date(paste(input$testing_start_date, sep = "-"))
    testing_end <- as.Date(paste(input$testing_end_date, sep = "-"))
     
    # Check for overlapping dates
    if (training_start > training_end || testing_start > testing_end) {
      showModal(modalDialog(
        title = "Error",
        "Start date cannot be greater than end date."
      ))
    } else if ( training_start==training_end || testing_start == testing_end) {
      showModal(modalDialog(
        title = "Error",
        "Start date should not be equals to the end date. Please select non-overlapping periods."
      ))
    }else if (!(training_start < training_end && testing_start < testing_end &&
               (training_end < testing_start && testing_end > training_start))) {
      showModal(modalDialog(
        title = "Error",
        "Training and testing periods overlap. Please select non-overlapping periods."
      ))
    }
    else{
      
   
    train_data <- reactiveValues()
    testing_data <- reactiveValues()
    log_training_data <- reactiveValues()
    log_testing_data <- reactiveValues()
    req(input$patient_type_dataset_for_test)
    
    # Filter the data based on the selected dates
    if (input$patient_type_dataset_for_test == "Medical Patient") {
      data <- medical_data_dataset()
      data_ts <- ts(data$Medical.Patient, frequency = 12, start = c(1987, 1))
      
    } else if (input$patient_type_dataset_for_test == "Obstetrics Patient") {
      data <- obstetrics_data_dataset()
      data_ts <- ts(data$Obstetrics.Patient, frequency = 12, start = c(1987, 1))
    }
    
    
    train_start_year <- format(as.Date(input$training_start_date), "%Y")
    train_end_year <- format(as.Date(input$training_end_date), "%Y")
    
    train_start_month <- format(as.Date(input$training_start_date), "%m")
    train_end_month <- format(as.Date(input$training_end_date), "%m")
    
    test_start_year <- format(as.Date(input$testing_start_date), "%Y")
    test_end_year <- format(as.Date(input$testing_end_date), "%Y")
    
    test_start_month <- format(as.Date(input$testing_start_date), "%m")
    test_end_month <- format(as.Date(input$testing_end_date), "%m")
    
    
    
    # Split the data into training, testing, and validation sets
    train_data <- window(data_ts, start = c(as.integer(train_start_year),as.integer(train_start_month)), end = c(as.integer(train_end_year), as.integer(train_end_month)))
    test_data <- window(data_ts, start = c(as.integer(test_start_year),as.integer(test_start_month)), end = c(as.integer(test_end_year), as.integer(test_end_month)))
    
     
    if (input$patient_type_dataset_for_test == "Medical Patient") {
      # Train the nnetar model
          log_training_data=log10(train_data)
          log_testing_data=log10(test_data)
          
          withProgress(message = "Testing in progress...", value = 0, {
            set.seed(107)
            model <- nnetar(log_training_data,p=2,P=1,size=4, repeats = 500, learningrate=0.001)
            forecast <- forecast(model,h=length(log_testing_data))
            
            Sys.sleep(1) # Simulating some processing time
            incProgress(1, detail = "Forecast completed.")
          })
          
         
        
        # Generate the forecast
        
        
      
        
          residuals <-residuals(model)
          residuals=residuals[13:length(residuals)]
          fitted_values<-fitted(model)
          fitted<-fitted_values[13:length(fitted_values)]
          print(residuals)
          print(fitted)
 
    
    }
    else if (input$patient_type_dataset_for_test == "Obstetrics Patient") {
      # Train the ses model
       
        log_training_data=log10(train_data)
        log_testing_data=log10(test_data)
         
        withProgress(message = "Testing in progress...", value = 0, {
          
          forecast <- ses(log_training_data,h=length(log_testing_data),level=95)
         
          
          Sys.sleep(1) # Simulating some processing time
          incProgress(1, detail = "Forecast completed.")
        }) 
        residuals <-residuals(forecast)
        fitted <- forecast$fitted
        
        
      # Generate the forecast
     # forecast <- forecast(model, PI=TRUE, h=84, level=95)
      
    }
  
    training_performance <- reactive({
      data.frame(
        Category = "Training",
        ME = format(round(mean(residuals), 6), nsmall = 6),
        RMSE = format(round(sqrt(mean((residuals)^2)), 6), nsmall = 6),
        MAE = format(round(mean(abs(residuals)), 6), nsmall = 6),
        MPE = format(round(mean((residuals)/fitted*100), 6), nsmall = 6),
        MAPE = format(round(mean(abs(residuals)/fitted*100), 6), nsmall = 6)
      )
    })
    
    # Create a reactive expression for testing_performance
    testing_performance <- reactive({
      data.frame(
        Category = "Testing",
        ME = format(round(mean(log_testing_data - forecast$mean), 6), nsmall = 6),
        RMSE = format(round(sqrt(mean((log_testing_data - forecast$mean)^2)), 6), nsmall = 6),
        MAE = format(round(mean(abs(log_testing_data - forecast$mean)), 6), nsmall = 6),
        MPE = format(round(mean(((log_testing_data - forecast$mean)/log_testing_data)*100), 6), nsmall = 6),
        MAPE = format(round(mean(abs(((log_testing_data - forecast$mean)/log_testing_data)*100)), 6), nsmall = 6)
      )
    })
    
    
    
    
    # Create a reactive expression for performance_table  
    performance_table <- reactive({
      rbind(training_performance(), testing_performance())
    })
    
    # Render the table
    # Add a title above the table
    output$performance_table <- renderUI({
      tagList(
        h3("Performance Measure"),
        renderTable(performance_table())
      )
    })
    
    
    
     
    
     
  
    
  
    
    output$forecast_plot_for_test <- renderPlot({
      if (input$patient_type_dataset_for_test == "Medical Patient") {
        
        
        
        plot(forecast,ylab="Medical Patient Admission (log10)",type="l", xlab="Year",panel.first = rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#D4F1F4"))
        # Add the test data to the plot
        lines(log10(test_data),col="black", lwd = 1.5)
        
        lines(forecast$fitted, col = "red", lwd = 1)
        # Add a legend
        legend("topleft", legend = c("Fitted Values", "Actual Data","Forecast"), 
               col = c("red", "black","blue"), lty = 1, cex = 0.8)
      }
      else {
        
        plot(forecast,ylab="Obstetrics Patient Admission (log10)",type="l", xlab="Year",panel.first = rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#D4F1F4"))
        # Add the test data to the plot
        lines(log10(test_data),col="black", lwd = 1.5)
        
        lines(forecast$fitted, col = "red", lwd = 1)
        # Add a legend
        legend("topleft", legend = c("Fitted Values", "Actual Data","Forecast"), 
               col = c("red", "black","blue"), lty = 1, cex = 0.8)
      }
         
      
    })
    }
    # Plot the forecasted and actual data
   
  })
  
  observeEvent(input$num_years, {
    if (is.na(input$num_years)) {
       
    } else if (input$num_years < 1) {
      updateNumericInput(session, "num_years", value = 1)
       
    } else if (input$num_years > 84) {
      updateNumericInput(session, "num_years", value = 84)
       
    } else {
       
    }
  })
  
  
   

  
  
  # Check credentials directly on SQLite DB
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "database.sqlite",
      passphrase = key_get(services, usernames)
      
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
  
  
  
  
  
  
  
  
  
  
}
