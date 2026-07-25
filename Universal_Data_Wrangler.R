# =========================================================
# Universal Data Wrangler Project
#
# IMPORTANT:
# The app focuses ONLY on data wrangling and NOT
# statistical analysis or modeling.
#
# FEATURES:
# 1. Import CSV, JSON, Excel from URL
# 2. Preview data
# 3. Smart date detection
# 4. Automatic type suggestions
# 5. Missing value handling
# 6. Variable type conversion
# 7. Duplicate removal
# 8. Filtering
# 9. Column renaming
# 10. Reshaping
# 11. Download cleaned data
# 12. Reset dataset
#
# =========================================================



# =========================================================
# REQUIRED LIBRARIES
# =========================================================

library(shiny)      # Build Shiny app
library(dplyr)      # Data wrangling
library(tidyr)      # Pivoting / reshaping
library(readr)      # Read CSV files
library(readxl)     # Read Excel files
library(jsonlite)   # Read JSON files
library(DT)         # Interactive tables
library(lubridate)  # Date parsing
library(stringr)    # String operations



# =========================================================
# HELPER FUNCTION:
# SMART DATE DETECTION
#
# PURPOSE:
# Automatically identify character columns that appear
# to contain dates and convert them to Date format.
#
# LOGIC:
# If more than 70% of values successfully convert to
# dates, the column becomes a Date column.
# =========================================================

detect_dates <- function(df){
  
  # loop through all columns
  for(col in names(df)){
    
    # only test character columns
    if(is.character(df[[col]])){
      
      # attempt multiple date formats
      parsed_dates <- suppressWarnings(
        
        parse_date_time(
          
          df[[col]],
          
          orders = c(
            "ymd",
            "mdy",
            "dmy",
            "Ymd HMS",
            "mdY HMS"
          )
          
        )
        
      )
      
      # determine conversion success rate
      success_rate <- mean(!is.na(parsed_dates))
      
      # convert if majority are valid dates
      if(success_rate > .70){
        
        df[[col]] <- as.Date(parsed_dates)
        
      }
      
    }
    
  }
  
  return(df)
  
}



# =========================================================
# HELPER FUNCTION:
# TYPE SUGGESTION
#
# PURPOSE:
# Suggest possible better data types for columns.
#
# Example:
# Character values like:
# "1", "2", "3"
# may actually represent numeric data.
# =========================================================

suggest_type <- function(x){
  
  if(is.numeric(x)){
    
    return("numeric")
    
  }
  
  if(is.logical(x)){
    
    return("logical")
    
  }
  
  if(inherits(x,"Date")){
    
    return("Date")
    
  }
  
  if(is.character(x)){
    
    suppressWarnings({
      
      numeric_test <- mean(
        !is.na(as.numeric(x))
      )
      
    })
    
    # if most values convert successfully
    if(numeric_test > .80){
      
      return("Possible Numeric")
      
    }
    
    return("character")
    
  }
  
  return(class(x)[1])
  
}



# =========================================================
# USER INTERFACE
# =========================================================

ui <- fluidPage(
  
  titlePanel(
    "Universal Data Wrangler"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # ============================================
      # DATA IMPORT SECTION
      # ============================================
      
      h4("1. Load Data"),
      
      textInput(
        "url",
        "Dataset URL",
        placeholder = "Paste dataset URL here"
      ),
      
      selectInput(
        "file_type",
        "File Type",
        choices = c(
          "csv",
          "json",
          "xlsx"
        )
      ),
      
      actionButton(
        "load_data",
        "Load Dataset"
      ),
      
      br(),
      br(),
      
      # reset dataset button
      actionButton(
        "reset_data",
        "Reset Dataset"
      ),
      
      hr(),
      
      # ============================================
      # MISSING VALUES
      # ============================================
      
      h4("2. Missing Values"),
      
      selectInput(
        "na_action",
        "Choose Method",
        choices = c(
          "None",
          "Drop Rows with NA",
          "Replace Numeric NA with Mean",
          "Replace Numeric NA with Median",
          "Replace Character NA with Unknown"
        )
      ),
      
      actionButton(
        "apply_na",
        "Apply"
      ),
      
      hr(),
      
      # ============================================
      # TYPE CONVERSION
      # ============================================
      
      h4("3. Convert Variable Types"),
      
      uiOutput(
        "column_selector"
      ),
      
      selectInput(
        "new_type",
        "Convert To",
        choices = c(
          "character",
          "numeric",
          "factor",
          "Date",
          "logical"
        )
      ),
      
      actionButton(
        "convert_type",
        "Convert"
      ),
      
      hr(),
      
      # ============================================
      # DUPLICATE REMOVAL
      # ============================================
      
      h4("4. Duplicate Removal"),
      
      checkboxInput(
        "remove_duplicates",
        "Remove duplicate rows",
        FALSE
      ),
      
      actionButton(
        "apply_duplicates",
        "Apply"
      ),
      
      hr(),
      
      # ============================================
      # FILTERING
      # ============================================
      
      h4("5. Filter Rows"),
      
      textInput(
        "filter_column",
        "Column Name"
      ),
      
      textInput(
        "filter_value",
        "Value Contains"
      ),
      
      actionButton(
        "apply_filter",
        "Apply Filter"
      ),
      
      hr(),
      
      # ============================================
      # COLUMN RENAMING
      # ============================================
      
      h4("6. Rename Columns"),
      
      textInput(
        "old_name",
        "Current Name"
      ),
      
      textInput(
        "new_name",
        "New Name"
      ),
      
      actionButton(
        "rename_column",
        "Rename"
      ),
      
      hr(),
      
      # ============================================
      # RESHAPING
      # ============================================
      
      h4("7. Reshape Data"),
      
      selectInput(
        "reshape_type",
        "Operation",
        choices = c(
          "None",
          "Pivot Longer",
          "Pivot Wider"
        )
      ),
      
      textInput(
        "reshape_cols",
        "Columns (comma separated)"
      ),
      
      actionButton(
        "apply_reshape",
        "Apply"
      ),
      
      hr(),
      
      # ============================================
      # DOWNLOAD
      # ============================================
      
      h4("8. Download"),
      
      downloadButton(
        "download_csv",
        "Download CSV"
      )
      
    ),
    
    
    
    # ============================================
    # MAIN PANEL
    # ============================================
    
    mainPanel(
      
      tabsetPanel(
        
        # preview tab
        tabPanel(
          "Data Preview",
          
          br(),
          
          DTOutput("data_table")
        ),
        
        # summary tab
        tabPanel(
          
          "Summary",
          
          br(),
          
          verbatimTextOutput(
            "summary_info"
          ),
          
          br(),
          
          DTOutput(
            "column_info"
          )
          
        )
        
      )
      
    )
    
  )
  
)



# =========================================================
# SERVER LOGIC
# =========================================================

server <- function(input, output, session){
  
  # =======================================================
  # REACTIVE STORAGE
  #
  # data:
  # current modified dataset
  #
  # original:
  # untouched original dataset
  #
  # allows reset functionality
  # =======================================================
  
  rv <- reactiveValues(
    
    data = NULL,
    
    original = NULL
    
  )
  
  
  
  # =======================================================
  # LOAD DATA
  # =======================================================
  
  observeEvent(input$load_data,{
    
    # ensure URL exists
    req(input$url)
    
    # =====================================================
    # URL VALIDATION
    #
    # verify URL begins with:
    # http:// or https://
    # =====================================================
    
    if(!grepl("^https?://", input$url)){
      
      showNotification(
        "Please enter a valid URL.",
        type = "error"
      )
      
      return()
      
    }
    
    
    
    # =====================================================
    # TRY TO LOAD DATA
    #
    # tryCatch prevents app crashes
    # =====================================================
    
    tryCatch({
      
      # ===================================================
      # CSV FILES
      # ===================================================
      
      if(input$file_type == "csv"){
        
        df <- read_csv(
          input$url,
          show_col_types = FALSE
        )
        
        
        
        # ===================================================
        # JSON FILES
        # ===================================================
        
      } else if(input$file_type == "json"){
        
        json_data <- fromJSON(
          input$url
        )
        
        df <- as.data.frame(
          json_data
        )
        
        
        
        # ===================================================
        # EXCEL FILES
        # ===================================================
        
      } else if(input$file_type == "xlsx"){
        
        # create temporary file
        temp_file <- tempfile(
          fileext = ".xlsx"
        )
        
        # download excel file
        download.file(
          input$url,
          temp_file,
          mode = "wb"
        )
        
        # read excel file
        df <- read_excel(
          temp_file
        )
        
        df <- as.data.frame(df)
        
      }
      
      
      
      # ===================================================
      # AUTOMATIC DATE DETECTION
      # ===================================================
      
      df <- detect_dates(df)
      
      
      
      # ===================================================
      # STORE DATASETS
      #
      # data:
      # active working dataset
      #
      # original:
      # untouched backup copy
      # ===================================================
      
      rv$data <- df
      
      rv$original <- df
      
      
      
      # success message
      showNotification(
        "Dataset loaded successfully!"
      )
      
    },
    
    # =====================================================
    # ERROR HANDLING
    # =====================================================
    
    error = function(e){
      
      showNotification(
        
        paste(
          "Error loading dataset:",
          e$message
        ),
        
        type = "error"
        
      )
      
    })
    
  })
  
  
  
  # =======================================================
  # RESET DATASET
  #
  # restore original imported dataset
  # =======================================================
  
  observeEvent(input$reset_data,{
    
    req(rv$original)
    
    rv$data <- rv$original
    
    showNotification(
      "Dataset reset successfully."
    )
    
  })
  
  
  
  # =======================================================
  # DYNAMIC COLUMN SELECTOR
  #
  # updates available columns whenever data changes
  # =======================================================
  
  output$column_selector <- renderUI({
    
    req(rv$data)
    
    selectInput(
      
      "selected_column",
      
      "Select Column",
      
      choices = names(rv$data)
      
    )
    
  })
  
  
  
  # =======================================================
  # DATA PREVIEW TABLE
  # =======================================================
  
  output$data_table <- renderDT({
    
    req(rv$data)
    
    datatable(
      
      rv$data,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
      
    )
    
  })
  
  
  
  # =======================================================
  # SUMMARY INFORMATION
  # =======================================================
  
  output$summary_info <- renderPrint({
    
    req(rv$data)
    
    cat(
      "Rows:",
      nrow(rv$data),
      "\n"
    )
    
    cat(
      "Columns:",
      ncol(rv$data),
      "\n"
    )
    
    cat(
      "Missing Values:",
      sum(is.na(rv$data))
    )
    
  })
  
  
  
  # =======================================================
  # COLUMN INFORMATION TABLE
  # =======================================================
  
  output$column_info <- renderDT({
    
    req(rv$data)
    
    info <- data.frame(
      
      Column = names(rv$data),
      
      Current_Type = sapply(
        
        rv$data,
        
        function(x)
          class(x)[1]
        
      ),
      
      Suggested_Type = sapply(
        rv$data,
        suggest_type
      ),
      
      Missing_Values = sapply(
        
        rv$data,
        
        function(x)
          sum(is.na(x))
        
      )
      
    )
    
    datatable(info)
    
  })
  
  
  
  # =======================================================
  # MISSING VALUE HANDLING
  # =======================================================
  
  observeEvent(input$apply_na,{
    
    req(rv$data)
    
    df <- rv$data
    
    
    
    # remove rows containing NA values
    if(input$na_action == "Drop Rows with NA"){
      
      df <- na.omit(df)
      
      
      
      # replace numeric NA with mean
    } else if(
      
      input$na_action ==
      "Replace Numeric NA with Mean"
      
    ){
      
      numeric_cols <- sapply(
        df,
        is.numeric
      )
      
      for(col in names(df)[numeric_cols]){
        
        df[[col]][is.na(df[[col]])] <-
          
          mean(
            df[[col]],
            na.rm = TRUE
          )
        
      }
      
      
      
      # replace numeric NA with median
    } else if(
      
      input$na_action ==
      "Replace Numeric NA with Median"
      
    ){
      
      numeric_cols <- sapply(
        df,
        is.numeric
      )
      
      for(col in names(df)[numeric_cols]){
        
        df[[col]][is.na(df[[col]])] <-
          
          median(
            df[[col]],
            na.rm = TRUE
          )
        
      }
      
      
      
      # replace character NA with Unknown
    } else if(
      
      input$na_action ==
      "Replace Character NA with Unknown"
      
    ){
      
      char_cols <- sapply(
        df,
        is.character
      )
      
      for(col in names(df)[char_cols]){
        
        df[[col]][is.na(df[[col]])] <-
          "Unknown"
        
      }
      
    }
    
    rv$data <- df
    
    showNotification(
      "Missing value handling applied."
    )
    
  })
  
  
  
  # =======================================================
  # TYPE CONVERSION
  # =======================================================
  
  observeEvent(input$convert_type,{
    
    req(rv$data)
    
    req(input$selected_column)
    
    df <- rv$data
    
    col <- input$selected_column
    
    
    
    # convert selected column type
    if(input$new_type == "numeric"){
      
      df[[col]] <- as.numeric(df[[col]])
      
    } else if(input$new_type == "character"){
      
      df[[col]] <- as.character(df[[col]])
      
    } else if(input$new_type == "factor"){
      
      df[[col]] <- as.factor(df[[col]])
      
    } else if(input$new_type == "Date"){
      
      df[[col]] <- as.Date(df[[col]])
      
    } else if(input$new_type == "logical"){
      
      df[[col]] <- as.logical(df[[col]])
      
    }
    
    rv$data <- df
    
    showNotification(
      "Column converted successfully."
    )
    
  })
  
  
  
  # =======================================================
  # REMOVE DUPLICATES
  # =======================================================
  
  observeEvent(input$apply_duplicates,{
    
    req(rv$data)
    
    if(input$remove_duplicates){
      
      rv$data <- distinct(rv$data)
      
      showNotification(
        "Duplicate rows removed."
      )
      
    }
    
  })
  
  
  
  # =======================================================
  # FILTERING
  #
  # ignore_case = TRUE allows:
  #
  # apple
  # Apple
  # APPLE
  #
  # to all match
  # =======================================================
  
  observeEvent(input$apply_filter,{
    
    req(rv$data)
    
    df <- rv$data
    
    
    
    # verify column exists
    if(input$filter_column %in% names(df)){
      
      df <- df %>%
        
        filter(
          
          str_detect(
            
            as.character(
              .data[[input$filter_column]]
            ),
            
            regex(
              input$filter_value,
              ignore_case = TRUE
            )
            
          )
          
        )
      
      rv$data <- df
      
      showNotification(
        "Filter applied."
      )
      
    } else {
      
      showNotification(
        "Column name not found.",
        type = "error"
      )
      
    }
    
  })
  
  
  
  # =======================================================
  # RENAME COLUMNS
  # =======================================================
  
  observeEvent(input$rename_column,{
    
    req(rv$data)
    
    df <- rv$data
    
    
    
    # verify column exists
    if(input$old_name %in% names(df)){
      
      names(df)[
        names(df) == input$old_name
      ] <- input$new_name
      
      rv$data <- df
      
      showNotification(
        "Column renamed successfully."
      )
      
    } else {
      
      showNotification(
        "Column name not found.",
        type = "error"
      )
      
    }
    
  })
  
  
  
  # =======================================================
  # RESHAPE DATA
  # =======================================================
  
  observeEvent(input$apply_reshape,{
    
    req(rv$data)
    
    df <- rv$data
    
    
    
    # split comma-separated columns
    cols <- str_trim(
      
      unlist(
        
        strsplit(
          input$reshape_cols,
          ","
        )
        
      )
      
    )
    
    
    
    tryCatch({
      
      # ===============================================
      # PIVOT LONGER
      # ===============================================
      
      if(input$reshape_type == "Pivot Longer"){
        
        df <- pivot_longer(
          
          df,
          
          cols = all_of(cols)
          
        )
        
        
        
        # ===============================================
        # PIVOT WIDER
        # ===============================================
        
      } else if(
        
        input$reshape_type ==
        "Pivot Wider"
        
      ){
        
        if(length(cols) >= 2){
          
          df <- pivot_wider(
            
            df,
            
            names_from =
              all_of(cols[1]),
            
            values_from =
              all_of(cols[2])
            
          )
          
        }
        
      }
      
      rv$data <- df
      
      showNotification(
        "Reshaping applied."
      )
      
    },
    
    error = function(e){
      
      showNotification(
        
        paste(
          "Reshape Error:",
          e$message
        ),
        
        type = "error"
        
      )
      
    })
    
  })
  
  
  
  # =======================================================
  # DOWNLOAD CLEANED DATA
  # =======================================================
  
  output$download_csv <- downloadHandler(
    
    filename = function(){
      
      paste0(
        
        "cleaned_data_",
        
        Sys.Date(),
        
        ".csv"
        
      )
      
    },
    
    content = function(file){
      
      write.csv(
        
        rv$data,
        
        file,
        
        row.names = FALSE
        
      )
      
    }
    
  )
  
}



# =========================================================
# RUN APPLICATION
# =========================================================

shinyApp(
  
  ui = ui,
  
  server = server
  
)