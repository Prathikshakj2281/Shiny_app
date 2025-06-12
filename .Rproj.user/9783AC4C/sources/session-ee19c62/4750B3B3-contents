# Final - 21st april Load Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(dplyr)
library(magrittr) 
library(ggplot2)
library(sf)
library(DT)
library(smwrGraphs) # For piperPlot
library(smwrBase)   # Often used with smwrGraphs for utility functions
library(readr)
library(tidyr)
library(rmarkdown)
library(shinycssloaders) # For loading spinners

# Custom color palette
iisc_palette <- c(
  primary = "#005A9C",
  secondary = "#00A86B",
  accent = "#4FC3F7",
  background = "#F0F4F8",
  text = "#2C3E50"
)

# WQI Calculation Function (Provided by user, ensuring Sodium column name)
calculate_wqi <- function(gpkg_path) {
  layer_name <- st_layers(gpkg_path)$name[1]
  water_sf <- st_read(gpkg_path, layer = layer_name, quiet = TRUE)
  
  # Ensure WGS84 projection
  if (sf::st_crs(water_sf)$epsg != 4326) {
    water_sf <- st_transform(water_sf, 4326)
  }
  
  # Validate geometries
  if (!all(st_is_valid(water_sf))) {
    water_sf <- st_make_valid(water_sf)
  }
  
  # Rename "NA" column to "Sodium" if present
  if ("NA" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "NA"] <- "Sodium"
  }
  
  # Rename "BICARBONATE" to "HCO3" for consistency with common hydrochem packages
  if ("BICARBONATE" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "BICARBONATE"] <- "HCO3"
  }
  # Rename "SULPHATE" to "SO4"
  if ("SULPHATE" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "SULPHATE"] <- "SO4"
  }
  # Rename "CHLORIDE" to "Cl"
  if ("CHLORIDE" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "CHLORIDE"] <- "Cl"
  }
  # Rename "CA" to "Ca"
  if ("CA" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "CA"] <- "Ca"
  }
  # Rename "MG" to "Mg"
  if ("MG" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "MG"] <- "Mg"
  }
  # Rename "K" to "K" (already common)
  if ("K" %in% names(water_sf)) {
    # No action needed, just illustrating check
  }
  
  standards <- list(
    TDS = list(St = 1000, Wi = 0.121),
    EC = list(St = 2500, Wi = 0.121),
    NITRATE = list(St = 50, Wi = 0.152),
    SO4 = list(St = 250, Wi = 0.121), # Changed from SULPHATE
    Cl = list(St = 250, Wi = 0.093), # Changed from CHLORIDE
    HCO3 = list(St = 500, Wi = 0.152), # Changed from BICARBONATE
    FLUORIDE = list(St = 1.2, Wi = 0.030),
    Ca = list(St = 100, Wi = 0.060), # Changed from CA
    Mg = list(St = 50, Wi = 0.060), # Changed from MG
    Sodium = list(St = 200, Wi = 0.060),
    K = list(St = 20, Wi = 0.030)
  )
  
  water_sf$WQI <- NA_real_
  param_names <- names(standards)
  
  # Replace missing parameters with 0
  for (param in param_names) {
    if (!param %in% names(water_sf)) {
      water_sf[[param]] <- 0
    } else {
      water_sf[[param]][is.na(water_sf[[param]])] <- 0
    }
  }
  
  for (param in param_names) {
    qi_col <- paste0("qi_", param)
    sli_col <- paste0("SLi_", param)
    water_sf[[qi_col]] <- water_sf[[param]] / standards[[param]]$St
    water_sf[[sli_col]] <- water_sf[[qi_col]] * standards[[param]]$Wi
  }
  
  sli_cols <- paste0("SLi_", param_names)
  sli_values <- st_drop_geometry(water_sf)[, sli_cols]
  sli_values[] <- lapply(sli_values, as.numeric)
  water_sf$WQI <- rowSums(sli_values, na.rm = TRUE)
  
  water_sf$Quality <- cut(
    water_sf$WQI,
    breaks = c(-Inf, 0.5, 1, 2, 3, Inf),
    labels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable"),
    right = FALSE,
    ordered_result = TRUE # Ensure proper ordering for plotting/summaries
  )
  
  # Write CSV output with WQI and Quality columns (optional, for debugging)
  # csv_path <- file.path(getwd(), "water_quality_output.csv")
  # write.csv(st_drop_geometry(water_sf), csv_path, row.names = FALSE)
  
  return(water_sf)
}

# UI (Updated to uncomment plots and sections)
ui <- dashboardPage(
  title = "Ground Water Assessment Dashboard",
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; height: 60px; background-color: transparent;",
      tags$img(src = "sirpilogo white.avif", height = "30px", style = "margin-right: 10px;"),
      tags$img(src = "gdx_logo.png", height = "30px", style = "margin-right: 10px;"), # Add your second logo here
      tags$span("Ground Water Assessment Dashboard",
                style = "color: white; font-weight: bold; font-size: 18px; margin-left: 30px;")
    ),
    titleWidth = 650
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Data Exploration", tabName = "data_explore", icon = icon("magnifying-glass")),
      menuItem("Ground Water Chemistry", tabName = "chemistry", icon = icon("flask"))
      
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$title("Ground Water Dashboard"),
      tags$style(HTML(sprintf('
      .skin-blue .main-header .logo { background-color: %s; color: white; }
      .skin-blue .main-header .navbar { background-color: %s; }
      body { background-color: %s; color: %s; }
      .box { border-top-color: %s; }
    ', iisc_palette["primary"], iisc_palette["secondary"],
                              iisc_palette["background"], iisc_palette["text"],
                              iisc_palette["accent"]))),
      tags$style(HTML("
      .main-header {
        height: 60px !important; /* Adjust this value as needed */
      }
      .main-header .logo {
        height: 60px !important; /* Match the main-header height */
        line-height: 60px !important; /* Vertically center the logo text if any (though you have images) */
      }
      .main-header .navbar {
        min-height: 60px !important; /* Ensure navbar doesn't collapse */
      }
      .main-header .title {
        height: 60px !important; /* Match the main-header height */
        line-height: 60px !important; /* Vertically center the title text */
      }
      .main-header .title > div { /* Target the div containing your logos and text */
        display: flex;
        align-items: center; /* Vertically align items within the div */
        height: 100%; /* Ensure the div takes full height of the title */
      }
    "))
    ),
    
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload GPKG File", status = "primary", solidHeader = TRUE,
                    fileInput("gpkg_upload", "Choose GPKG File", accept = c(".gpkg")),
                    actionButton("load_data", "Load Data", icon = icon("database"))
                ),
                box(title = "File Information", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("file_info"))
              )
      ),
      
      tabItem(tabName = "data_explore",
              fluidRow(
                box(title = "Dataset Overview", status = "primary", solidHeader = TRUE, uiOutput("dataset_summary")),
                box(title = "Column Details", status = "success", solidHeader = TRUE, DTOutput("column_details"))
              ),
              fluidRow(
                box(title = "Data Preview", status = "warning", solidHeader = TRUE, DTOutput("data_preview")),
                box(title = "Data Statistics", status = "info", solidHeader = TRUE, uiOutput("data_statistics"))
              )
      ),
      
      tabItem(tabName = "chemistry",
              fluidRow(
                box(title = "Filters", status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, uiOutput("state_ui")),
                      column(4, uiOutput("district_ui")),
                      column(4, uiOutput("block_ui"))
                    ),
                    br(),
                    uiOutput("download_chemistry_ui") # Now uncommented
                )
              ),
              fluidRow(
                box(title = "Piper Plot", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("piper_plot", height = "700px"), type = 6) # Uncommented
                )
              ),
              fluidRow(
                box(title = "Chemical Composition", status = "success", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("chemCompPlot", height = "700px"), type = 6) # Uncommented
                )
              ),
              fluidRow(
                box(title = "Water Types", status = "warning", solidHeader = TRUE, width = 12,
                    withSpinner(uiOutput("water_types_section"), type = 6) # Uncommented
                )
              ),
              fluidRow(
                box(title = "Suggested Measures", status = "info", solidHeader = TRUE, width = 12,
                    withSpinner(uiOutput("measures_section"), type = 6) # Uncommented
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data_storage <- reactiveValues(csv_data = NULL, sf_data = NULL, water_sf = NULL, data_loaded = FALSE)
  
  # Helper function to convert mg/L to meq/L
  mg_to_meq <- function(value, ion_name) {
    equivalent_weights <- list(
      Ca = 20.04,   # 40.08 / 2
      Mg = 12.155,  # 24.31 / 2
      Sodium = 22.99,  # 22.99 / 1
      K = 39.10,    # 39.10 / 1
      Cl = 35.45,   # 35.45 / 1
      HCO3 = 61.02, # 61.02 / 1
      SO4 = 48.03   # 96.06 / 2
    )
    
    if (ion_name %in% names(equivalent_weights)) {
      return(value / equivalent_weights[[ion_name]])
    } else {
      warning(paste("Equivalent weight for", ion_name, "not found. Returning original value."))
      return(value)
    }
  }
  
  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    tryCatch({
      water_sf_raw <- calculate_wqi(input$gpkg_upload$datapath)
      
      # Ensure required columns for chemistry plots exist, fill with 0 if missing
      required_chem_cols <- c("Ca", "Mg", "Sodium", "K", "Cl", "HCO3", "SO4", "STATE_UT", "DISTRICT", "BLOCK")
      for (col in required_chem_cols) {
        if (!(col %in% names(water_sf_raw))) {
          water_sf_raw[[col]] <- 0
          warning(paste0("Column '", col, "' not found in uploaded data. Filled with 0 for chemistry plots."))
        }
      }
      
      data_storage$csv_data <- st_drop_geometry(water_sf_raw)
      data_storage$sf_data <- water_sf_raw # Keep the sf object for potential future map use
      data_storage$water_sf <- water_sf_raw # This will be the source for filters and plots
      data_storage$data_loaded <- TRUE
      
      # Update State choices immediately after data load
      states <- c("Select State", unique(data_storage$water_sf$STATE_UT))
      updateSelectInput(session, "state_chem", choices = states, selected = "Select State")
      
      # Reset subsequent filters
      updateSelectInput(session, "district_chem", choices = "Select District", selected = "Select District")
      updateSelectInput(session, "block_chem", choices = "Select Block", selected = "Select Block")
      
      showNotification("GPKG loaded and WQI calculated successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error processing GPKG file:", e$message), type = "error")
      data_storage$data_loaded <- FALSE
    })
  })
  
  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    cat("File Uploaded Successfully!\n",
        "Number of Rows:", nrow(data_storage$csv_data), "\n",
        "Number of Columns:", ncol(data_storage$csv_data))
  })
  
  output$dataset_summary <- renderUI({
    req(data_storage$csv_data)
    HTML(paste(
      "<h4>Dataset Characteristics</h4>",
      "<p><strong>Total Rows:</strong>", nrow(data_storage$csv_data), "</p>",
      "<p><strong>Total Columns:</strong>", ncol(data_storage$csv_data), "</p>"
    ))
  })
  
  output$column_details <- renderDT({
    req(data_storage$csv_data)
    column_info <- data.frame(
      Column = names(data_storage$csv_data),
      Type = sapply(data_storage$csv_data, function(x) class(x)[1]),
      Unique_Values = sapply(data_storage$csv_data, function(x) length(unique(x))),
      Missing_Values = sapply(data_storage$csv_data, function(x) sum(is.na(x)))
    )
    datatable(column_info, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_preview <- renderDT({
    req(data_storage$csv_data)
    datatable(data_storage$csv_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_statistics <- renderUI({
    req(data_storage$csv_data)
    numeric_cols <- names(data_storage$csv_data)[sapply(data_storage$csv_data, is.numeric)]
    if (length(numeric_cols) > 0) {
      stats <- data_storage$csv_data %>%
        select(all_of(numeric_cols)) %>%
        summarise(across(everything(), list(Mean = mean, Median = median, Min = min, Max = max, SD = sd), na.rm = TRUE))
      HTML(paste("<h4>Numeric Column Statistics</h4><pre>", capture.output(print(stats)), "</pre>"))
    } else HTML("<p>No numeric columns found.</p>")
  })
  
  # --- Ground Water Chemistry Tab Logic ---
  
  # Reactive expression for filtered data
  filtered_chemistry_data <- reactive({
    req(data_storage$water_sf)
    df <- data_storage$water_sf
    
    if (!is.null(input$state_chem) && input$state_chem != "Select State") {
      df <- df %>% filter(STATE_UT == input$state_chem)
    }
    if (!is.null(input$district_chem) && input$district_chem != "Select District") {
      df <- df %>% filter(DISTRICT == input$district_chem)
    }
    if (!is.null(input$block_chem) && input$block_chem != "Select Block") {
      df <- df %>% filter(BLOCK == input$block_chem)
    }
    
    # Convert relevant columns to numeric for calculation, handling NA as 0
    # Also ensure required columns exist for Piper plot (set to 0 if missing)
    ions_mg_l <- c("Ca", "Mg", "Sodium", "K", "Cl", "HCO3", "SO4")
    for (ion in ions_mg_l) {
      if (!(ion %in% names(df))) {
        df[[ion]] <- 0 # Add column if missing
      }
      df[[ion]] <- as.numeric(df[[ion]])
      df[[ion]][is.na(df[[ion]])] <- 0 # Replace NA with 0
    }
    
    # Convert to meq/L for plotting (Piper plot uses this)
    df_meq <- df %>%
      mutate(
        Ca_meq = mg_to_meq(Ca, "Ca"),
        Mg_meq = mg_to_meq(Mg, "Mg"),
        Na_meq = mg_to_meq(Sodium, "Sodium"),
        K_meq = mg_to_meq(K, "K"),
        Cl_meq = mg_to_meq(Cl, "Cl"),
        HCO3_meq = mg_to_meq(HCO3, "HCO3"),
        SO4_meq = mg_to_meq(SO4, "SO4")
      )
    
    return(df_meq)
  })
  
  # --- Filter UI Generation ---
  
  output$state_ui <- renderUI({
    req(data_storage$water_sf)
    states <- c("Select State", unique(data_storage$water_sf$STATE_UT))
    selectInput("state_chem", "State/UT:", choices = states, selected = "Select State")
  })
  
  output$district_ui <- renderUI({
    req(input$state_chem)
    df <- data_storage$water_sf
    districts <- "Select District"
    if (input$state_chem != "Select State") {
      districts <- c("Select District", unique(df$DISTRICT[df$STATE_UT == input$state_chem]))
    }
    selectInput("district_chem", "District:", choices = districts, selected = "Select District")
  })
  
  output$block_ui <- renderUI({
    req(input$district_chem)
    df <- data_storage$water_sf
    blocks <- "Select Block"
    if (input$district_chem != "Select District") {
      blocks <- c("Select Block", unique(df$BLOCK[df$DISTRICT == input$district_chem & df$STATE_UT == input$state_chem]))
    }
    selectInput("block_chem", "Block:", choices = blocks, selected = "Select Block")
  })
  
  # --- Piper Plot ---
  output$piper_plot <- renderPlot({
    df_plot <- filtered_chemistry_data()
    req(nrow(df_plot) > 0)
    
    # Ensure all required meq/L columns exist and are numeric
    required_piper_cols <- c("Ca_meq", "Mg_meq", "Na_meq", "K_meq", "Cl_meq", "HCO3_meq", "SO4_meq")
    for (col in required_piper_cols) {
      if (! (col %in% names(df_plot)) || !is.numeric(df_plot[[col]]) ) {
        # This should ideally be caught by `filtered_chemistry_data` but as a last resort
        df_plot[[col]] <- 0 
        warning(paste("Missing or non-numeric Piper plot column:", col, ". Filling with 0."))
      }
    }
    
    # Check if any data remains after filtering and numeric conversion
    if (nrow(df_plot) == 0 || all(is.na(df_plot[required_piper_cols])) ) {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), main="No Data Available for Piper Plot")
      text(0.5, 0.5, "Adjust filters or upload data.", cex=1.5)
      return(NULL)
    }
    
    # Remove rows with all NA for Piper plot parameters to avoid errors
    df_plot_clean <- df_plot %>% 
      filter(rowSums(is.na(select(., all_of(required_piper_cols)))) < length(required_piper_cols)) %>%
      select(all_of(c(required_piper_cols, "Quality")))
    
    # Ensure at least one valid row for plotting
    if (nrow(df_plot_clean) == 0) {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Insufficient Data for Piper Plot")
      text(0.5, 0.5, "Data is all NA for chemical parameters.", cex=1.5)
      return(NULL)
    }
    
    # Define colors for water quality types
    quality_colors <- c(
      "Excellent" = iisc_palette["secondary"], # green
      "Good" = iisc_palette["accent"],       # light blue
      "Poor" = iisc_palette["primary"],       # dark blue
      "Very Poor" = "#FF7F00",                # orange
      "Unsuitable" = "#FF0000"                # red
    )
    
    # Map colors to the levels present in the data
    plot_colors <- quality_colors[levels(df_plot_clean$Quality)]
    
    # Ensure a proper title is shown for the plot
    plot_title <- "Piper Diagram of Water Samples"
    if (input$state_chem != "Select State") {
      plot_title <- paste0(plot_title, " (State: ", input$state_chem, ")")
    }
    if (input$district_chem != "Select District") {
      plot_title <- paste0(plot_title, " (District: ", input$district_chem, ")")
    }
    if (input$block_chem != "Select Block") {
      plot_title <- paste0(plot_title, " (Block: ", input$block_chem, ")")
    }
    
    # Create the Piper plot
    smwrGraphs::piperPlot(
      Ca = df_plot_clean$Ca_meq,
      Mg = df_plot_clean$Mg_meq,
      Na = df_plot_clean$Na_meq + df_plot_clean$K_meq, # Na+K as Na is common
      Cl = df_plot_clean$Cl_meq,
      HCO3 = df_plot_clean$HCO3_meq,
      SO4 = df_plot_clean$SO4_meq,
      # add title
      Plot = list(name = plot_title,
                  margin = c(0.1, 0.1, 0.1, 0.1)), # Adjust margins if title is cut off
      Points = list(
        col = plot_colors[df_plot_clean$Quality], # Color points by WQI Quality
        cex = 1.2 # Adjust point size
      ),
      key.title = "Water Quality",
      key.labels = names(plot_colors), # Use the actual quality labels
      cex.key = 1.2, # Adjust legend text size
      mar.key = c(0, 0, 0, 0) # Adjust key margin
    )
  })
  
  # --- Chemical Composition Plot ---
  output$chemCompPlot <- renderPlot({
    df_plot <- filtered_chemistry_data()
    req(nrow(df_plot) > 0)
    
    # Select meq/L columns for plotting
    ions_meq_cols <- c("Ca_meq", "Mg_meq", "Na_meq", "K_meq", "Cl_meq", "HCO3_meq", "SO4_meq")
    
    # Ensure numeric and fill NA with 0
    for (col in ions_meq_cols) {
      if (! (col %in% names(df_plot)) || !is.numeric(df_plot[[col]]) ) {
        df_plot[[col]] <- 0
      }
      df_plot[[col]][is.na(df_plot[[col]])] <- 0
    }
    
    # Calculate average composition (or take one sample if only one exists)
    avg_comp <- df_plot %>%
      summarise(across(all_of(ions_meq_cols), mean, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "Ion", values_to = "Concentration_meq") %>%
      mutate(Ion = gsub("_meq", "", Ion)) # Clean up ion names
    
    # Order ions for consistent plotting
    avg_comp$Ion <- factor(avg_comp$Ion, levels = c("Ca", "Mg", "Na", "K", "Cl", "HCO3", "SO4"))
    
    if (nrow(avg_comp) == 0 || all(is.na(avg_comp$Concentration_meq))) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No chemical data available for plotting.", size = 6) +
        theme_void()
    } else {
      ggplot(avg_comp, aes(x = Ion, y = Concentration_meq, fill = Ion)) +
        geom_bar(stat = "identity") +
        labs(
          title = paste0("Average Chemical Composition ", 
                         ifelse(input$state_chem != "Select State", paste0("(State: ", input$state_chem, ")"), ""),
                         ifelse(input$district_chem != "Select District", paste0(" (District: ", input$district_chem, ")"), ""),
                         ifelse(input$block_chem != "Select Block", paste0(" (Block: ", input$block_chem, ")"), "")
          ),
          y = "Average Concentration (meq/L)",
          x = "Ion Type"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c(
          "Ca" = "#005A9C", "Mg" = "#00A86B", "Na" = "#4FC3F7", "K" = "#A0C4FF",
          "Cl" = "#FFD54F", "HCO3" = "#EF9A9A", "SO4" = "#BDBDBD"
        )) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              legend.position = "none")
    }
  })
  
  # --- Water Types Section ---
  output$water_types_section <- renderUI({
    df_summary <- filtered_chemistry_data()
    req(nrow(df_summary) > 0)
    
    if (!"Quality" %in% names(df_summary)) {
      return(HTML("<p>Water Quality (WQI) data is not available.</p>"))
    }
    
    quality_counts <- df_summary %>%
      group_by(Quality) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      arrange(factor(Quality, levels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable"))) # Ensure consistent order
    
    total_samples <- sum(quality_counts$Count)
    
    if (total_samples == 0) {
      return(HTML("<h4>Water Quality Distribution (WQI)</h4><p>No samples found for the selected filters.</p>"))
    }
    
    html_output <- "<h4>Water Quality Distribution (WQI)</h4>"
    html_output <- paste0(html_output, "<p>Total Samples: ", total_samples, "</p>")
    html_output <- paste0(html_output, "<table class='table table-striped'>")
    html_output <- paste0(html_output, "<thead><tr><th>Quality Category</th><th>Number of Samples</th><th>Percentage</th></tr></thead><tbody>")
    
    for (i in 1:nrow(quality_counts)) {
      category <- quality_counts$Quality[i]
      count <- quality_counts$Count[i]
      percentage <- round((count / total_samples) * 100, 2)
      html_output <- paste0(html_output, "<tr><td>", category, "</td><td>", count, "</td><td>", percentage, "%</td></tr>")
    }
    
    html_output <- paste0(html_output, "</tbody></table>")
    
    # Add a simple bar chart for visualization
    plot_data <- quality_counts
    
    quality_plot <- ggplot(plot_data, aes(x = Quality, y = Count, fill = Quality)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), vjust = -0.5) +
      labs(title = "Distribution of Water Quality Index Categories",
           x = "Water Quality Category",
           y = "Number of Samples") +
      theme_minimal() +
      scale_fill_manual(values = c(
        "Excellent" = iisc_palette["secondary"],
        "Good" = iisc_palette["accent"],
        "Poor" = iisc_palette["primary"],
        "Very Poor" = "#FF7F00",
        "Unsuitable" = "#FF0000"
      )) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "none")
    
    tagList(
      HTML(html_output),
      renderPlot(quality_plot) # Render the plot here
    )
  })
  
  # --- Suggested Measures Section ---
  output$measures_section <- renderUI({
    df_measures <- filtered_chemistry_data()
    req(nrow(df_measures) > 0)
    
    if (!"Quality" %in% names(df_measures)) {
      return(HTML("<p>No water quality data to provide suggestions.</p>"))
    }
    
    quality_summary <- table(df_measures$Quality)
    
    suggestions <- ""
    
    if ("Unsuitable" %in% names(quality_summary) && quality_summary["Unsuitable"] > 0) {
      suggestions <- paste(suggestions, "<li><p><strong>Unsuitable Quality:</strong> Water in this category is not safe for drinking or general use without extensive treatment. Immediate action and alternative sources should be considered. Investigate specific contaminants for targeted remediation.</p></li>")
    }
    if ("Very Poor" %in% names(quality_summary) && quality_summary["Very Poor"] > 0) {
      suggestions <- paste(suggestions, "<li><p><strong>Very Poor Quality:</strong> Requires significant treatment before consumption. Long-term use without treatment can lead to health issues. Consider implementing advanced filtration (e.g., RO, activated carbon) or exploring alternative water sources.</p></li>")
    }
    if ("Poor" %in% names(quality_summary) && quality_summary["Poor"] > 0) {
      suggestions <- paste(suggestions, "<li><p><strong>Poor Quality:</strong> Not recommended for drinking. May require boiling or basic filtration. Monitor regularly and explore options for improving water quality or finding supplementary sources, especially for potable uses.</p></li>")
    }
    if ("Good" %in% names(quality_summary) && quality_summary["Good"] > 0) {
      suggestions <- paste(suggestions, "<li><p><strong>Good Quality:</strong> Generally safe for most uses. Regular monitoring is still advisable to detect any changes. Simple household filtration might be beneficial for taste or minor impurities.</p></li>")
    }
    if ("Excellent" %in% names(quality_summary) && quality_summary["Excellent"] > 0) {
      suggestions <- paste(suggestions, "<li><p><strong>Excellent Quality:</strong> Ideal for all purposes. Continue routine monitoring to maintain this high standard. Protect source from potential contamination.</p></li>")
    }
    
    if (suggestions == "") {
      return(HTML("<h4>Suggested Measures for Water Quality Improvement</h4><p>No specific quality issues detected or no data available to provide suggestions.</p>"))
    } else {
      return(HTML(paste0("<h4>Suggested Measures for Water Quality Improvement</h4><ul>", suggestions, "</ul>")))
    }
  })
  
  # --- Download Button ---
  output$download_chemistry_ui <- renderUI({
    req(data_storage$data_loaded) # Only show if data is loaded
    downloadButton("download_chemistry_data", "Download Filtered Data (CSV)", class = "btn-primary")
  })
  
  output$download_chemistry_data <- downloadHandler(
    filename = function() {
      paste("ground_water_chemistry_filtered-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Ensure to drop geometry before writing CSV
      write.csv(st_drop_geometry(filtered_chemistry_data()), file, row.names = FALSE)
    }
  )
  
} # End of server function

# Run App
shinyApp(ui, server)