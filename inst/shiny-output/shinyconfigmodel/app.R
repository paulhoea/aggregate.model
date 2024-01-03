
library(shiny)
library(DT)

# Define UI for app
ui <- fluidPage(
  titlePanel("The Aggregate Model"),

  HTML("<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"),

  fluidRow(
    column(
      width = 2,
      tabsetPanel(
        tabPanel("Model Settings",
                 numericInput("max_ar", "Maximum Number of Autoregressive Lags:", min = 0, max = 12, step = 1, value = 4),
                 numericInput("max_dl", "Maximum Number of Distributed Lags:", min = 0, max = 12, step = 1, value = 2),
                 checkboxInput("gets_selection", "Run GETS Selection?", value = TRUE),
                 numericInput("gets_pvalue", "GETS P-value:", min = 0.01, max = 0.5, value = 0.05),
                 checkboxInput("indicator_saturation", "Run Indicator Saturation?", value = TRUE),
                 numericInput("ind_sat_pval", "Indicator Saturation P-value:", min = 0.01, max = 0.5, value = 0.05),
                 textInput("save_file", "Save Processed Input Data (must be file ending with RDS, rds, Rds, csv, xls, xlsx):", value = "inputdata/processed_inputdata.csv")#,
                 #actionButton("run_button", "Run Model")
        ),
        tabPanel("Forecast Settings",
                 radioButtons("forecast_option", "Select Forecast Option:",
                              choices = c("Option 1", "Option 2"),
                              selected = "Option 1")#,
                 #actionButton("forecast_button", "Forecast Model")
        )
      ),
      actionButton("run_button", "Run Model"),
      actionButton("forecast_button", "Forecast Model")
    ),
    column(
      width = 4,
      tabsetPanel(
        tabPanel("Specification",
                 fileInput("spec", "Upload Specification (CSV)", accept = ".csv"),
                 DT::dataTableOutput("specification_table")
        ),
        tabPanel("Input Data",
                 fileInput("data", "Upload Input Data (CSV)", accept = ".csv"),
                 DT::dataTableOutput("input_data_table")
        ),
        tabPanel("Dictionary",
                 fileInput("dict", "Upload Dictionary (CSV)", accept = ".csv"),
                 DT::dataTableOutput("dictionary_table")
        ),
      )
    ),
    column(
      width = 5,
      h2("Model Output"),
      verbatimTextOutput("model_output"),
      h2("Model Forecast"),
      plotOutput("forecast_output")
      #tabsetPanel(
        # tabPanel("Model Output",
        #          verbatimTextOutput("model_output")
        # ),
        # tabPanel("Model Forecast",
        #          verbatimTextOutput("forecast_output")
        # )
      #)
    )
  )
)

# Default dictionary
default_dict <- aggregate.model::dict

# Default specification
default_spec <- dplyr::tibble(
  type = c("d", "d", "n", "n"),
  dependent = c("StatDiscrep", "TOTS", "Import", "EmiCO2Combustion"),
  independent = c("TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
                  "GValueAdd + Import",
                  "FinConsExpHH + GCapitalForm",
                  "HDD + HICP_Energy + GValueAdd")
)

default_input <- aggregate.model::sample_input

# Define server logic
server <- function(input, output, session) {

  # Define a reactive values object to store the user's input data, dictionary, and specification
  rv <- reactiveValues(
    inputdata = default_input,
    dictionary = default_dict,
    specification = default_spec,
    save_file = ""
  )

  observe({
    rv$max_ar <- input$max_ar
    rv$max_dl <- input$max_dl
    rv$saturation <- input$indicator_saturation
    rv$gets_select <- input$gets_selection
    rv$ind_sat_pval <- input$ind_sat_pval
    rv$gets_pval <- input$gets_pval
  })

  # Load the user's input data
  observe({
    req(input$data)
    rv$inputdata <- readr::read_csv(input$data$datapath, show_col_types = FALSE)
    rv$inputdirectory <- dirname(input$data$datapath)
  })

  # Load the user's dictionary
  observe({
    req(input$dict)
    rv$dictionary <- readr::read_csv(input$dict$datapath, show_col_types = FALSE)
  })

  # Load the user's specification
  observe({
    req(input$spec)
    rv$specification <- readr::read_csv(input$spec$datapath, show_col_types = FALSE)
  })

  # Render the input data table
  output$input_data_table <- renderDT({
    datatable(rv$inputdata, editable = FALSE)
  })

  # Render the dictionary table
  output$dictionary_table <- renderDT({
    datatable(rv$dictionary, editable = TRUE)
  })

  # Render the specification table
  output$specification_table <- renderDT({
    datatable(rv$specification, editable = TRUE)
  })

  # Update the reactive values object with the edited input data table
  observeEvent(input$input_data_table_cell_edit, {
    info <- input$input_data_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value

    rv$inputdata[row, col] <- value
  })

  # Update the reactive values object with the edited dictionary table
  observeEvent(input$dictionary_table_cell_edit, {
    info <- input$dictionary_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value

    rv$dictionary[row, col] <- value
  })

  # Update the reactive values object with the edited specification table
  observeEvent(input$specification_table_cell_edit, {
    info <- input$specification_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value

    rv$specification[row, col] <- value
  })

  # Update the reactive values object with the save file
  observe({
    rv$save_file <- input$save_file
  })

  # Function to run the model
  run_model_shiny <- function() {
    model_output <- aggregate.model::run_model(specification = rv$specification,
                                               dictionary = rv$dictionary,
                                               inputdata_directory = if (is.null(rv$inputdirectory)) { NULL } else { dirname(input$data$datapath) }, # inputdata
                                               primary_source = if (is.null(rv$inputdirectory)) { "download" } else { "local" },
                                               save_to_disk = rv$save_file,
                                               present = FALSE,
                                               quiet = TRUE,
                                               use_logs = "both",
                                               trend = TRUE,
                                               max.ar = rv$max_ar,
                                               max.dl = rv$max_dl,
                                               saturation = rv$saturation,
                                               saturation.tpval = rv$ind_sat_pval,
                                               gets_selection = rv$gets_select,
                                               selection.tpval = rv$gets_pval)

    # # Print or process the model output as needed
    # print(model_output)
    return(model_output)
  }

  # Function to forecast the model
  forecast_model_shiny <- function(){
    forecast_model(rv$model_output)
    # Add code to handle forecasting
  }

  # Run model when "Run Model" button is clicked
  observeEvent(input$run_button, {
    rv$model_output <- run_model_shiny()
  })

  # Forecast model when "Forecast Model" button is clicked
  observeEvent(input$forecast_button, {
    rv$forecast_output <- plot(forecast_model_shiny())
  })

  # Display the model output
  output$model_output <- renderPrint({
    rv$model_output
  })

  # Display the forecast output
  output$forecast_output <- renderPlot({
    rv$forecast_output
  })
}




























































# Define UI for app
# ui <- fluidPage(
#   titlePanel("The Aggregate Model"),
#
#   sidebarLayout(
#
#     sidebarPanel(
#       fileInput("data", "Upload Input Data (CSV)", accept = ".csv"),
#       fileInput("dict", "Upload Dictionary (CSV)", accept = ".csv"),
#
#       tabsetPanel(
#         tabPanel("Input Data",
#                  DT::dataTableOutput("input_data_table")
#         ),
#         tabPanel("Dictionary",
#                  DT::dataTableOutput("dictionary_table")
#         )
#       ),
#
#       selectInput("lags", "Maximum Lags:", choices = 0:4, selected = 1),
#       checkboxInput("gets_selection", "Run GETS Selection?", value = FALSE),
#       numericInput("gets_pvalue", "GETS P-value:", min = 0.01, max = 0.5, value = 0.05),
#       checkboxInput("indicator_saturation", "Run Indicator Saturation?", value = FALSE),
#       numericInput("ind_sat_pvalue", "Indicator Saturation P-value:", min = 0.01, max = 0.5, value = 0.05),
#
#       actionButton("run_button", "Run Model")
#     ),
#
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Model Output",
#                  verbatimTextOutput("model_output")
#         )
#       )
#     )
#   )
# )





# # Define UI for app
# ui <- navbarPage(title = "The Aggregate Model",
#                  tabPanel("Input",
#                           sidebarPanel(
#                             selectInput("lags", "Maximum lags:",
#                                         choices = 0:4, selected = 1),
#                             radioButtons("gets", "Run GETS selection?",
#                                          choices = list("Yes" = TRUE, "No" = FALSE),
#                                          selected = FALSE),
#                             numericInput("pvalue", "Select p-value:", value = 0.05, step = 0.01)
#                           ),
#                           mainPanel(
#                             tabsetPanel(
#                               tabPanel("Dictionary",
#                                        DT::dataTableOutput("dict_table")
#                               ),
#                               tabPanel("Input Data",
#                                        DT::dataTableOutput("input_table")
#                               )
#                             ),
#                             br(),
#                             actionButton("run_model", "Run Model")
#                           )
#                  ),
#                  tabPanel("Model Output",
#                           verbatimTextOutput("model_output")
#                  )
# )
#
# # Define server logic
# server <- function(input, output, session) {
#   # Load default dictionaries and specs
#   dictionary <- aggregate.model::dict
#   specs <- aggregate.model::spec
#
#   # Render dictionary table
#   output$dict_table <- DT::renderDataTable({
#     DT::datatable(dictionary, editable = TRUE, rownames = FALSE)
#   })
#
#   # Render input table
#   output$input_table <- DT::renderDataTable({
#     DT::datatable(specs, editable = TRUE, rownames = FALSE)
#   })
#
#   # Run model when button is clicked
#   run_model <- eventReactive(input$run_model, {
#     # Get edited dictionaries and specs
#     edited_dict <- input$dict_table_cell_edit
#     edited_dict_rows <- edited_dict$row
#     edited_dict_cols <- edited_dict$col
#     edited_dict_value <- edited_dict$value
#
#     edited_spec <- input$input_table_cell_edit
#     edited_spec_rows <- edited_spec$row
#     edited_spec_cols <- edited_spec$col
#     edited_spec_value <- edited_spec$value
#
#     # Update dictionary and specs with edited values
#     dictionary[cbind(edited_dict_rows, edited_dict_cols)] <- edited_dict_value
#     specs[cbind(edited_spec_rows, edited_spec_cols)] <- edited_spec_value
#
#     # Call run_model function
#     model_output <- aggregate.model::run_model(specs, dictionary, input$lags, input$gets, input$pvalue)
#
#     # Return model output
#     return(model_output)
#   })
#
#   # Render model output
#   output$model_output <- renderPrint({
#     run_model()
#   })
# }
#
# # Run the app
# shinyApp(ui, server)
#








# ui <- fluidPage(
#
#   tabsetPanel(
#     tabPanel("Input", fluid = TRUE,
#              mainPanel(
#                fileInput("upload", NULL, buttonLabel = "Upload Stored Model from Disk...", multiple = FALSE, accept = ".Rds"),
#                tableOutput("files"),
#                tags$hr(),
#                h1("Specification"),
#                DT::DTOutput("spec"),
#                h1("Dependency"),
#                plotOutput("network", height = "600", width = "800")#,
#                #tableOutput(outputId = "test")
#              )
#     ),
#     tabPanel("Graphs", fluid = TRUE,
#              dateRangeInput(inputId = "range_plot", label = "Date Range for Plots",
#                             start = as.Date("1960-01-01"), end = Sys.Date()),
#              mainPanel(
#                plotOutput("plots", height = "600", width = "800")
#              )
#     ),
#     tabPanel("Equations", fluid = TRUE,
#              mainPanel(
#                verbatimTextOutput("equations")
#              )
#     ),
#     tabPanel("Diagnostics", fluid = TRUE,
#              mainPanel(
#                DT::DTOutput("diag")
#              )
#     )
#   )
#
# )
#
# server <- function(input, output) {
#
#   # data <- reactive({
#   #   req(input$upload)
#   # })
#
#   # input$upload <- observe({
#   #   if (is.null(input$upload)){
#   #     input$upload <- getShinyOption("object", model)
#   #   }
#   # })
#
#
#   aggmod <- reactive({
#     if (is.null(input$upload)) {
#       getShinyOption("aggmodel_direct")
#     } else {
#       readRDS(file = input$upload$datapath)
#     }
#   })
#
#   wide <- reactive({
#     aggmod()$full_data %>%
#       tidyr::pivot_wider(names_from = na_item, values_from = values)
#   })
#
#   sel <- reactive({
#     f <- colnames(wide())[grep(colnames(wide()), pattern = "\\.hat")]
#     basef <- sub("\\.hat*", "", f)
#     sub <- wide() %>%
#       dplyr::select(time, union(basef, f)) %>%
#       tidyr::pivot_longer(cols = !time, names_to = c("variable", "type"), names_sep = "\\.", values_to = "value") %>%
#       dplyr::mutate(type = dplyr::case_when(is.na(type) ~ "observed",
#                                             type == "hat" ~ "fitted"))
#     return(sub)
#   })
#
#   eq <- reactive({
#     modulesprint <- aggmod()$module_collection %>%
#       filter(type == "n")
#     wholeprint <- ""
#     for (i in 1:NROW(modulesprint)) {
#       wholeprint <- capture.output(print(modulesprint[[i, "model"]]), file = NULL)
#     }
#     return(wholeprint)
#   })
#
#   # get_file_or_default <- reactive({
#   #   if (is.null(input$upload)) {
#   #     getShinyOption("aggmodel_direct ")
#   #   } else {
#   #     readRDS(file = input$upload$datapath)
#   #   }
#   # })
#   #
#   # output$test <- renderTable({
#   #   #if(is.null(input$upload)){
#   #    # tibble("test")
#   #   #} else {
#   #     get_file_or_default()$args$specification
#   #     #object$args$specification
#   #   #}
#   # })
#
#   output$test <- renderText(input$range_plot)
#
#   output$files <- renderTable(input$upload)
#   output$spec <- DT::renderDT(aggmod()$module_order)
#   output$plots <- renderPlot({
#     sel() %>%
#       filter(time >= as.Date(input$range_plot[1]) & time <= as.Date(input$range_plot[2])) %>%
#       ggplot2::ggplot(ggplot2::aes(x = time, y = value, color = type)) +
#       ggplot2::geom_line() +
#       ggplot2::facet_wrap(facets = "variable", scales = "free", nrow = length(unique(sel()$variable))) +
#       ggplot2::theme_minimal(base_size = 20) +
#       ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
#       ggplot2::labs(x = NULL, y = NULL)
#   })
#   output$equations <- renderPrint(eq(), width = 1000)
#   output$diag <- DT::renderDT({
#
#     diagnostics_model(aggmod()) %>%
#       DT::datatable() %>%
#       DT::formatStyle(columns = c("AR", "ARCH"),
#                       backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
#       DT::formatRound(columns = c("AR", "ARCH", "indicator_share"),
#                       digits = 4)
#
#   })
#   output$network <- renderPlot({
#     req(aggmod())
#     network(aggmod())
#   })
#
# }


shinyApp(ui = ui, server = server)


