#' @importFrom abind adrop abind
#' @importFrom alphahull ashape
#' @importFrom dplyr mutate_ filter_all any_vars tibble select_if as_tibble bind_rows
#' mutate_all bind_cols select slice pull all_vars "%>%" arrange_ left_join filter_ group_by 
#' group_by_ n summarise ungroup
#' @importFrom car ellipse
#' @importFrom coda mcmc raftery.diag gelman.diag geweke.diag heidel.diag
#' @importFrom colourpicker colourInput
#' @importFrom DataTools has_internet importDataUI importDataServer importOptions downloadModelUI
#'  downloadModelServer 
#' @importFrom DT dataTableOutput renderDataTable datatable coerceValue renderDT DTOutput
#' @importFrom futile.logger flog.warn
#' @importFrom ggplot2 ggplot ylab xlab aes_ geom_boxplot geom_density geom_histogram geom_line
#' theme element_text scale_fill_brewer labs ylim scale_fill_manual scale_color_manual
#' geom_point scale_color_brewer xlim aes geom_errorbar geom_smooth
#' @importFrom grDevices chull colorRampPalette
#' @importFrom htmltools save_html withTags
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom nimble calculateWAIC nimbleOptions nimbleModel configureMCMC buildMCMC compileNimble 
#' runMCMC nimbleFunction registerDistributions rmulti
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom plotly plot_ly layout add_trace renderPlotly plotlyOutput save_image
#' @importFrom RColorBrewer brewer.pal
#' @importFrom shiny actionButton bindEvent callModule checkboxInput column conditionalPanel 
#'  debounce div downloadButton downloadHandler eventReactive exportTestValues fileInput fluidRow
#'  helpText hideTab HTML includeHTML invalidateLater isolate is.reactive 
#'  mainPanel modalButton modalDialog moduleServer 
#'  navbarMenu NS numericInput observe observeEvent outputOptions plotOutput 
#'  reactive reactiveValues renderText radioButtons reactiveVal reactiveValuesToList removeModal 
#'  renderCachedPlot renderPlot renderPrint renderTable renderUI 
#'  req runApp selectInput selectizeInput setProgress showModal showTab sidebarLayout sidebarPanel 
#'  singleton sliderInput span tableOutput tabPanel tabsetPanel tagAppendAttributes tagList tags 
#'  testServer
#'  textAreaInput textInput textOutput uiOutput updateCheckboxInput updateNumericInput
#'  updateRadioButtons updateSelectInput updateSelectizeInput updateTextAreaInput updateTextInput
#'  validate withProgress
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs reset useShinyjs alert show hide
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom shinyTools formatRangesOfGGplot formatTitlesOfGGplot plotExportButton 
#'  plotExportServer plotRangesUI plotRangesServer plotTitlesServer plotTitlesUI shinyTryCatch
#'  vectorInputServer vectorInputUI
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats dist setNames runif acf na.omit qnorm cov pnorm rbeta dbeta pchisq dnorm manova
#' var sd cov2cor p.adjust var.test t.test shapiro.test median quantile rnorm pbeta qbeta qchisq 
#' @importFrom templates tmpl tmplEval
#' @importFrom tibble add_row add_column
#' @importFrom tidyr gather
#' @importFrom tools file_path_sans_ext
#' @importFrom utils data packageVersion head combn compareVersion tail
#' installed.packages write.table capture.output read.table
#' @importFrom yaml read_yaml
NULL
