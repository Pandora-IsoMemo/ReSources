#' UI function for the FRUITS Tab / Module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
fruitsUI <- function(id, title = "FRUITS") {
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    useShinyjs(),
    fluidRow(
      # Left sidebar ----
      sidebarPanel(
        style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
        width = 2,
        importUI(ns("modelUpload"), label = "Import Model"),
        tags$br(), tags$br(),
        downloadModelUI(ns("modelDownload"), label = "Save Model"),
        # selectInput(
        #   ns("exampleData"),
        #   label = "Select example models",
        #   choices = c(
        #     "Five Sources Data",
        #     "Brown Bear Data",
        #     "Black Bear Data",
        #     "Roman Data"
        #   ),
        #   selected = "Five_Sources_Data"
        # ),
        # actionButton(ns("exampleModel"), "Load selected model"),
        tags$hr(),
        checkboxInput(ns("onlyShowNimbleInput"),
                      "Only show nimbleModel() input", 
                      width = "100%"),
        conditionalPanel(
          ns = ns,
          condition = "input.onlyShowNimbleInput == true",
          helpText("After 'Run': Check in 'Results Report' the sections 'Model code' and 'Model inputs'.")
        ),
        fluidRow(
          actionButton(ns("run"), "Run", width = "100%") %>%
            column(width = 5),
          actionButton(ns("reset"), "Reset", width = "100%") %>%
            column(width = 5, align = "right", offset = 2)
        ),
        #checkboxInput(ns("adaptiveNames"), "Adaptive Names", value = FALSE),
        tags$hr(),
        dbContentSelectUI(ns("popUpTables"), label = "Data table"),
        tags$button(
          class = "btn btn-default",
          type = "button",
          onClick = "javascript:window.open('https://isomemoapp.com/app/iso-memo-app', '_blank')",
          "IsoMemo App"
        ),
        div(
          style = "display:none;",
          verbatimTextOutput(ns("status")),
          verbatimTextOutput(ns("statusSim"))
        )
      ),
      # Main panel ----
      mainPanel(
        width = 10,
        tabsetPanel(
          id = ns("mainTabs"),
          type = "tabs",
          ## Data ----
          navbarMenu(
            "Data",
            tabPanel(
              "Target & target-to-source offsets",
              targetValuesUI(ns("targetVals"), title = "Target & target-to-source offsets")
            ),
            tabPanel(
              "Components",
              componentsUI(ns("components"), title = "Components")
            ),
            tabPanel(
              "Sources",
              sourcesUI(ns("sources"), title = "Sources")
            ),
            tabPanel(
              "Concentrations",
              concentrationsUI(ns("concentration"), title = "Concentrations")
            )
          ),
          ## Model options ----
          tabPanel(
            "Model options",
            fluidRow(
              column(
                width = 8,
                tags$h4("Model options"),
                fluidRow(
                  column(
                    width = 6,
                    radioButtons(
                      ns("modelType"),
                      "Model type:",
                      choices = list(
                        "Update model (all info shared)" = "1",
                        "Individual targets (partially shared info)" = "2",
                        "Baseline model (partially shared info)" = "3",
                        "Individual targets (no shared info)" = "4",
                        "Baseline model (no shared info)" = "5"
                      ),
                      selected = 1
                    ),
                    numericInput(
                      ns("minUnc"),
                      "Minimum uncertainty",
                      value = 0.005,
                      min = 0.0001,
                      max = 1,
                      step = 0.001
                    ),
                    checkboxInput(ns("modelWeights"), "Include components",
                                  value = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.modelWeights == true",
                      ns = ns,
                      checkboxInput(
                        ns("modelWeightsContrained"),
                        "Constrain weights between 0 and 100",
                        value = TRUE
                      )
                    ),
                    checkboxInput(ns("modelConcentrations"), "Include concentrations",
                                  value = TRUE
                    ),
                    conditionalPanel(
                      condition = "input.modelConcentrations == true",
                      ns = ns,
                      checkboxInput(
                        ns("modelConcentrationsContrained"),
                        "Constrain concentration between 0 and 100",
                        value = TRUE
                      )
                    ),
                    radioButtons(
                      ns("inflatedBeta"),
                      "Source contribution distribution:",
                      choices = list(
                        "Dirichlet" = 0,
                        "Zero Inflated Beta" = 1
                      ),
                      selected = 0
                    ),
                    checkboxInput(
                      ns("optimalPrior"),
                      "Optimal objective prior",
                      value = TRUE
                    ),
                    conditionalPanel(
                      condition = "input.optimalPrior == false && input.inflatedBeta == '0'",
                      ns = ns,
                      vectorInputUI(ns("alphaHyper"),
                                    "Hyperparameters for alpha/sources",
                                    newValueRange = c(0.0001, 100))
                    ),
                    checkboxInput(
                      ns("oxcalCheck"),
                      "Show interface for Oxcal export",
                      value = FALSE
                    )
                  ),
                  column(
                    width = 6,
                    radioButtons(
                      ns("covariateType"),
                      "Covariates model:",
                      choices = list(
                        "none" = 0,
                        "fixed intercept (cat. vars), fixed slope (num. vars)" = 1,
                        "random intercept (cat. vars), fixed slope (num. vars)" = 2,
                        "random intercept (cat. vars), random slope (num. vars)" = 3
                      ),
                      selected = 0
                    ),
                    conditionalPanel(
                      condition = "input.covariateType !== '0'",
                      ns = ns,
                      pickerInput(
                        ns("categoricalVars"),
                        "Select categorical variables",
                        choices = list(),
                        selected = NULL,
                        multiple = TRUE
                      ),
                      pickerInput(
                        ns("numericVars"),
                        "Select numeric variables",
                        choices = list(),
                        selected = NULL,
                        multiple = TRUE
                      )
                    )
                  )
                )
              ),
              column(
                width = 3,
                offset = 1,
                tags$h4("MCMC options"),
                numericInput(ns("burnin"), "Burn-In", value = 1e4, step = 1e3),
                numericInput(
                  ns("iterations"),
                  "Iterations",
                  value = 1e4,
                  step = 1e3
                ),
                numericInput(
                  ns("thinning"),
                  "Thinning",
                  value = 10,
                  step = 1
                ),
                numericInput(
                  ns("nchains"),
                  "Number of chains",
                  value = 1,
                  step = 1
                )
              )
            )
          ),
          ## Priors ----
          tabPanel(
            "Prior Info",
            tags$h4("All Priors"),
            priorInput(ns("priors")),
            tags$br(),
            tags$br(),
            textInput(ns("newPrior"), "New prior", width = "100%"),
            div(class = "text-danger", textOutput(ns("priorWarning"))),
            fluidRow(
              column(width = 2, actionButton(ns("addPrior"), "Add prior")),
              column(width = 2, checkboxInput(ns("addUnc"), "Add uncertainty", width = "100%")),
              column(
                width = 6,
                numericInput(
                  ns("Unc"),
                  "Minimum uncertainty",
                  value = 0.001,
                  width = "100%",
                  step = 0.001,
                  min = 0.001
                )
              )
            ),
            tags$br(),
            div(
              class = "calculatorInputContainer",
              fluidRow(
                column(
                  4,
                  priorCalculator(),
                  tags$br(),
                  tags$p("Functions"),
                  priorCalculator(funs = TRUE)
                ),
                column(
                  4,
                  selectInput(
                    ns("priorSource"),
                    "Source contributions",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  selectInput(
                    ns("priorProxies"),
                    "Source contributions by proxy",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  conditionalPanel(
                    condition = "output.targetOffset == true",
                    selectInput(
                      ns("priorOffset"),
                      "Target-to-source offsets",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  conditionalPanel(
                    condition = "input.modelConcentrations == true",
                    ns = ns,
                    selectInput(
                      ns("priorConcentration"),
                      "Concentrations",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    )
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true & input.covariateType !== '0'",
                    ns = ns,
                    selectInput(
                      ns("priorHierarchicalValues"),
                      "Source contribution categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    selectInput(
                      ns("priorHierarchicalValuesBeta"),
                      "Component contributions categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    selectInput(
                      ns("priorHierarchicalValuesTheta"),
                      "Source contributions by proxy categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    )
                  )
                ),
                column(
                  4,
                  selectInput(
                    ns("priorSourceFractions"),
                    "Component contributions",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  selectInput(
                    ns("priorProxyValues"),
                    "Component proxies",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  conditionalPanel(
                    condition = "input.modelWeights == true",
                    selectInput(
                      ns("priorWeightValues"),
                      "Weights",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  selectInput(
                    ns("priorConsumerValues"),
                    "Target proxies",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true && (input.modelType == 5 || input.modelType == 4)"
                    ,
                    selectInput(
                      ns("priorProxyHierarchicalValues"),
                      "Component proxies categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true && (input.modelType == 5 || input.modelType == 4)"
                    ,
                    selectInput(
                      ns("priorConsumerHierarchicalValues"),
                      "Target proxies categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  
                  conditionalPanel(
                    condition = "input.modelConcentrations == true && output.targetValuesShowCovariates == true && (input.modelType == 5 || input.modelType == 4)"
                    ,
                    selectInput(
                      ns("priorConcentrationHierarchicalValues"),
                      "Concentrations categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  conditionalPanel(
                    condition = "input.modelWeights == true && output.targetValuesShowCovariates == true && (input.modelType == 5 || input.modelType == 4)"
                    ,
                    selectInput(
                      ns("priorWeightHierarchicalValues"),
                      "Weight categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  )
                  
                )
              )
            )
          ),
          ## User Estimates ----
          tabPanel(
            "User Estimates",
            tags$h4("Estimates"),
            priorInput(ns("userEstimate")),
            userEstimateGroupUI(ns("userEstimateGroup")),
            tags$br(),
            tags$br(),
            textInput(ns("newUserEstimate"), "New user estimate", width = "100%"),
            div(class = "text-danger", textOutput(ns(
              "userEstimateWarning"
            ))),
            actionButton(ns("addUserEstimate"), "Add user estimate"),
            div(
              class = "calculatorInputContainerUserEstimate",
              fluidRow(
                column(
                  4,
                  userEstimatesCalculator(),
                  tags$br(),
                  tags$p("Functions"),
                  userEstimatesCalculator(funs = TRUE)
                ),
                column(
                  4,
                  textInput(ns("userEstimateName"), "Estimate Name", value = "UserEstimate"),
                  div(
                    class = "user-estimates-additions",
                    div(
                      style = "display:inline-block",
                      tags$button(
                        id = "addUserEstimateConstant", class =
                          "btn btn-default", "Add Constant"
                      )
                    ),
                    div(
                      style = "display:inline-block",
                      tags$input(id = "userEstimateConstant", value = 0)
                    )
                  ),
                  div(
                    class = "user-estimates-additions",
                    div(
                      style = "display:inline-block",
                      tags$button(
                        id = "addUserEstimateND",
                        class = "btn btn-default",
                        "Add Normal Distribution (Mean, SD)"
                      )
                    ),
                    div(
                      style = "display:inline-block",
                      tags$input(id = "userEstimateNDMean", value = 0)
                    ),
                    div(
                      style = "display:inline-block",
                      tags$input(id = "userEstimateNDSd", value = 1)
                    )
                  )
                ),
                column(
                  4,
                  selectInput(
                    ns("userEstimateSource"),
                    "Source contributions",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  selectInput(
                    ns("userEstimateProxies"),
                    "Contributions by proxy",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  selectInput(
                    ns("userEstimateSourceFractions"),
                    "Component contributions",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  conditionalPanel(
                    condition = "output.targetOffset == true",
                    selectInput(
                      ns("userEstimateOffset"),
                      "Target-to-source offsets",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  conditionalPanel(
                    condition = "input.modelConcentrations == true",
                    selectInput(
                      ns("userEstimateConcentration"),
                      "Concentrations",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true",
                    selectInput(
                      ns("userEstimateHierarchicalValues"),
                      "Source contribution categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    selectInput(
                      ns("userEstimateHierarchicalValuesBeta"),
                      "Component contributions categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    selectInput(
                      ns("userEstimateHierarchicalValuesTheta"),
                      "Source contributions by proxy categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  selectInput(
                    ns("userEstimateProxyValues"),
                    "Component proxies",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  conditionalPanel(
                    condition = "input.modelWeights == true",
                    selectInput(
                      ns("userEstimateWeightValues"),
                      "Weights",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  selectInput(
                    ns("userEstimateConsumerValues"),
                    "Target proxies",
                    NULL,
                    selectize = FALSE,
                    multiple = TRUE,
                    size = 3
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true && (input.modelType == 5 || input.modelType == 4)"
                    ,
                    selectInput(
                      ns("userEstimateProxyHierarchicalValues"),
                      "Component proxies categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true && (input.modelType == 5 || input.modelType == 4)"
                    ,
                    selectInput(
                      ns("userEstimateConsumerHierarchicalValues"),
                      "Target proxies categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  
                  conditionalPanel(
                    condition = "input.modelConcentrations == true && output.targetValuesShowCovariates == true && (input.modelType == 5 || input.modelType == 4)"
                    ,
                    selectInput(
                      ns("userEstimateConcentrationHierarchicalValues"),
                      "Concentrations categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  ),
                  conditionalPanel(
                    condition = "input.modelWeights == true && output.targetValuesShowCovariates == true && (input.modelType == 5 || input.modelType == 4)"
                    ,
                    selectInput(
                      ns("userEstimateWeightHierarchicalValues"),
                      "Weight categories",
                      NULL,
                      selectize = FALSE,
                      multiple = TRUE,
                      size = 3
                    ),
                    ns = ns
                  )
                )
              )
            )
          ),
          ## Model Characteristics ----
          tabPanel(
            "Model Characteristics",
            tags$h4("Model Characteristics"),
            value = "Characteristics",
            sidebarPanel(
              checkboxInput(ns("showConfidence"),
                            label = "Show credible bars/ellipses",
                            value = TRUE
              ),
              checkboxInput(ns("showLegend"),
                            label = "Show legend if available",
                            value = FALSE
              ),
              conditionalPanel(
                condition = "input.showLegend == true",
                ns = ns,
                checkboxInput(ns("legendInside"),
                              label = "Show legend within plot",
                              value = FALSE
                )
              ),
              sliderInput(
                ns("confidenceLevel"),
                label = "Credible level",
                min = 0.5,
                max = 0.9999,
                value = 0.9
              ),
              radioButtons(
                ns("horizontalPlot"),
                label = "Horizontal or vertical 1D plots?",
                choices = c("horizontal", "vertical"),
                selected = "vertical"
              ),
              tags$hr(),
              helpText("Click on 'Simulate' for additional characteristics:"),
              div(foodIntakesButton(
                ns("foodIntakes"), "Add Source Contributions"
              )),
              tags$br(),
              div(
                sliderInput(
                  ns("seqSim"),
                  label = "Mixture plot: source shares simulation split",
                  min = 2,
                  max = 20,
                  value = 10
                )
              ),
              pickerInput(
                inputId = ns("simSpecSources"),
                label = "Select sources for simulation",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              ),
              div(actionButton(ns("runModelChar"), "Simulate")),
              tags$br(),
              conditionalPanel(
                condition = "output.statusSim == 'COMPLETED'",
                ns = ns,
                exportDataUI(ns("exportSimSources"), "Export simulated data")
              ),
              width = 3
            ),
            fluidRow(mainPanel(
              width = 8,
              tabsetPanel(
                id = ns("MCharResults"),
                tabPanel(
                  "Target values plot",
                  pickerInput(
                    inputId = ns("targetSelect"),
                    label = "Select up to three proxies",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true",
                    ns = ns,
                    pickerInput(
                      ns("characteristicsCovariatesTarget"),
                      "Compare and show covariates",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE
                    )
                  ),
                  plotlyOutput(outputId = ns("targetPlot")),
                  plotExportButton(ns("exportTargetPlot")),
                  helpText(
                    "Please export plot using the camera symbol at the top-right of the plot."
                  )
                ),
                tabPanel(
                  "Concentrations plot",
                  value = "concentrationsPlot",
                  pickerInput(
                    inputId = ns("concentrationsSelect"),
                    label = "Select up to three fractions",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
                  ),
                  plotlyOutput(outputId = ns("concentrationsPlot")),
                  plotExportButton(ns("exportConcentrationsPlot")),
                  helpText(
                    "Please export plot using the camera symbol at the top-right of the plot."
                  )
                ),
                tabPanel(
                  "Source Plot",
                  value = "sourcePlot",
                  pickerInput(
                    inputId = ns("sourceSelect"),
                    label = "Select up to three proxies",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
                  ),
                  checkboxInput(ns("showIndividuals"), label = "Show targets", value = TRUE),
                  conditionalPanel(
                    condition = "input.showIndividuals == true",
                    ns = ns,
                    checkboxInput(ns("showTargetNames"), label = "Show target names", value = TRUE)
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true",
                    ns = ns,
                    pickerInput(
                      ns("characteristicsCovariates"),
                      "Compare and show covariates",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE
                    )
                  ),
                  plotlyOutput(outputId = ns("SourceCharacteristicsPlot")),
                  plotExportButton(ns("exportSourceCharacteristicsPlot")),
                  helpText(
                    "Please export plot using the camera symbol at the top-right of the plot."
                  )
                ),
                tabPanel(
                  "Source Mixture Plot",
                  value = "sourceMixPlot",
                  pickerInput(
                    inputId = ns("sourceSelectMix"),
                    label = "Select one, two or three proxies",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
                  ),
                  pickerInput(
                    inputId = ns("sourceSelectMix2"),
                    label = "Select sources",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
                  ),
                  checkboxInput(
                    ns("showIndividualsMix"),
                    label = "Show targets",
                    value = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.showIndividualsMix == true",
                    ns = ns,
                    checkboxInput(
                      ns("showTargetNamesMix"),
                      label = "Show target names",
                      value = TRUE
                    )
                  ),
                  conditionalPanel(
                    condition = "output.targetValuesShowCovariates == true",
                    ns = ns,
                    pickerInput(
                      ns("characteristicsCovariatesMix"),
                      "Compare and show covariates",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE
                    )
                  ),
                  checkboxInput(ns("showGrid"), label = "Show grid", value = TRUE),
                  checkboxInput(ns("showPoints"), label = "Show grid points", value = FALSE),
                  radioButtons(
                    ns("hull"),
                    label = "Outer hull",
                    choices = c("convex hull", "alpha convex hull")
                  ),
                  conditionalPanel(
                    condition = "input.hull == 'alpha convex hull'",
                    ns = ns,
                    numericInput(
                      ns("alpha"),
                      label = "Alpha value convex hull (higher = more conservative)",
                      value = 10,
                      min = 0,
                      max = 100,
                      step = 0.01
                    )
                  ),
                  # actionButton(ns("updateMix"), label = "Update Plot"),
                  plotlyOutput(outputId = ns("SourceCharacteristicsPlot2")),
                  plotExportButton(ns("exportSourceCharacteristicsPlot2")),
                  helpText(
                    "Please export plot using the camera symbol at the top-right of the plot."
                  )
                ),
                tabPanel(
                  "Z-Scores sources",
                  value = "zScorePlot",
                  DT::dataTableOutput(ns("zScores")),
                  exportDataUI(ns("exportZScores"), "Export Data")
                ),
                tabPanel(
                  "Source separation test (MANOVA)",
                  value = "scoreSepTest",
                  verbatimTextOutput(ns("scoreSep"), download = TRUE)
                ),
                tabPanel(
                  "Mahalanobis-Distance sources",
                  value = "mahaPlot",
                  DT::dataTableOutput(ns("mahaDist")),
                  exportDataUI(ns("exportMahaDist"), "Export Data")
                ),
                tabPanel(
                  "Target & Source summary",
                  value = "corrPlot",
                  verbatimTextOutput(ns("corrMat"), download = TRUE)
                )
              )
            ))
          ),
          ## Results Report ----
          navbarMenu(
            "Results Report",
            menuName = "resultsReport",
            tabPanel(
              "Summary statistics",
              fluidRow(
                sidebarPanel(
                  tags$h5("Select additional summary statistics"),
                  checkboxInput(ns("SummaryMin"), "Minimum"),
                  checkboxInput(ns("SummaryMax"), "Maximum"),
                  checkboxInput(ns("SummaryMedian"), "Median"),
                  checkboxInput(ns("SummaryQuantileCheck"), "Quantiles"),
                  conditionalPanel(
                    condition = "input.SummaryQuantileCheck == true",
                    ns = ns,
                    sliderInput(
                      inputId = ns("SummaryQuantile"),
                      label = "Select quantile",
                      min = 0,
                      max = 1,
                      value = 0.95,
                      width = "100%",
                      step = 0.001
                    ),
                    sliderInput(
                      inputId = ns("SummaryQuantile2"),
                      label = "Select quantile",
                      min = 0,
                      max = 1,
                      value = 0.99,
                      width = "100%",
                      step = 0.001
                    )
                  ),
                  checkboxInput(ns("BayesianPValuesCheck"), "P-values"),
                  conditionalPanel(
                    condition = "input.BayesianPValuesCheck == true",
                    ns = ns,
                    numericInput(
                      ns("pVal"),
                      "Test against value of:",
                      value = 0,
                      step = 0.1
                    )
                  ),
                  exportDataUI(ns("exportSummaryData"), "Export Data"),
                  width = 3
                ),
                mainPanel(DT::dataTableOutput(ns("SummaryResults")), width = 8)
              )
            ),
            tabPanel(
              "Model code",
              tags$h4("Model code"),
              verbatimTextOutput(ns("modelCode"), download = TRUE)
            ),
            tabPanel(
              "Model inputs",
              tags$h4("Model inputs"),
              tabsetPanel(
                id = ns("ModelInput"),
                tabPanel(
                  "Data",
                  verbatimTextOutput(ns("modelInputData"), download = TRUE)
                ),
                tabPanel(
                  "Names",
                  verbatimTextOutput(ns(
                    "modelInputValueNames"
                  ), download = TRUE)
                ),
                tabPanel(
                  "Model options",
                  verbatimTextOutput(
                    ns("modelInputModelOptions"),
                    download = TRUE
                  )
                ),
                tabPanel(
                  "Priors",
                  verbatimTextOutput(ns(
                    "modelInputPriors"
                  ), download = TRUE)
                ),
                tabPanel(
                  "User estimates",
                  verbatimTextOutput(ns(
                    "modelUserEstimates"
                  ), download = TRUE)
                )
              )
            )
          ),
          ## Diagnostics ----
          navbarMenu(
            "Model Diagnostics",
            menuName = "modelDiagnostics",
            tabPanel(
              "Convergence plots",
              value = "modelDiagnosticsTab",
              tags$h4("Convergence plots"),
              conditionalPanel(
                condition = "output.status == 'COMPLETED'",
                ns = ns,
                modelDiagnosticsPlotUI(ns("modelDiagnosticsPlot"))
              )
            ),
            tabPanel(
              "Convergence tests",
              tags$h4("Convergence tests"),
              tabsetPanel(
                id = ns("Convergence"),
                tabPanel(
                  "Geweke",
                  verbatimTextOutput(ns("geweke"))
                ),
                tabPanel(
                  "Raftery-Lewis",
                  verbatimTextOutput(ns("raftery"))
                ),
                tabPanel(
                  "Heidelberg-Welch",
                  verbatimTextOutput(ns("heidel"))
                ),
                tabPanel(
                  "Gelman-Rubin",
                  verbatimTextOutput(ns("gelman"))
                )
              ),
              exportDataUI(ns("exportDataChainsAll"), "Export all chains")
            ),
            tabPanel(
              "Information criteria",
              tags$h4("Information criteria"),
              tabsetPanel(
                id = ns("Convergence"),
                tabPanel(
                  "wAIC",
                  verbatimTextOutput(ns("wAIC"))
                ),
                tabPanel(
                  "BIC",
                  verbatimTextOutput(ns("BIC"))
                ),
                tabPanel(
                  "Bayesian p-value (posterior predictive p-value)",
                  DT::dataTableOutput(ns("pValue")),
                  width = 4
                )
              )
            )
          ),
          ## Output ----
          navbarMenu(
            "Output",
            menuName = "Output",
            ### Output Plots ----
            tabPanel(
              "Output Plots",
              value = "output",
              conditionalPanel(
                condition = "output.status == 'COMPLETED'",
                ns = ns,
                outputPlotUI(ns("outputPlot"))
              )
            ),
            ### Export Output to IsoMemo-App ----
            tabPanel(
              "Export Output to IsoMemo-App",
              value = "isomemo",
              conditionalPanel(
                condition = "output.targetValuesShowCoordinates == false",
                ns = ns,
                tags$h5(
                  "Please add coordinates in the data - Target Values tab to export results to the IsoMemo App"
                )
              ),
              conditionalPanel(
                condition = "output.targetValuesShowCoordinates == true",
                ns = ns,
                radioButtons(
                  ns("exportType"),
                  "Type of Export",
                  c(
                    "Proxy" = "proxy",
                    "Source contributions" = "Source contributions",
                    "Component contributions" = "Component contributions",
                    "Source contributions by proxy" = "Source contributions by proxy"
                  ),
                  selected = "Source contributions"
                ),
                conditionalPanel(
                  condition = "input.exportType == 'proxy'",
                  ns = ns,
                  selectInput(ns("exportProxy"), "Proxy", choices = NULL)
                ),
                conditionalPanel(
                  condition = "input.exportType == 'Source contributions'",
                  ns = ns,
                  selectInput(ns("exportSources"), "Source contributions", choices = NULL)
                ),
                conditionalPanel(
                  condition = "input.exportType == 'Component contributions'",
                  ns = ns,
                  selectInput(ns("exportBeta"), "Component contributions", choices = NULL)
                ),
                conditionalPanel(
                  condition = "input.exportType == 'Source contributions by proxy'",
                  ns = ns,
                  selectInput(ns("exportTheta"), "Source contributions by proxy", choices = NULL)
                ),
                conditionalPanel(
                  condition = "input.exportType == 'userEstimates'",
                  ns = ns,
                  selectInput(ns("exportUserEstimates"), "UserEstimates", choices = NULL)
                ),
                conditionalPanel(
                  condition = "output.targetValuesShowCovariates == true",
                  ns = ns,
                  checkboxInput(
                    inputId = ns("useSite"),
                    label = "Include \"Site\" variable from covariates table (optional)",
                    value = FALSE
                  )
                ),
                conditionalPanel(
                  condition = "input.useSite == true",
                  ns = ns,
                  selectInput(
                    ns("siteExport"),
                    "Site variable from \"Covariates\" table",
                    choices = NULL
                  )
                ),
                tableOutput(ns("exportPreview")),
                if (!isoInstalled()) {
                  helpText(
                    paste(
                      "To export data to fruits you need the package MpiIsoApp installed in version >=",
                      isoVersion()
                    )
                  )
                } else {
                  NULL
                },
                actionButton(ns("exportToIsoMemo"), "Export to IsoMemo App")
              )
            ),
            ### OxCal ----
            tabPanel(
              "Oxcal export",
              OxCalOutputUI(ns("oxcal"))
            )
          )
        )
      )
    )
  )
}
