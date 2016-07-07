# Define UI
shinyUI(navbarPage("Facebook V: Predicting Check Ins",
                   theme = shinytheme("cerulean"), #united
                   tabPanel("Visual inspection",
                            # Visual inspection sidebar panel
                            sidebarPanel(
                              selectInput("placeIdSel", "Place id selection",
                                          selected = placeIds[1], multiple = FALSE,
                                          choices = c(placeIds)),
                              fluidRow(column(3,actionButton("prevPlace", "Previous",
                                                             icon = icon("arrow-left"))),
                                       column(3,actionButton("nextPlace", "Next",
                                                             icon = icon("arrow-right")),
                                              offset=1)
                              ),
                              br(),
                              selectInput("analysisVar", "Analysis variable",
                                          choices = c(analysisVarsComp, "accuracyGroup"),
                                          selected = "x"
                              ),
                              fluidRow(column(3,actionButton("prevAnalysisVar", "Previous",
                                                             icon = icon("arrow-left"))),
                                       column(3,actionButton("nextAnalysisVar", "Next",
                                                             icon = icon("arrow-right")),
                                              offset=1)
                              ),
                              br(),
                              checkboxInput("wrapTime", "Wrap plots vs time",
                                            value = FALSE),
                              conditionalPanel(condition="input.wrapTime", 
                                               sliderInput("wrapCutoffTime", "Time wrap cutoff time",
                                                           min = min(dataSubset$time),
                                                           max = max(dataSubset$time),
                                                           value = median(dataSubset$time)))
                            ),
                            
                            # Visual inspection main panel
                            mainPanel(
                              tabsetPanel(
                                tabPanel("X-Y analysis",
                                         br(),
                                         radioButtons("xyType", "Plot type",
                                                      choices = c("Density", "Observations"),
                                                      selected = "Observations",
                                                      inline = TRUE),
                                         plotlyOutput("xyPlotly"),
                                         br(),
                                         fluidRow(column(6,
                                                         sliderInput("nbXYBins", "Number of x and y density bins",
                                                                     min = 5, max = 100, value = 50)
                                                         , offset=3
                                         ))
                                ),
                                tabPanel("Distribution",
                                         br(),
                                         fluidRow(column(4,
                                                         radioButtons("densityType", "Plot type",
                                                                      choices = c("Histogram", "Density"),
                                                                      inline = TRUE)
                                         ), column(4,
                                                   conditionalPanel(condition='input.analysisVar=="accuracy"',
                                                                    sliderInput("maxAcDensPlot", "Accuracy plot cutoff",
                                                                                min = 100, max=1000, value = 250))
                                         )
                                         ),
                                         plotlyOutput("densityPlotly"),
                                         fluidRow(column(6,
                                                         sliderInput("nbDensityBins", "Maximum number of distribution histogram bins",
                                                                     min = 5, max = 100, value = 50)
                                                         , offset=3
                                         ))
                                ),
                                tabPanel("Distribution Accuracy Group",
                                         br(),
                                         fluidRow(column(4,
                                                         radioButtons("accuracyDensityType", "Plot type",
                                                                      choices = c("Histogram", "Density"),
                                                                      inline = TRUE)
                                         ),
                                         column(2,checkboxInput("accuracyDensityRemoveOutliers",
                                                                "Remove outliers?", value=FALSE))
                                         ), 
                                         plotlyOutput("accuracyDensityPlotly"),
                                         fluidRow(column(6,
                                                         sliderInput("nbAccuracyDensityBins", "Maximum number of distribution histogram bins",
                                                                     min = 5, max = 100, value = 50)
                                                         , offset=3
                                         ))
                                ),
                                tabPanel("Custom comparison",
                                         br(),
                                         fluidRow(column(4,
                                                         selectInput("comparisonVar", "Comparison variable",
                                                                     choices = analysisVarsComp
                                                         )
                                         ), column(4,
                                                   selectInput("colComparisonVar", "Color variable",
                                                               choices = analysisVarsComp
                                                   )
                                         )
                                         ),
                                         radioButtons("compPlotType", "Plot type",
                                                      choices = c("Scatter", "Violin"),
                                                      selected = "Scatter",
                                                      inline = TRUE),
                                         plotlyOutput("comparisonPlotly")
                                ),
                                tabPanel("Plot data",
                                         br(),
                                         dataTableOutput('plotDataTable'))
                              )
                            )
                   ),
                   # About tab
                   tabPanel("About",
                            h4(HTML(aboutString))
                   )
))