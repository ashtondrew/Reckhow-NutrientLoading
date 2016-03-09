

shinyUI(fluidPage(
    
    titlePanel("Explore Higgins Lake Model Coefficient Values"),
    
    h4(paste0("This Shiny application allows the user to interactively explore the sensitivity of ",
              "Higgins Lake phosphorus concentration estimates to model coefficient values as defined in the Reckhow and ",
              "Simpson (1980) model.  The default coefficient values represented below are the low, high, and most ",
              "likely coefficients values published by Reckhow and Simpson (1980) based on literature review and expert judgement.",  
              "You can adjust the most likely values and observe subsequent changes in the expected phosphorus levels.")),
    
    fluidRow(
        column(4,
               sliderInput("coefF", label = h4("Forest"), min = 2, max = 40, value = 20)),
        column(4,
               sliderInput("coefA", label = h4("Agriculture"), min = 10, max = 80, value = 30)),
        column(4,
               sliderInput("coefU", label = h4("Urban"), min = 50, max = 150, value = 90))
    ),
    
    fluidRow(
        column(4,
               sliderInput("coefPr", label = h4("Precipitation"), min = 15, max = 50, value = 30)),
        column(4,
               sliderInput("coefSt", label = h4("Septic"), min = 0.3, max = 1, value = 0.6)),
        column(4,
               sliderInput("coefSr", label = h4("Soil Retention"), min = 0.5, max = 0.95, value = 0.75))
    ),
    
    fluidRow(
        column(12,
               h5(paste0("Reckhow & Simpson (1980) included a coefficient for point source inputs, but this is excluded from the demonstration. ",
                         "Higgins Lake has no point source inputs, so this coefficient has no effect.")))
    ),
    
    tags$hr(),
    
    h2("Lake Phosporus Concentration"),
    
    fluidRow(
        column(offset=1, 3,
               sliderInput("hvar", label = h4("Set desired confidence interval:"), min = 0.55, max = 0.95, value = 0.90, step = 0.05)),
        column(8,
               plotOutput("laketype", width = "80%", height = "200px"))
    ),
    
    fluidRow(
        column(12,
               h4(textOutput("range")))
    )
    
))

