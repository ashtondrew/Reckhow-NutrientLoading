shinyUI(navbarPage("Lake Phosphorus Model, Reckhow & Simpson 1980",
                   
                   tabPanel("Land Use & Population Values",
                            
                            h4("Explore Higgins Lake Land Use & Population Values"),
                            
                            h5(paste0("These choices allow the user to interactively explore the sensitivity of ",
                                      "Higgins Lake phosphorus concentration estimates to changes in land use and ",
                                      "population characteristics.  The default values represented below are those ",
                                      "published by Reckhow and Simpson (1980) based on survey and census data.",
                                      "The model calculations are based on millions of acres of each land cover type. ",
                                      "To constrain the user input to not exceed the total available watershed area, we ",
                                      "converted the visual representation to percent cover.")),
                            
                            fluidRow(
                                column(4,
                                       sliderInput("areaF", label = h5("Forest"), min = 0, max = 100, value = 75, step=5)),
                                column(4,
                                       sliderInput("areaA", label = h5("Agriculture"), min = 0, max = 100, value = 0, step=5)),
                                column(4,
                                       sliderInput("areaU", label = h5("Urban"), min = 0, max = 100, value = 5, step=5))
                            ),
                            
                            fluidRow(
                                tags$head(tags$style("#totperc{color: red; font-size: 14px; font-style: italic;}")),
                                column(offset=2, 10,
                                       h4(textOutput("totperc")))
                            ),
                            
                            fluidRow(
                                column(4,
                                       sliderInput("indS", label = h5("Household Size (seasonal)"), min = 0, max = 10, value = 3.5)),
                                column(4,
                                       sliderInput("daysS", label = h5("Use Days (seasonal)"), min = 0, max = 270, value = 60, step=10)),
                                column(4,
                                       sliderInput("unitS", label = h5("Units (seasonal)"), min = 0, max = 2000, value = 1000, step=200))
                            ),
                            
                            fluidRow(
                                column(offset=2, 4,
                                       sliderInput("indP", label = h5("Household Size (permanent)"), min = 0, max = 10, value = 0)),
                                column(4,
                                       sliderInput("unitP", label = h5("Units (permanent)"), min = 0, max = 2000, value = 0, step=200))
                            ),
                            
                            fluidRow(
                                column(12,
                                       h6(paste0("For the purposes of this visualization, we defined any residential use greater than 9 months (270 days) ",
                                                 "as permanent (year-round).")))  
                            ),
                            
                            tags$hr(),
                            
                            h4("Lake Phosphorus Concentration"),
                            
                            fluidRow(
                                column(offset=1, 3,
                                       sliderInput("civar2", label = h5("Set desired confidence interval:"), min = 0.55, max = 0.90, value = 0.90, step = 0.05)),
                                column(8,
                                       plotOutput("laketype2", width = "80%", height = "200px"))
                            ),
                            
                            fluidRow(
                                column(12,
                                       h5(textOutput("range2")))
                            )),
                   
                   tabPanel("Coefficient Values",
                            
                            h4("Explore Higgins Lake Coefficient Values"),
                            
                            h5(paste0("These choices allow the user to interactively explore the sensitivity of ",
                                      "Higgins Lake phosphorus concentration estimates to model coefficient values as defined in the Reckhow and ",
                                      "Simpson (1980) model.  The default coefficient values represented below are the low, high, and most ",
                                      "likely coefficients values published by Reckhow and Simpson (1980) based on literature review and expert judgement.",  
                                      "You can adjust the most likely values and observe subsequent changes in the expected phosphorus levels.")),
                            
                            fluidRow(
                                column(4,
                                       sliderInput("coefF", label = h5("Forest"), min = 2, max = 40, value = 20)),
                                column(4,
                                       sliderInput("coefA", label = h5("Agriculture"), min = 10, max = 80, value = 30)),
                                column(4,
                                       sliderInput("coefU", label = h5("Urban"), min = 50, max = 150, value = 90))
                            ),
                            
                            fluidRow(
                                column(4,
                                       sliderInput("coefPr", label = h5("Precipitation"), min = 15, max = 50, value = 30)),
                                column(4,
                                       sliderInput("coefSt", label = h5("Septic"), min = 0.3, max = 1, value = 0.6)),
                                column(4,
                                       sliderInput("coefSr", label = h5("Soil Retention"), min = 0.5, max = 0.95, value = 0.75))
                            ),
                            
                            fluidRow(
                                column(12,
                                       h6(paste0("Reckhow & Simpson (1980) included a coefficient for point source inputs, but this is excluded from the demonstration. ",
                                                 "Higgins Lake has no point source inputs, so this coefficient has no effect.")))
                            ),
                            
                            tags$hr(),
                            
                            h4("Lake Phosphorus Concentration"),
                            
                            fluidRow(
                                column(offset=1, 3,
                                       sliderInput("civar", label = h5("Set desired confidence interval:"), min = 0.55, max = 0.90, value = 0.90, step = 0.05)),
                                column(8,
                                       plotOutput("laketype", width = "80%", height = "200px"))
                            ),
                            
                            fluidRow(
                                column(12,
                                       h5(textOutput("range")))
                            )
                   ),
                   
                   tabPanel("About",
                             
                            withTags({
                                div(checked=NA,
                                    img(src="KDV_logo_name.png",width = "800px", height = "100px"),
                                    hr(),
                                    h3("This demonstration Shiny app is a direct transcription of the lake phosphorus model published in:"),
                                    h3(i("Reckhow KH, Simpson JT (1980) A procedure using modeling and error analysis for the prediction of lake phosphorus concentration from land use information.  Canadian Journal of Fisheries and Aquatic Sciences 37(9):1439-1448. DOI: 10.1139/f80-184")),
                                    hr(),
                                    h4(b("For more information about the original model, contact:")),
                                    h4(a(href="https://www.linkedin.com/in/kenreckhow", target="_blank", "Dr. Ken Reckhow (via LinkedIn)")),
                                    h4(a(href="http://kreckhow.blogspot.com/", target="_blank", "The Water Quality Wire")),
                                    h4("reckhow <insert at symbol> duke.edu"),
                                    br(),
                                    h4(b("For more information about this Shiny app, contact:")),
                                    h4(a(href="https://www.linkedin.com/in/ashtondrew", target="_blank", "Dr. Ashton Drew (via LinkedIn)")),
                                    h4(a(href="http://www.cashtondrew.com", target="_blank", "KDV Decision Analysis LLC")),
                                    h4("ashton <insert at symbol> cashtondrew.com")
                                    #hr(),
                                    #img(src="KDV_logo_name.png",width = "800px", height = "100px")
                
                                )
                            })
                   )

                   
))

        