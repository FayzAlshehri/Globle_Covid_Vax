



ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Covid-19 Vaccinations Dashboard",
                                    titleWidth = 400),
                    
                    dashboardSidebar(
                      collapsed = TRUE,
                      width = "0px"),
                    
                    # Application title
                    
                    dashboardBody(  tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
                                    navbarPage(title = "Review the Dashboard Using the Tabs:",
                                               
                                               tabPanel("Global Herd Immunity",
                                                        
                                                        fluidRow(
                                                          infoBoxOutput("TotalBoosterBox"),
                                                          infoBoxOutput("TotalvaccinationBox"),
                                                          infoBoxOutput("LatesetDateBox"),
                                                        ),
                                                        
                                                        
                                                        
                                                        
                                                        plotOutput( "graph_allworld")
                                                        
                                               ),
                                               
                                               
                                               
                                               
                                               
                                               tabPanel("Saudi Arabia ",
                                                        fluidRow(
                                                          infoBoxOutput("TotalBoosterBox_SA"),
                                                          infoBoxOutput("TotalvaccinationBox_SA"),
                                                          infoBoxOutput("LatesetDateBox_SA"),
                                                          infoBoxOutput("TotalPercentDateBox_SA"),
                                                          infoBoxOutput("FullyPercentDateBox_SA"),
                                                          infoBoxOutput("DailyVaxDateBox_SA")
                                                        ),
                                                        
                                                        
                                                        sidebarPanel(
                                                          sliderInput("total_vac_saudi", "Total vaccinations(at least one dose):", 
                                                                      min(0),
                                                                      max(50000000),
                                                                      value = c(10000000, 30000000), 
                                                                      step = 1000000)
                                                          
                                                          
                                                        ),
                                                        mainPanel(
                                                          
                                                          plotOutput("total_vaccin_Plot_saudi")
                                                        ),
                                                        
                                                        
                                                        
                                                        sidebarPanel(
                                                          sliderInput("Fully_vac_saudi", "Fully vaccinated:", 
                                                                      min(0),
                                                                      max(50000000),
                                                                      value = c(10000000, 30000000), 
                                                                      step = 1000000)
                                                        ),
                                                        mainPanel(
                                                          
                                                          plotOutput("Fully_total_vaccin_Plot_saudi")
                                                          
                                                        )),
                                               
                                               tabPanel("Visualize Countries per Total Vaccinations ",
                                                        
                                                        
                                                        
                                                        sidebarPanel(
                                                          sliderInput("total_vac", "Total vaccinations:", 
                                                                      min(0),
                                                                      max(5000000000),
                                                                      value = c(200000000, 1000000000), 
                                                                      step = 1000000
                                                          )
                                                        ),
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        mainPanel(
                                                          #plotOutput("total_vaccin_Plot2")
                                                          
                                                          plotOutput("total_vaccin_Plot")
                                                        )),
                                               
                                               tabPanel("Search Over the Dataset",
                                                        conditionalPanel(
                                                          'input.dataset === "dfallworld"'),
                                                        helpText("You can click the column header to sort a column or search for a country in the search box.")
                                                        
                                                        ,
                                                        
                                                        DT::dataTableOutput("mytable"),
                                                        downloadButton("downloadData", "Download as Excel")
                                               ),
                                               tabPanel("About",
                                                        textOutput("about_output")
                                               )
                                               
                                               
                                               
                                    )
                    )
                    
                    
)


