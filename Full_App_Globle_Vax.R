
#modification history:
#updated 3/10/2021
#last updated 9/6/2022 (pie chart was spiting into four slices) (fixed)

library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(rsconnect)
library(reshape2)
library(plotrix)
library(lubridate)
library(shinydashboard)
library(zoo)
library(stringr)
options(scipen = 999)


d <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"))




dfall<- d%>%
    group_by(location,iso_code, total_vaccinations,people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred , date)%>%
    mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
    summarize( Lateset_update = max(date))%>%
    arrange(desc(total_vaccinations))%>%
    filter(!(str_detect(iso_code, "OWID_")))%>%
    select(location , total_vaccinations ,people_fully_vaccinated_per_hundred, people_vaccinated_per_hundred, Lateset_update)
dfallworld <- distinct(dfall, location, .keep_all = TRUE)

dfall_DT<- d%>%
    group_by(location,iso_code, daily_vaccinations,total_vaccinations, people_fully_vaccinated_per_hundred , date,people_vaccinated_per_hundred)%>%
    arrange(desc(total_vaccinations))%>%
    mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
    filter(!(str_detect(iso_code, "OWID_")))%>%
    select(location , date , daily_vaccinations,total_vaccinations,people_fully_vaccinated_per_hundred, people_vaccinated_per_hundred)
dfallworld_DT <- distinct(dfall_DT, location, .keep_all = TRUE)


df_top <- d%>%
    group_by(location,iso_code, daily_vaccinations, date )%>%
    summarize(total_vaccinated_people = max(total_vaccinations))%>%
    mutate(total_vaccinated_people = as.numeric( total_vaccinated_people))%>%
    arrange(desc( total_vaccinated_people))%>%
    mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
    filter(!(str_detect(iso_code, "OWID_")))
df1 <- distinct(df_top, total_vaccinated_people, .keep_all = TRUE)

df_top_saudi <- d%>%
    group_by(location,iso_code,people_vaccinated, daily_vaccinations, date )%>%
    summarize(total_vaccinated_people = max(people_vaccinated))%>%
    mutate(total_vaccinated_people = as.numeric( total_vaccinated_people))%>%
    arrange(desc( total_vaccinated_people))%>%
    mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
    filter(!(str_detect(iso_code, "OWID_")))%>%
    filter(location == "Saudi Arabia")
df_saudi_1 <- distinct(df_top_saudi, total_vaccinated_people, .keep_all = TRUE)

df_top_saudi_2 <- d%>%
    group_by(location,iso_code,  people_fully_vaccinated, date )%>%
    summarize(Fully_total_vaccinated_people = max(people_fully_vaccinated))%>%
    mutate(Fully_total_vaccinated_people = as.numeric( Fully_total_vaccinated_people))%>%
    arrange(desc( Fully_total_vaccinated_people))%>%
    mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
    filter(!(str_detect(iso_code, "OWID_")))%>%
    filter(location == "Saudi Arabia")
df_saudi_2 <- distinct(df_top_saudi_2, Fully_total_vaccinated_people, .keep_all = TRUE)





# piechart_1 <- d %>%    
#     group_by(location, people_vaccinated_per_hundred, date)%>%
#     summarize( lateset_update = max(date), total_vaccinated_people = max(people_vaccinated_per_hundred))%>%
#     arrange(desc(total_vaccinated_people))%>%
#     filter(location == "World")%>%
#     mutate( Curren_no._of_population =  c(100))%>%
#     mutate(Curren_no._of_population = as.numeric(Curren_no._of_population))%>%
#     mutate(total_vac = as.numeric(total_vaccinated_people))%>%
#     group_by( Curren_no._of_population)%>%
#     top_n(1, total_vac )%>%
#     select(date, total_vac )

piechart_1 <- d %>%    
  group_by(location, people_vaccinated_per_hundred, date)%>%
  summarize( lateset_update = max(date), total_vaccinated_people = max(people_vaccinated_per_hundred))%>%
  arrange(desc(total_vaccinated_people))%>%
  filter(location == "World")%>%
  mutate( Curren_no._of_population =  c(100))%>%
  mutate(Curren_no._of_population = as.numeric(Curren_no._of_population))%>%
  mutate(total_vac = as.numeric(total_vaccinated_people))%>%
  group_by( Curren_no._of_population)%>%
  top_n(1, lateset_update )%>%
  select(lateset_update, total_vac )

total_llworld <- d%>%
  group_by( location, people_fully_vaccinated, people_vaccinated, date)%>%
  summarize( Lateset_Update = max(date), Total_Vaccinated_People = max(people_vaccinated))%>%
  arrange(desc(Total_Vaccinated_People))%>%
  filter(location == "World")


info_box_total<- total_llworld%>% group_by(location)%>%
  top_n(1, Total_Vaccinated_People )%>%
  select(location, Total_Vaccinated_People)

info_box_latest_date<- total_llworld%>% group_by( location)%>%
  top_n(1, Total_Vaccinated_People )%>%
  select(date, location, Total_Vaccinated_People)
#drop location
info_box_total1 <- subset(info_box_total, select = -c(location))
#drop location + total
info_box_latest_date2 <- subset(info_box_latest_date, select = -c(location,Total_Vaccinated_People))



Boosters <- d%>%
  group_by( location,  people_fully_vaccinated, date)%>%
  summarize( Lateset_Update = max(date), totalBoost = max(people_fully_vaccinated))%>%
  arrange(desc(totalBoost))%>%
  filter(location == "World")


TotalBoosters<- Boosters%>% group_by(location)%>%
  top_n(1, totalBoost )%>%
  select(location, totalBoost)
#drop location
info_box_booster <- subset(TotalBoosters, select = -c(location))

total_l_SA <- d%>%
  group_by( location,daily_vaccinations, daily_vaccinations_per_million,people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, people_vaccinated, date)%>%
  summarize( Lateset_Update = max(date), Total_Vaccinated_People = max(people_vaccinated_per_hundred))%>%
  arrange(desc(Total_Vaccinated_People))%>%
  filter(location == "Saudi Arabia")

info_box_total_SA<- total_l_SA%>% group_by(location)%>%
  top_n(1, people_vaccinated )%>%
  select(location, people_vaccinated)

info_box_latest_date_SA<- total_l_SA%>% group_by( location)%>%
  top_n(1, Total_Vaccinated_People )%>%
  select(date, location, Total_Vaccinated_People)
#drop location
info_box_total_SA1 <- subset(info_box_total_SA, select = -c(location))
#drop location + total
info_box_latest_date_SA2 <- subset(info_box_latest_date_SA, select = -c(location,Total_Vaccinated_People))


Boosters_SA <- d%>%
  group_by( location,  people_fully_vaccinated, date)%>%
  summarize( Lateset_Update = max(date), totalBoost = max(people_fully_vaccinated))%>%
  arrange(desc(totalBoost))%>%
  filter(location == "Saudi Arabia")


TotalBoosters_SA<- Boosters_SA%>% group_by(location)%>%
  top_n(1, totalBoost )%>%
  select(location, totalBoost)
#drop location
info_box_booster_SA <- subset(TotalBoosters_SA, select = -c(location))

#percentage fo SA
info_box_totalPercent_SA<- total_l_SA%>% group_by(location)%>%
  top_n(1, people_vaccinated_per_hundred )%>%
  select(location, people_vaccinated_per_hundred)

info_box_FullyPercent_SA<- total_l_SA%>% group_by(location)%>%
  top_n(1, people_fully_vaccinated_per_hundred )%>%
  select(location, people_fully_vaccinated_per_hundred)

info_box_DailyVax_SA<- total_l_SA%>% group_by(location)%>%
  top_n(1, daily_vaccinations )%>%
  select(location, daily_vaccinations)


info_box_totalPercent_SA1 <- subset(info_box_totalPercent_SA, select = -c(location))

info_box_FullyPercent_SA1 <- subset(info_box_FullyPercent_SA, select = -c(location))

info_box_DailyVax_SA1 <- subset(info_box_DailyVax_SA, select = -c(location))


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















# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$World  <- renderTable({
        
        
        filter(dfallworld, Lateset_update>= input$world_date[1] & Lateset_update<= input$world_date[2])
    })
    
    #########
    output$TotalvaccinationBox <- renderInfoBox({
        infoBox(
            "Total Vaccinated people",
            info_box_total1,
             #icon = icon("hospital"),
            color =  "green", fill = TRUE
        )
    })
    
    output$LatesetDateBox <- renderInfoBox({
        infoBox("Latest Update ",
          info_box_latest_date2,
             #icon = icon("calendar", lib = "glyphicon"),
            color = "yellow", fill = TRUE)
    })
    
    
    output$TotalBoosterBox <- renderInfoBox({
      infoBox("Fully  vaccinated",
              info_box_booster,
              #icon = icon("refresh", lib = "glyphicon"),
              color = "blue", fill = TRUE)
    })
    #########
    output$TotalvaccinationBox_SA <- renderInfoBox({
      infoBox(
        " Vaccinated people",
        info_box_total_SA1,
        #icon = icon("hospital"),
        color =  "green", fill = TRUE
      )
    })
    
    output$LatesetDateBox_SA <- renderInfoBox({
      infoBox("Latest Update ",
              info_box_latest_date_SA2,
              
              color = "yellow", fill = TRUE)
    })
    
    
    output$TotalBoosterBox_SA <- renderInfoBox({
      infoBox("Fully  vaccinated",
              info_box_booster_SA,
              
              color = "blue", fill = TRUE)
    })
    output$TotalPercentDateBox_SA <- renderInfoBox({
      infoBox("Total Vaccinated %",
              info_box_totalPercent_SA1,
              
              color = "purple", fill = TRUE)
    })
    
    
    
    output$FullyPercentDateBox_SA <- renderInfoBox({
      infoBox("Fully Vaccinated %",
              info_box_FullyPercent_SA1,
              
              color = "purple", fill = TRUE)
    })
    
    
    output$DailyVaxDateBox_SA <- renderInfoBox({
      infoBox("Daily Vaccinations",
              info_box_DailyVax_SA1,
              
              color = "yellow", fill = TRUE)
    })
    
    
    output$mytable <- DT::renderDataTable({
        DT::datatable(dfallworld_DT, options = list(orderClasses = TRUE))
    })
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(dfallworld_DT, file)
        }
    )
    
    output$total_vaccin_Plot <- renderPlot({
        
        
        
        ggplot(filter(df1, total_vaccinated_people>= input$total_vac[1] & total_vaccinated_people <= input$total_vac[2] ))+
            geom_point(aes( date ,total_vaccinated_people, color = location, size =total_vaccinated_people ),stat='identity' , fill="#f68060", alpha=.4, width=.4, group = 1)+
            
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            labs( x = "Date", y = "Number of vaccinated people",
                  title ="Cumulative number of vaccinated people so far",
                  caption = "Source: Updated daily by Our World in Data")+
            theme(plot.title = element_text(hjust = 0.5))+
            theme(legend.position="bottom",legend.box = "vertical",
                  legend.spacing.x = unit(0, 'cm'))+
            guides(fill = guide_legend(label.position = "bottom"))
        
    })
    
    output$graph_allworld <- renderPlot({
        
        calc <- round(piechart_1$total_vac ) 
        lbls <- c("Vaccinated (at least one dose)", "Not yet vaccinated")
        pct <- round(100 - piechart_1$total_vac)
        pct1<- c(calc, pct)
        lbls <- paste(lbls, pct1) 
        lbls <- paste(lbls,"%", sep="")
        pie3D(pct1, labels = lbls,  col=c( "#00CC33", "#999999" ),
              main="Percentage of vaccinated people across the world " )
        
    })
    
 
    
    output$total_vaccin_Plot_saudi <- renderPlot({
        
        ggplot(filter(df_saudi_1, total_vaccinated_people>= input$total_vac_saudi[1] & total_vaccinated_people <= input$total_vac_saudi[2] ))+
            geom_point(aes( date ,total_vaccinated_people, color = location, size =total_vaccinated_people ),stat='identity' , fill="#f68060", alpha=.4, width=.4, group = 1)+
            
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            labs( x = "Date", y = "Number of vaccinated people",
                  title ="Cumulative number of vaccinated people so far (at least one dose)",
                  caption = "Source: Updated daily by Our World in Data")+
            theme(plot.title = element_text(hjust = 0.5))+
            theme(legend.position="bottom",legend.box = "vertical",
                  legend.spacing.x = unit(0, 'cm'))+
            guides(fill = guide_legend(label.position = "bottom"))
        
    })
    
    
    
    output$Fully_total_vaccin_Plot_saudi <- renderPlot({
        
        ggplot(filter(df_saudi_2, Fully_total_vaccinated_people>= input$Fully_vac_saudi[1] & Fully_total_vaccinated_people <= input$Fully_vac_saudi[2] ))+
            geom_point(aes( date ,Fully_total_vaccinated_people, color = location, size =Fully_total_vaccinated_people),stat='identity' , 
                       fill="#f68060", alpha=.4, width=.4, group = 1)+
            
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            labs( x = "Date", y = "Number of vaccinated people",
                  title ="Cumulative number of fully vaccinated people so far",
                  caption = "Source: Updated daily by Our World in Data")+
            theme(plot.title = element_text(hjust = 0.5))+
            theme(legend.position="bottom",legend.box = "vertical",
                  legend.spacing.x = unit(0, 'cm'))+
            guides(fill = guide_legend(label.position = "bottom"))
        
    })


  output$about_output <- renderText({
    
    paste("This application pull the data automatically on daily basis from the Our World in Data", "\n",
          "at https://ourworldindata.org/coronavirus ", "\n",
          "and at https://github.com/owid/covid-19-data."
          
          ,"The author of this web application assumes no responsibility or liability for any errors or omissions in the content of this site. The information contained in this site is provided on an as is basis with no guarantees of completeness, accuracy, usefulness or timeliness."
          ,"(FAYZ ALSHEHRI © 2021)")
  })
  
  
  
}

 
shinyApp(ui = ui, server = server)

