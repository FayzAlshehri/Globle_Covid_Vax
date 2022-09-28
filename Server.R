

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