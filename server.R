#server
# Define server logic required to draw a histogram ----
server <- function(input, output) {

  ## Grad Rates Based on Enrollment Public or Private (Reactive)
  output$my_plot=renderPlot({
      my_data <- data[data$Private %in% input$unitype,]
      ggplot(data=my_data, aes(x= my_data$Enroll, y= my_data$Grad.Rate)) + 
      geom_point() + 
      geom_line() + 
      labs(title="Enrollment and Graduation Rate", x= "Enrollment", y= "Grad Rate")
  }) 
  
  ## Data Manipulation
      pie_data= setNames(aggregate(data$PhD, by= list(data$Private), FUN= sum), c("Private", "NumberPhDs"))
      enrollment= setNames(aggregate(data$Enroll, by= list(data$Private), FUN=sum), c("Private", "Enrollment"))
      tmp2= inner_join(pie_data,enrollment, by= "Private")  
      tmp2=tmp2 %>% mutate( total=sum(tmp2$Enrollment)) 
      tmp2 = mutate(tmp2, percent=tmp2$NumberPhDs/tmp2$total)
      mean_top10_Private= mean(College[which(College$Private=="Yes"),]$Top10perc) 
      mean_top10_Public= mean(College[-c(which(College$Private=="Yes")),]$Top10perc)  
      tmp3= data.frame(c(mean_top10_Private, mean_top10_Public))  
      colnames(tmp3)= "Mean Percentage of Top 10" 
      working_data= cbind(tmp2, tmp3)
  
  ## Mean Percent of Top 10 in Private Vs Public (Static)
  output$my_pie_plot=renderPlot({
      ggplot(working_data, aes(x= working_data$Private, y=working_data$`Mean Percentage of Top 10`, fill= Private))+ 
      geom_bar(stat="identity")+
      labs(title= "Mean Percent of Top 10", x= "Private vs. Non-Private", y= "Percent of Population in Top 10 Percent")
    # pie_chart= chart + coord_polar("y",start=0)+
    #   theme(axis.text = element_blank(),
    #     axis.ticks = element_blank(),
    #       panel.grid  = element_blank())
    # pie_chart 
  })  
  
  ## College Size Vs Cost for either Private/Public   
      working_data2= College %>% select(Private, Enroll, Room.Board,Books) %>% mutate(total_cost= Room.Board + Books)  
  output$my_slider_plot= renderPlot({
    working_data2= working_data2[working_data2$Enroll<input$slider & working_data2$Private %in% input$unitype,]
    working_data3= working_data2 %>% mutate(average_total_cost= ceiling((mean(working_data2$total_cost))))
    ggplot(working_data3, aes(x= input$unitype, y=working_data3$average_total_cost, fill=Private))+
    geom_bar(position= "dodge", stat="identity") + 
    labs(title="Average Total Cost (Room + Books)",subtitle="Adjust Slider!", x= "Private University?", y= "Average Total Cost") +
    geom_text(aes(label=  paste("$",working_data3$average_total_cost, "For Institutions with Less than", input$slider, "students")), color="Black", vjust= -.3)  +
     scale_fill_manual(values="cornflowerblue")
  },)
}

  
## Percent of Phds depending on Phds (Static)
  ##chart= ggplot(new_data, aes(x="", y= new_data$percent, fill= Private))+geom_bar(stat="identity")  


