ui = fluidPage(
  titlePanel("diive dashboard"),  
  #this is for your selections
  sidebarLayout(
    mainPanel(
      splitLayout(cellWidths=c("25%", "25%", "50%"),
        plotOutput(outputId= "my_plot"), 
        plotOutput(outputId= "my_pie_plot"), 
        plotOutput(outputId= "my_slider_plot") 
      )
    ),
    
    sidebarPanel (
      selectInput("unitype", "Select Public/Private University:", choices=unique(data$Private), selected= "NULL",
      ), 
      sliderInput("slider", "Enrollment:", min=min(working_data2$Enroll), max= max(working_data2$Enroll), value= as.integer(mean(College$Enroll)))
    )
  )
)
