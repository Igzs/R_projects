#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(reshape2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    full_trains_df <- read.csv("datasets/full_trains.csv")
    output$carried_bplot <- renderPlot({
        choice = sym(input$choice)
      
        # generate bins based on input$bins from ui.R
        carried_df <- full_trains_df %>% group_by(!!(choice)) %>% summarize(total = sum(total_num_trips) - sum(num_of_canceled_trains))
        ggplot(carried_df,aes(x=!!(choice),y=total)) +
          geom_bar(stat="identity",width = 0.5,fill="dodgerblue") +
          scale_y_continuous(labels = comma) +
          scale_fill_manual(values=c("#56B4E9")) +
          ggtitle(paste("Total number of carried train rides by",choice)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
                plot.title = element_text(color="black", size=24, face="bold")
                )

    })
    
    output$multiple_bplot <- renderPlot({
      choice = sym(input$choice)
      
      #total number of train rides delayed at departure
  
      total_delay_dep <- full_trains_df %>% group_by(!!(choice)) %>% summarize(total_delay_dep = sum(num_late_at_departure))
      #total number of train rides delayed at arrival
      
      total_delay_arr <- full_trains_df %>% group_by(!!(choice)) %>% summarize(total_delay_arr = sum(num_arriving_late, na.rm = TRUE))
      #total number of canceled trains
    
      total_canceled <- full_trains_df %>% group_by(!!(choice)) %>% summarize(total_canceled = sum(num_of_canceled_trains))
      #combine the three previous dataframes
      combine_df <- melt(cbind(total_canceled,total_delay_dep,total_delay_arr), id.vars = c(choice))
      
    
      ggplot(combine_df, aes(x=!!(choice),y=value,fill=variable)) + 
        geom_bar(stat="identity", width=0.4,position="dodge") +
        scale_y_continuous(labels = comma) +
        labs(x=choice,y="Total") +
        ggtitle(paste("Total disruption by",choice)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
              plot.title = element_text(color="black", size=24, face="bold")
              ) 
      
      
    })

})
