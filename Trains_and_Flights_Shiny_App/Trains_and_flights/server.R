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
library(reshape2)
library(shinyjs)


#function to accelerate the process of loading large datasets
#returns the correct colnames for the flights csv files
accelerate_csv <- function(path) {
  #sample some rows from large dataset
  flights.sample <- read.csv(path,  
                             stringsAsFactors=FALSE, header=T, nrows=20)  
  
  flights.colclass <- sapply(flights.sample,class)
  
  flights.colclass["CANCELLATION_REASON"] <- "character"
  flights.colclass["AIR_SYSTEM_DELAY"] <- "integer"
  flights.colclass["SECURITY_DELAY"] <- "integer"
  flights.colclass["AIRLINE_DELAY"] <- "integer"
  flights.colclass["LATE_AIRCRAFT_DELAY"] <- "integer"
  flights.colclass["WEATHER_DELAY"] <- "integer"
  return(flights.colclass)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  colors = c('#D4706A','#7890C2')
  full_trains_df <- read.csv("datasets/full_trains.csv")
  airports <- read.csv("datasets/airports.csv")
  
  flights.colclass <- accelerate_csv("datasets/flights.csv")
  flights <- read.csv("datasets/flights.csv", header = TRUE, sep = ",",  stringsAsFactors = FALSE, nrow = 1000000, colClasses = flights.colclass)
  
  output$select_departure_ui <- renderUI({
    selectInput("station", "Departure station : ",  choices=unique(full_trains_df["departure_station"]), 
                selected="")
  })
  
  
  #disable departure station select input based on checkbox input
  observeEvent(input$is_departure, {
    if(input$is_departure){
      shinyjs::enable("select_departure_ui")
    }else{
      shinyjs::disable("select_departure_ui")
    }
  })
  
  
  #disable departure station checkbox  based on choice input
  observeEvent(input$choice, {
    if(input$choice=="departure_station"){
      
      updateCheckboxInput(
        session =session,
        inputId =  "is_departure", 
        value = FALSE
      )
      shinyjs::disable("is_departure")
      shinyjs::disable("select_departure_ui")
    }else{
      shinyjs::enable("is_departure")
    }
  })
  
  
  
  output$carried_bplot <- renderPlot({
    choice = sym(input$choice)
    
    # generate bins based on input$bins from ui.R
    if(input$is_departure){
      carried_df <- full_trains_df %>% group_by(!!(choice)) %>% filter(departure_station==input$station) %>%summarize(total = sum(total_num_trips) - sum(num_of_canceled_trains))
      
    }else{
      carried_df <- full_trains_df %>% group_by(!!(choice)) %>% summarize(total = sum(total_num_trips) - sum(num_of_canceled_trains))
    }
    ggplot(carried_df,aes(x=!!(choice),y=total),fill=!!(choice)) +
      geom_bar(stat="identity",width = 0.3,fill=colors[2]) +
      scale_y_continuous(labels = comma) +
      ggtitle(paste("Total number of carried train rides by",choice)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(color="black", size=24,hjust = 0.5)
      )
    
  })
  
  output$canceled_bplot <- renderPlot({
    #total number of canceled trains
    choice = sym(input$choice)
    
    if(input$is_departure){
      total_canceled <- full_trains_df %>% group_by(!!(choice)) %>% filter(departure_station==input$station) %>% summarize(total_canceled = sum(num_of_canceled_trains))
    }else{
      total_canceled <- full_trains_df %>% group_by(!!(choice)) %>% summarize(total_canceled = sum(num_of_canceled_trains))
    }
    
    
    ggplot(total_canceled, aes(x=!!(choice),y=total_canceled)) + 
      geom_bar(stat="identity", width=0.3, fill = colors[1]) +
      scale_y_continuous(labels = comma) +
      ggtitle(paste("Total number of canceled train rides by",choice)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(color="black", size=24,hjust = 0.5)
      )
  })
  
  output$delay_bplot <- renderPlot({
    choice = sym(input$choice)
    
    #total number of train rides delayed at departure
    if(input$is_departure){
      total_delay_dep <- full_trains_df %>% group_by(!!(choice)) %>% filter(departure_station==input$station) %>% summarize(Departure = sum(num_late_at_departure))
      #total number of train rides delayed at arrival
      
      total_delay_arr <- full_trains_df %>% group_by(!!(choice)) %>% filter(departure_station==input$station) %>% summarize(Arrival = sum(num_arriving_late, na.rm = TRUE))
    }
    else{
      total_delay_dep <- full_trains_df %>% group_by(!!(choice)) %>% summarize(Departure = sum(num_late_at_departure))
      #total number of train rides delayed at arrival
      
      total_delay_arr <- full_trains_df %>% group_by(!!(choice)) %>% summarize(Arrival = sum(num_arriving_late, na.rm = TRUE))
    }
    
    
    #combine the two previous dataframes
    combine_df <- melt(cbind(total_delay_dep,total_delay_arr), id.vars = c(choice))
    
    
    ggplot(combine_df, aes(x=!!(choice),y=value,fill=variable)) + 
      geom_bar(stat="identity", width=0.5,position="dodge") +
      scale_fill_manual(values=colors) +
      scale_y_continuous(labels = comma) +
      ggtitle(paste("Total number of delayed train rides by",choice)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(color="black", size=24,hjust = 0.5),
            legend.title = element_blank()
      )
  })
  
  output$total_time_bplot <- renderPlot({
    choice = sym(input$choice)
    
    #total number of train rides delayed at departure
    if(input$is_departure){
      #total average departure delay time of all trains
      
      total_avg_delay_dep <- full_trains_df %>% group_by(!!(choice)) %>% filter(departure_station == input$station) %>% summarize(Departure = sum(avg_delay_all_departing))
      
      #total average arrival delay time of all trains
      total_avg_delay_arr <- full_trains_df %>% group_by(!!(choice)) %>% filter(departure_station == input$station) %>% summarize(Arrival = sum(avg_delay_all_arriving))
    }
    else{
      #total average departure delay time of all trains
      
      total_avg_delay_dep <- full_trains_df %>% group_by(!!(choice)) %>% summarize(Departure = sum(avg_delay_all_departing))
      
      #total average arrival delay time of all trains
      total_avg_delay_arr <- full_trains_df %>% group_by(!!(choice)) %>% summarize(Arrival = sum(avg_delay_all_arriving))
    }
    
    #combine the two previous dataframes
    combine_df <- melt(cbind(total_avg_delay_dep,total_avg_delay_arr), id.vars = c(choice))
    
    
    ggplot(combine_df, aes(x=!!(choice),y=value,fill=variable)) + 
      geom_bar(stat="identity", width=0.5,position="dodge") +
      scale_fill_manual(values=colors) +
      scale_y_continuous(labels = comma) +
      labs(y="Time (min)") +
      ggtitle(paste("Total average delay time of all delayed train rides by",choice)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(color="black", size=24,hjust = 0.5),
            legend.title = element_blank()
      )
    
  })
  
  output$avg_time_lplot <- renderPlot({
    choice = sym(input$choice)
    if(input$is_departure){
      #average departure delay time of delayed trains
      avg_delay_dep_del <- full_trains_df %>% 
        group_by(!!(choice)) %>% 
        filter(num_late_at_departure>0 & num_arriving_late>0 & departure_station==input$station) %>% 
        summarize(Departure = mean(avg_delay_late_at_departure))
      
      #average arrival delay time of delayed trains
      avg_delay_arr_del <- full_trains_df %>% 
        group_by(!!(choice)) %>% 
        filter(num_late_at_departure>0 & num_arriving_late>0 & departure_station==input$station) %>% 
        summarize(Arrival = mean(avg_delay_late_on_arrival))
      
    }else{
      #average departure delay time of delayed trains
      avg_delay_dep_del <- full_trains_df %>% 
        group_by(!!(choice)) %>% 
        filter(num_late_at_departure>0 & num_arriving_late>0) %>% 
        summarize(Departure = mean(avg_delay_late_at_departure))
      
      #average arrival delay time of delayed trains
      avg_delay_arr_del <- full_trains_df %>% 
        group_by(!!(choice)) %>% 
        filter(num_late_at_departure>0 & num_arriving_late>0) %>% 
        summarize(Arrival = mean(avg_delay_late_on_arrival))
      
    }
    
    melt_avg_time<- melt(cbind(avg_delay_dep_del,avg_delay_arr_del), id.vars = c(choice))
    
    
    if(choice=="departure_station"){
      ggplot(melt_avg_time, aes(x=!!(choice),y=value,fill=variable)) + 
        geom_bar(stat="identity", width=0.5,position="dodge") +
        scale_fill_manual(values=colors) +
        scale_y_continuous(labels = comma) +
        labs(y="Time (min)") +
        ggtitle(paste("Average time of delayed train rides by",choice)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
              axis.title.x=element_blank(),
              plot.title = element_text(color="black", size=24,hjust = 0.5),
              legend.title = element_blank()
        )
      
    }
    else{
      
      ggplot(melt_avg_time,aes(x=!!(choice),y=value, group=variable, color=variable)) +
        geom_line(size=1.5) +
        scale_color_manual(values=colors) +
        labs(x="Year", y="Time (min)") +
        ggtitle("Average time of delayed train rides by year") +
        theme(plot.title = element_text(color="black", size=24,hjust = 0.5),
              legend.title = element_blank())
    }
  })
  
  output$avg_delay_bplot <- renderPlot({
    choice = sym(input$choice)
    
    if(input$is_departure){
      #average number of train rides delayed at departure
      
      avg_nb_delay_dep <- full_trains_df %>% group_by(!!(choice)) %>% filter(departure_station==input$station) %>% summarize(Departure = mean(num_late_at_departure))
      #average number of train rides delayed at arrival
      
      avg_nb_delay_arr <- full_trains_df %>% group_by(!!(choice)) %>% filter(departure_station==input$station) %>% summarize(Arrival = mean(num_arriving_late, na.rm=TRUE))
      
    }else{
      #average number of train rides delayed at departure
      
      avg_nb_delay_dep <- full_trains_df %>% group_by(!!(choice)) %>% summarize(Departure = mean(num_late_at_departure))
      #average number of train rides delayed at arrival
      
      avg_nb_delay_arr <- full_trains_df %>% group_by(!!(choice)) %>% summarize(Arrival = mean(num_arriving_late, na.rm=TRUE))
      
    }
    
    #combine the two dataframes
    
    melt_avg_delay <- melt(cbind(avg_nb_delay_dep,avg_nb_delay_arr), id.vars = c(choice))
    
    ggplot(melt_avg_delay, aes(x=!!(choice),y=value,fill=variable)) + 
      geom_bar(stat="identity", width=0.5,position="dodge") +
      scale_fill_manual(values=colors) +
      scale_y_continuous(labels = comma) +
      ggtitle(paste("Average number of delayed train rides by",choice)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(color="black", size=24,hjust = 0.5),
            legend.title = element_blank()
      )
    
    
  })
  
  output$journey_time_delay_plot <- renderPlot({
    
    journey_time_delay <- full_trains_df %>% 
      group_by(departure_station) %>% 
      summarize(journey = mean(journey_time_avg), delay = mean(cbind(avg_delay_late_at_departure , avg_delay_late_on_arrival), na.rm=TRUE)) %>%
      mutate(total = journey + delay) %>%
      arrange(desc(delay)) %>%
      top_n(10)
    
    ggplot(journey_time_delay, aes(x=reorder(departure_station,desc(delay)),y=delay)) +
      geom_bar(stat = "identity",width=0.3, fill=colors[2]) +
      labs(y="Average delay time (min)") +
      ggtitle("Top 10 most delayed departure stations") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
            axis.title.x=element_blank(),
            plot.title = element_text(color="black", size=24,hjust = 0.5)
      )
  })
  
  output$per_canceled_plot<- renderPlot({
    choice <- sym(input$choice)
    if(input$is_departure){
      #percentage of cancelled trains
      per_canceled <- full_trains_df %>% 
        group_by(!!(choice)) %>% 
        filter(departure_station==input$station) %>%
        summarize(freq = sum(num_of_canceled_trains), total = sum(total_num_trips)) %>% 
        mutate(Canceled = round((freq/total)*100,3)) %>% 
        mutate(Carried=100 - Canceled) %>%
        select(-c(freq,total))
      
    }else{
      #percentage of cancelled trains
      per_canceled <- full_trains_df %>% 
        group_by(!!(choice)) %>% 
        summarize(freq = sum(num_of_canceled_trains), total = sum(total_num_trips)) %>% 
        mutate(Canceled = round((freq/total)*100,3)) %>% 
        mutate(Carried=100 - Canceled) %>%
        select(-c(freq,total))      
    }
    
    combined_df <- melt(per_canceled,id.vars=c(choice))
    
    if(choice=="year"){
      ggplot(combined_df, aes(x = "", y = value, fill = variable)) +
        geom_bar(width = 1, stat = "identity") +
        geom_text(aes(label=value),color='white',position = position_stack(vjust = 0.5)) + 
        scale_fill_manual(values=colors) +
        coord_polar("y", start = 0)+
        facet_grid(.~year) +
        ggtitle("Percentage of canceled trains by year") +
        theme(plot.title = element_text(color="black", size=24,hjust = 0.5),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.title = element_blank())
    }else{
      ggplot(combined_df, aes(x=!!(choice),y=value,fill=variable)) + 
        geom_bar(stat="identity", width=0.5,position="stack") +
        scale_fill_manual(values=colors) +
        scale_y_continuous(labels = comma) +
        ggtitle(paste("Percentage of canceled trains by departure station")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
              axis.title.y=element_blank(),
              plot.title = element_text(color="black", size=24,hjust = 0.5),
              legend.title = element_blank()
        )
    }
    
    
    
  })
  
  output$per_causes_bplot <- renderPlot({
    choice <- sym(input$choice)
    
    if(input$is_departure){
      #percentage of causes
      per_causes <- full_trains_df %>% group_by(!!(choice)) %>% 
        filter(departure_station == input$station) %>%
        summarize(external = mean(delay_cause_external_cause, na.rm = TRUE), 
                  rail = mean(delay_cause_rail_infrastructure, na.rm = TRUE), 
                  traffic = mean(delay_cause_traffic_management, na.rm = TRUE),
                  stock = mean(delay_cause_rolling_stock, na.rm = TRUE),
                  station = mean(delay_cause_station_management, na.rm = TRUE),
                  travelers = mean(delay_cause_travelers, na.rm = TRUE)
        )      }else{
          #percentage of causes
          per_causes <- full_trains_df %>% group_by(!!(choice)) %>% 
            summarize(external = mean(delay_cause_external_cause, na.rm = TRUE), 
                      rail = mean(delay_cause_rail_infrastructure, na.rm = TRUE), 
                      traffic = mean(delay_cause_traffic_management, na.rm = TRUE),
                      stock = mean(delay_cause_rolling_stock, na.rm = TRUE),
                      station = mean(delay_cause_station_management, na.rm = TRUE),
                      travelers = mean(delay_cause_travelers, na.rm = TRUE)
            )
        }
    rearranged_df <- melt(per_causes, id.vars=c(choice))
    rearranged_df["value"] <- mapply(function(x) round(x,4)*100,rearranged_df["value"])
    
    
    ggplot(rearranged_df, aes(x=!!(choice),y=value,fill=variable)) + 
      geom_bar(stat="identity", width=0.5,position="stack") +
      geom_text(aes(label = value), color="white", position = position_stack(vjust = 0.5), size = 4) +
      scale_y_continuous(labels = comma) +
      ggtitle(paste("Percentage of delay causes by",choice)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
            axis.title.y=element_blank(),
            plot.title = element_text(color="black", size=24,hjust = 0.5)
      )
    
    
  })
  
  # FLIGHTS
  
  output$nb_flights_plot <- renderPlot({
    flights_choice = sym(input$flights_choice)
    # generate bins based on input$bins from ui.R
    if (input$flights_choice == "AIRLINE") {
      total_flights <-
        flights %>% group_by(AIRLINE) %>% summarise(total = n() - sum(CANCELLED))
    }
    else{
      total_flights <-
        flights %>% group_by(ORIGIN_AIRPORT) %>% summarise(total = n() - sum(CANCELLED))
    }
    ggplot(total_flights, aes(x = !!(flights_choice), y = total)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "dodgerblue") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#56B4E9")) +
      ggtitle(paste("Total number of flights cancelled by", input$flights_choice)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          color = "black",
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
  output$del_flights_plot <- renderPlot({
    flights_choice = sym(input$flights_choice)
    # generate bins based on input$bins from ui.R
    if (input$flights_choice == "AIRLINE") {
      total_delayed <-
        flights %>% group_by(AIRLINE) %>% filter(DEPARTURE_DELAY != 0 ||
                                                   ARRIVAL_DELAY != 0) %>% summarize(Total_delayed = n())
    }
    else{
      total_delayed <-
        flights %>% group_by(ORIGIN_AIRPORT) %>% filter(DEPARTURE_DELAY != 0 ||
                                                          ARRIVAL_DELAY != 0) %>% summarize(Total_delayed = n())
    }
    ggplot(total_delayed, aes(x = !!(flights_choice), y = Total_delayed)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "dodgerblue") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#56B4E9")) +
      ggtitle(paste("Total number of flights delayed by", input$flights_choice)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          color = "black",
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
  output$avg_dur_flights_plot <- renderPlot({
    flights_choice = sym(input$flights_choice)
    # generate bins based on input$bins from ui.R
    if (input$flights_choice == "AIRLINE") {
      avg_duration <-
        flights %>% group_by(AIRLINE) %>% summarise(Average_duration = mean(ELAPSED_TIME, na.rm = TRUE))
    }
    else{
      avg_duration <-
        flights %>% group_by(ORIGIN_AIRPORT) %>% summarise(Average_duration = mean(ELAPSED_TIME, na.rm = TRUE))
    }
    ggplot(avg_duration, aes(x = !!(flights_choice), y = Average_duration)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "dodgerblue") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#56B4E9")) +
      ggtitle(paste("Average flight duration by", input$flights_choice)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          color = "black",
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
  output$avg_dist_flights_plot <- renderPlot({
    flights_choice = sym(input$flights_choice)
    # generate bins based on input$bins from ui.R
    if (input$flights_choice == "AIRLINE") {
      avg_distance <-
        flights %>% group_by(AIRLINE) %>% summarise(Average_distance = mean(DISTANCE, na.rm = TRUE))
    }
    else{
      avg_distance <-
        flights %>% group_by(ORIGIN_AIRPORT) %>% summarise(Average_distance = mean(DISTANCE, na.rm = TRUE))
    }
    ggplot(avg_distance, aes(x = !!(flights_choice), y = Average_distance)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "dodgerblue") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#56B4E9")) +
      ggtitle(paste("Average flight distance by", input$flights_choice)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          color = "black",
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
  output$avg_dur_flights_plot <- renderPlot({
    flights_choice = sym(input$flights_choice)
    # generate bins based on input$bins from ui.R
    if (input$flights_choice == "AIRLINE") {
      avg_duration <-
        flights %>% group_by(AIRLINE) %>% summarise(Average_duration = mean(ELAPSED_TIME, na.rm = TRUE))
    }
    else{
      avg_duration <-
        flights %>% group_by(ORIGIN_AIRPORT) %>% summarise(Average_duration = mean(ELAPSED_TIME, na.rm = TRUE))
    }
    ggplot(avg_duration, aes(x = !!(flights_choice), y = Average_duration)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "dodgerblue") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#56B4E9")) +
      ggtitle(paste("Average flight duration by", input$flights_choice)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          color = "black",
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
  output$tot_dist_flights_plot <- renderPlot({
    flights_choice = sym(input$flights_choice)
    # generate bins based on input$bins from ui.R
    if (input$flights_choice == "AIRLINE") {
      total_distance <-
        flights %>% group_by(AIRLINE) %>% summarise(Total_distance = sum(DISTANCE))
    }
    else{
      total_distance <-
        flights %>% group_by(ORIGIN_AIRPORT) %>% summarise(Total_distance = sum(DISTANCE))
    }
    ggplot(total_distance, aes(x = !!(flights_choice), y = Total_distance)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "dodgerblue") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#56B4E9")) +
      ggtitle(paste("Total flight distance by", input$flights_choice)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          color = "black",
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
  output$avg_dep_del_flights_plot <- renderPlot({
    flights_choice = sym(input$flights_choice)
    # generate bins based on input$bins from ui.R
    if (input$flights_choice == "AIRLINE") {
      avg_dep_delay <-
        flights %>% group_by(AIRLINE) %>% summarise(Average_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE))
    }
    else{
      avg_dep_delay <-
        flights %>% group_by(ORIGIN_AIRPORT) %>% summarise(Average_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE))
    }
    ggplot(avg_dep_delay, aes(x = !!(flights_choice), y = Average_departure_delay)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "dodgerblue") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#56B4E9")) +
      ggtitle(paste("Average flight departure delay by", input$flights_choice)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          color = "black",
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
  output$del_ar_del_flights_plot <- renderPlot({
    flights_choice = sym(input$flights_choice)
    # generate bins based on input$bins from ui.R
    if (input$flights_choice == "AIRLINE") {
      avg_arr_delay <-
        flights %>% group_by(AIRLINE) %>% summarise(Average_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE))
    }
    else{
      avg_arr_delay <-
        flights %>% group_by(ORIGIN_AIRPORT) %>% summarise(Average_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE))
    }
    ggplot(avg_arr_delay, aes(x = !!(flights_choice), y = Average_arrival_delay)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "dodgerblue") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#56B4E9")) +
      ggtitle(paste("Average flight arrival delay by", input$flights_choice)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          color = "black",
          size = 24,
          hjust = 0.5
        )
      )
    
  })
  
  output$airports_map <- renderLeaflet({
    map <- leaflet(data = airports) %>%
      addProviderTiles("CartoDB.Positron", group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
      addMarkers( ~ LONGITUDE,
                  ~ LATITUDE,
                  label = ~ AIRPORT,
                  group = "airports") %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Map", "Satellite", "Relief"),
        overlayGroups = c("airports", "flights"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  
})