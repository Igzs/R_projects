full_trains_df <- read.csv("datasets/full_trains.csv")
regularite_df <- read.csv("datasets/regularite-mensuelle-tgv-aqst.csv",sep=";")
flights <- read.csv('datasets/flights.csv')
airlines <- read.csv('datasets/airlines.csv')
airports <- read.csv('datasets/airports.csv')
#total number of train rides carried out by year
total_carried <- full_trains_df %>% group_by(year) %>% summarize(total_carried = sum(total_num_trips) - sum(num_of_canceled_trains))
View(total_carried)

#total number of train rides delayed at departure
total_delay_dep <- full_trains_df %>% group_by(year) %>% summarize(total_delay_dep = sum(num_late_at_departure))
View(total_delay_dep)

#total number of train rides delayed at arrival
total_delay_arr <- full_trains_df %>% group_by(year) %>% summarize(total_delay_arr = sum(num_arriving_late, na.rm = TRUE))
View(total_delay_arr)

#average number of train rides delayed at departure
avg_nb_delay_dep <- full_trains_df %>% group_by(year) %>% summarize(departure = mean(num_late_at_departure))
View(avg_nb_delay_dep)

#average number of train rides delayed at arrival
avg_nb_delay_arr <- full_trains_df %>% group_by(year) %>% summarize(arrival = mean(num_arriving_late, na.rm=TRUE))
View(avg_nb_delay_arr)

#total average departure delay time of all trains by year
total_avg_delay_dep <- full_trains_df %>% group_by(year) %>% summarize(total = sum(avg_delay_all_departing))
View(total_avg_delay_dep)

#total average arrival delay time of all trains by year
total_avg_delay_arr <- full_trains_df %>% group_by(year) %>% summarize(total = sum(avg_delay_all_arriving))
View(total_avg_delay_arr)

#average departure delay time of delayed trains
avg_delay_dep_del <- full_trains_df %>% group_by(year) %>% filter(num_late_at_departure>0 & num_arriving_late>0) %>% summarize(average = mean(avg_delay_late_at_departure))
View(avg_delay_dep_del)

#average arrival delay time of delayed trains
avg_delay_arr_del <- full_trains_df %>% group_by(year) %>% filter(num_late_at_departure>0 & num_arriving_late>0) %>% summarize(average = mean(avg_delay_late_on_arrival))
View(avg_delay_arr_del)

#total number of canceled trains by year
total_canceled <- full_trains_df %>% group_by(year) %>% summarize(total_canceled = sum(num_of_canceled_trains))
View(total_canceled)

#percentage of cancelled trains
per_canceled <- full_trains_df %>% 
  group_by(year) %>% 
  summarize(freq = sum(num_of_canceled_trains), total = sum(total_num_trips)) %>% 
  mutate(Canceled = round((freq/total)*100,3)) %>% 
  mutate(Carried=100 - Canceled) %>%
  select(-c(freq,total))
View(per_canceled)
test <- melt(per_canceled,id.vars=c("year"))

ggplot(test, aes(x = "", y = value, fill = variable)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label=value),color='white',position = position_stack(vjust = 0.5)) + 
  coord_polar("y", start = 0)+
  facet_wrap(~ year,  nrow = 2) +
  ggtitle("Percentage of canceled trains by year") +
  theme(plot.title = element_text(color="black", size=24,hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 8, colour = "orange", angle = 90))
#percentage of delay causes
per_causes <- full_trains_df %>% group_by(year) %>% 
          summarize(external = mean(delay_cause_external_cause, na.rm = TRUE), 
                    rail = mean(delay_cause_rail_infrastructure, na.rm = TRUE), 
                    traffic = mean(delay_cause_traffic_management, na.rm = TRUE),
                    stock = mean(delay_cause_rolling_stock, na.rm = TRUE),
                    station = mean(delay_cause_station_management, na.rm = TRUE),
                    travelers = mean(delay_cause_travelers, na.rm = TRUE)
            )

View(per_causes)

rearranged_df <- melt(per_causes, id.vars=c('year'))
rearranged_df["value"] <- mapply(function(x) round(x,4)*100,rearranged_df["value"])

plot <- ggplot(test, aes(x=year,y=value,fill=variable)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(labels = comma) +
  labs(x="Year",y="Total")
plot
input=""
input$choice = "year"
choice = sym(input$choice)
test_df <- full_trains_df %>% group_by(!!(choice)) %>% summarize(total = sum(total_num_trips) - sum(num_of_canceled_trains))
print(test_df)
ggplot(test_df,aes(x=!!(choice),y=total)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#56B4E9")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

ggplot(per_canceled, aes(ymax=percent, ymin=c(0, head(percent, n=-1)), xmax=4, xmin=3, fill=year)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart


d <- melt(per_causes, id.vars=c('year'))

ggplot(d, aes(ymax=value, ymin=c(0, head(value, n=-1)), xmax=4, xmin=3, fill=variable)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) 

melt_avg_delay <- melt(cbind(avg_nb_delay_arr,avg_nb_delay_dep), id.vars = c("year"))

ggplot(melt_avg_delay,aes(x=year,y=value, group=variable, color=variable)) +
  geom_line(size=1.5) +
  labs(x="Year",y="Average number of delayed train rides") +
  ggtitle("Average number of delayed train rides by year")


test <- full_trains_df %>% filter(departure_station=="PARIS EST")
journey_time_delay <- full_trains_df %>% 
  group_by(departure_station) %>% 
  summarize(journey = mean(journey_time_avg), delay = mean(cbind(avg_delay_late_at_departure , avg_delay_late_on_arrival), na.rm=TRUE)) %>%
  mutate(total = journey + delay) %>%
  arrange(desc(delay)) %>%
  top_n(10)

ggplot(journey_time_delay, aes(x=year,y=journey)) +
  geom_bar(stat = "identity",width=0.3) +
  geom_line(aes(x=year,y=delay), size=3, color="dodgerblue")
  
  


