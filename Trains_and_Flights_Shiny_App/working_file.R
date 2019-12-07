full_trains_df <- read.csv("datasets/full_trains.csv")
regularite_df <- read.csv("datasets/regularite-mensuelle-tgv-aqst.csv",sep=";")
flights <- read.csv('datasets/flights.csv')
airlines <- read.csv('datasets/airlines.csv')
airports <- read.csv('datasets/airports.csv')
#total number of train rides carried out by year
total_carried <- full_trains_df %>% group_by(year) %>% summarize(total = sum(total_num_trips) - sum(num_of_canceled_trains))
View(total_carried)

#total number of train rides delayed at departure
total_delay_dep <- full_trains_df %>% group_by(year) %>% summarize(total = sum(num_late_at_departure))
View(total_delay_dep)

#total number of train rides delayed at arrival
total_delay_arr <- full_trains_df %>% group_by(year) %>% summarize(total = sum(num_arriving_late, na.rm = TRUE))
View(total_delay_arr)

#average number of train rides delayed at departure
avg_nb_delay_dep <- full_trains_df %>% group_by(year) %>% summarize(average = mean(num_late_at_departure))
View(avg_nb_delay_dep)

#average number of train rides delayed at arrival
avg_nb_delay_arr <- full_trains_df %>% group_by(year) %>% summarize(average = mean(num_arriving_late, na.rm=TRUE))
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
View(avg_delay_dep_del)

#total number of canceled trains by year
total_canceled <- full_trains_df %>% group_by(year) %>% summarize(total = sum(num_of_canceled_trains))
View(total_canceled)

#percentage of cancelled trains
per_canceled <- full_trains_df %>% group_by(year) %>% summarize(freq = sum(num_of_canceled_trains), total = sum(total_num_trips)) %>% mutate(percent = freq/total)
View(per_canceled)

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