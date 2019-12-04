full_trains_df <- read.csv("datasets/full_trains.csv")
regularite_df <- read.csv("datasets/regularite-mensuelle-tgv-aqst.csv",sep=";")

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
avg_delay_dep <- full_trains_df %>% group_by(year) %>% summarize(average = mean(num_late_at_departure))
View(avg_delay_dep)
#average number of train rides delayed at arrival
avg_delay_arr <- full_trains_df %>% group_by(year) %>% summarize(average = mean(num_arriving_late, na.rm=TRUE))
View(avg_delay_arr)
#total average departure delay time of all trains by year
total_avg_delay_dep <- full_trains_df %>% group_by(year) %>% summarize(total = sum(avg_delay_all_departing))
View(total_avg_delay_dep)
#total average arrival delay time of all trains by year
total_avg_delay_arr <- full_trains_df %>% group_by(year) %>% summarize(total = sum(avg_delay_all_arriving))
View(total_avg_delay_arr)

#average departure delay time of delayed trains
avg_delay_dep_del <- full_trains_df %>% group_by(year) %>% filter() %>% summarize(average = mean())

#total number of canceled trains by year
total_canceled <- full_trains_df %>% group_by(year) %>% summarize(total = sum(num_of_canceled_trains))
View(total_canceled)