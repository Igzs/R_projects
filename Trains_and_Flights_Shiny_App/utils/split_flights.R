sample_size = 1000000
flights <- read.csv("datasets/flights.csv",stringsAsFactors = FALSE)
flights.sample <- flights %>% sample_n(sample_size)

write.csv(flights.sample, file ="datasets/flights_sample.csv",na = "",
          row.names = FALSE)