<style>
.background .reveal .state-background{
  background: #E6F6FF
}
</style> 

R Project presentation
========================================================
author: Igor Fidalgo, Arthur Laureau, Ruben Poretz
date: 19/12/2019
autosize: true
font-family: 'Roboto'


Goal of the project
========================================================
type: background
<p class=e> The goal of this project is to construct dashboards displaying information about SNCF rail trafic and US flights. 
This project is the synthesis of every concept reviewed during this Data Analytics course:</p>

- Getting and cleaning data
- Explore Data
- Developp Shiny applications

- To learn more, see [our github](https://github.com/Igzs/R_projects/tree/master/Trains_and_Flights_Shiny_App).  
- You can also test [our application](https://igzs.shinyapps.io/Trains_and_flights/)
- Link to the [presentation](https://rpubs.com/igz/trains_and_flights)

SNCF Dataset
========================================================
type:background
The dataset associated contains aggregated information about rail trafic accross 4 years.At issue is whether the data show signs of improvement or degradation of the state of french rail trafic. 

- We only used the full_trains dataset
- There were 5462 observations accross 59 train stations.

At issue is whether the data show signs of improvement or degradation of the state of french rail trafic. 

Dataset Columns
========================================================
type:background

```
 [1] "year"                        "month"                      
 [3] "service"                     "departure_station"          
 [5] "arrival_station"             "journey_time_avg"           
 [7] "total_num_trips"             "num_of_canceled_trains"     
 [9] "comment_cancellations"       "num_late_at_departure"      
[11] "avg_delay_late_at_departure" "avg_delay_all_departing"    
[13] "comment_delays_at_departure" "num_arriving_late"          
[15] "avg_delay_late_on_arrival"  
```

Some Graphs of the SNCF dashboard
========================================================
class:title
type:background


<img src="presentation-figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="800" height="800" style="display: block; margin: auto auto auto 0;" />
***
<img src="presentation-figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="800" height="800" style="display: block; margin: auto 0 auto auto;" />

US Flights dataset
========================================================
type:background
Three datasets were used to construct this dashboard: 
 - Airlines: Maps IATA flight codes to a specific airline.
 - Airports: Maps IATA flight codes to airports geographical information
 - Flights: Contains an overview of all the information about the flights

The combination of these datasets allows us to have an overview of the state of US air trafic 
Comparisons can be drawn between the two dashboards to reveal interesting results

Canceled and delayed flights per airline
========================================================
type:background


<img src="presentation-figure/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="500" height="500" style="display: block; margin: auto auto auto 0;" />
***
<img src="presentation-figure/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="500" height="500" style="display: block; margin: auto 0 auto auto;" />

Map of airports
========================================================
type:background

<div style="width:1000px;height:1000px;align="middle"">
<img src="map.png"></img>
</div>

Results
========================================================
Trains
* Large increase of canceled and delayed train rides in 2018, may be explained by increased trouble in rolling stocks and traffic, which are the cause of 42% of delays this year.
* Steady number of carried train rides throughout the years
* Large national departure stations such as PARIS EST and MARNE LA VALLEE have the highest number of delayed train rides, but the International lines are most impacted by delay times. 
