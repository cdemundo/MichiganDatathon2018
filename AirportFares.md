Analysis of Fares and Operational Efficiency
================

We begin by pulling in fare data and specifying major airlines to analyze.

``` r
flight_data <- read.csv('Data/PRIMARY_flight_traffic.csv')
fares <- read.csv('Data/fares.csv')

airline_ids_keep <- c("UA", "AA", "US", "F9", "B6", "OO", "AS", "NK", "WN", "DL", "EV", "HA", "MQ", "VX")

fares <- fares %>%
  filter(airline_id %in% airline_ids_keep)

fares[1, 1:5]
```

    ##   quarter airline_id origin_airport destination_airport distance
    ## 1       1         AA            CLT                 SGF      708

Right now, we don't have actual average fare data for the flight - we have the number of people who paid within a bucket (of size 10). We need to convert this to an average fare paid, per quarter, per route.

``` r
#calculate the price from the bucket data in the fares DB 
price <- seq(from = 15, to = 2505, by = 10) #assume in each bucket, avg fare is middle bucket

#get the total sum spent per bucket
sum_fares_per_bucket <- data.frame(mapply(`*`,fares[, 6:255],price))

#sum all the bucket fares and divide by the total number of people who bought tickets
totalFaresPerFlight <- rowSums(sum_fares_per_bucket)
totalPeoplePerFlight <- rowSums(fares[, 6:255])

fares$avg_fare <-  totalFaresPerFlight/totalPeoplePerFlight

select(fares, c("airline_id", "origin_airport", "destination_airport", "quarter", "avg_fare"))[1:5,]
```

    ##   airline_id origin_airport destination_airport quarter  avg_fare
    ## 1         AA            CLT                 SGF       1 166.22750
    ## 2         DL            DTW                 SAN       1 471.55466
    ## 3         DL            MSP                 ISN       1 130.69024
    ## 4         AA            CLT                 RDU       1  32.28124
    ## 5         DL            ATL                 RIC       1 114.96981

Now that we have the average fare in USD paid per route in each quarter, we can aggregate this across quarters and split the data to look at fares by airline and route.

``` r
#so i can join back in when I group by airline
route_distances <- fares %>%
  mutate(route = paste(origin_airport, destination_airport)) %>%
  select(c("route", "distance", "origin_airport", "destination_airport"))

#summarise the average fare per airline per route
route_fares <- fares %>%
  mutate(route = paste(origin_airport, destination_airport)) %>%
  group_by(airline_id, route, origin_airport, distance) %>%
  summarise(avg_fare = mean(avg_fare))

route_fares[1:5,]
```

    ## # A tibble: 5 x 5
    ## # Groups:   airline_id, route, origin_airport [5]
    ##   airline_id   route origin_airport distance  avg_fare
    ##       <fctr>   <chr>         <fctr>    <int>     <dbl>
    ## 1         AA ABE CLE            ABE      339  63.33333
    ## 2         AA ABE CLT            ABE      481 123.32242
    ## 3         AA ABE PHL            ABE       55  22.21275
    ## 4         AA ABI DFW            ABI      158  40.20488
    ## 5         AA ABQ ATL            ABQ     1269 115.00000

The question we want to examine is: Holding the airline and route distance constant, do higher delays caused at airports correlate with lower fares at that airport?

``` r
#dataset created separately to group airlines with delays
delays <- read.csv('Data/delays.csv')

#Join route delays to fare information
route_fares_delays <- inner_join(route_fares, delays, by = 'origin_airport') %>%
  mutate(route_dist = cut(distance, breaks=seq(0,1500, by=100)))
```

    ## Warning: Column `origin_airport` joining factors with different levels,
    ## coercing to character vector

``` r
#Filter to a specific airline for analysis
airline_filter <- "DL"

route_dist_by_airline <- route_fares_delays %>%
  group_by(airline_id, route_dist, origin_airport) %>%
  summarize(avg_fare_airport = mean(avg_fare), avg_delay_airport = mean(Avg..Total.Departure.Delay)) %>%
  filter(airline_id == "DL")

#hold distance constant, plot a scatterplot for each distance segment
unique_route <- unique(route_dist_by_airline$route_dist)

i <- 0 #limit for example purposes
for(route in unique_route)
{
  if(i<2)
  {
    selected_route <- filter(route_dist_by_airline, route_dist == route)
      
    plot(selected_route$avg_delay_airport, selected_route$avg_fare_airport, sub = paste("Flight Distance Between: ", route), main = paste(airline_filter, "Flights"), xlab="Delay (minutes)", ylab="Fare (USD $)")
        
    abline(lm(selected_route$avg_fare_airport ~ selected_route$avg_delay_airport))
  }
  i <- i+1
}
```

![](AirportFares_files/figure-markdown_github/unnamed-chunk-4-1.png)![](AirportFares_files/figure-markdown_github/unnamed-chunk-4-2.png)

Looking at the scatterplots, we have identified no clear trend between fare prices and the delays inherent at different airports. Given this, we look to explore if operational efficiency within airlines affects the fares they are able to charge.

``` r
flight_data[is.na(flight_data)] <- 0
flight_data <- flight_data %>%
  filter(flight_data$airline_id %in% airline_ids_keep)

#Get the average delay for each domestic route in the US
airline_delay_by_airport <- flight_data %>%
  mutate(route = paste(origin_airport, destination_airport)) %>%
  group_by(airline_id, route) %>%
  summarize(delay_minutes = mean(airline_delay))

airline_delay_by_airport[1:2, ]
```

    ## # A tibble: 2 x 3
    ## # Groups:   airline_id [1]
    ##   airline_id   route delay_minutes
    ##       <fctr>   <chr>         <dbl>
    ## 1         AA ABQ DFW     0.7166667
    ## 2         AA ABQ ORD     0.0000000

``` r
#Join delay information with fare price information
fares_airline_caused_delays <- inner_join(airline_delay_by_airport, route_fares, by = c("airline_id", "route"))
```

    ## Warning: Column `airline_id` joining factors with different levels,
    ## coercing to character vector

``` r
fares_airline_caused_delays[1,]
```

    ## # A tibble: 1 x 6
    ## # Groups:   airline_id [1]
    ##   airline_id   route delay_minutes origin_airport distance avg_fare
    ##        <chr>   <chr>         <dbl>         <fctr>    <int>    <dbl>
    ## 1         AA ABQ DFW     0.7166667            ABQ      569 148.0924

``` r
#Limit delays to 15 minutes as significant outliers skew the results
fares_airline_caused_delays <- fares_airline_caused_delays[!is.null(fares_airline_caused_delays)] %>%
  filter(delay_minutes < 15) #outliers

unique_airlines <- unique(fares_airline_caused_delays$airline_id)

#For each airline, produce a scatterplot relating fare prices to airline delays
i <- 0
for(airline in unique_airlines)
{
  if(i<2)
  {
    selected_airline <- filter(fares_airline_caused_delays, airline_id == airline)
    
    plot(selected_airline$delay_minutes, selected_airline$avg_fare, xlab="Delay", ylab="Fare", sub=airline)
    
    abline(lm(selected_airline$avg_fare ~ selected_airline$delay_minutes))
    
    dev.copy(png,filename=paste(airline, ".png"))
    dev.off ()
  }
  i <- i+1
}
```

![](AirportFares_files/figure-markdown_github/unnamed-chunk-7-1.png)![](AirportFares_files/figure-markdown_github/unnamed-chunk-7-2.png)
