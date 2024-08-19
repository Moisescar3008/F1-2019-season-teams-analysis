Mercedes 2019
================
Moises Carrillo
2024-08-17

## Introduction

This report analyzes the performance of the Mercedes team in the 2019
Formula 1 season. We examine aspects such as final positions, points
earned, poles, victories, tracks and fastest laps, as well as the
pole-to-victory conversion rate.

# Data loading and preparation

In this part we load the data and we do some cleaning of the database

``` r
f1_2019_results <- read.csv('C:/Users/moise/Analysis_project/formula1_2019season_raceResults.csv')

mercedes_data <- f1_2019_results %>%
  filter(Team == "Mercedes", ignore.case = TRUE) %>% 
  select(Driver, Position, Points, Track, Starting.Grid, Fastest.Lap)

mercedes_data$Position <- as.numeric(mercedes_data$Position)
```

    ## Warning: NAs introduced by coercion

``` r
mercedes_data$Track <- factor(mercedes_data$Track, levels = unique(f1_2019_results$Track))
mercedes_data$Position[22] <- 15
mercedes_data$Position[40] <- 20

mercedes_data
```

    ##             Driver Position Points         Track Starting.Grid Fastest.Lap
    ## 1  Valtteri Bottas        1     26     Australia             2         Yes
    ## 2   Lewis Hamilton        2     18     Australia             1          No
    ## 3   Lewis Hamilton        1     25       Bahrain             3          No
    ## 4  Valtteri Bottas        2     18       Bahrain             4          No
    ## 5   Lewis Hamilton        1     25         China             2          No
    ## 6  Valtteri Bottas        2     18         China             1          No
    ## 7  Valtteri Bottas        1     25    Azerbaijan             1          No
    ## 8   Lewis Hamilton        2     18    Azerbaijan             2          No
    ## 9   Lewis Hamilton        1     26         Spain             2         Yes
    ## 10 Valtteri Bottas        2     18         Spain             1          No
    ## 11  Lewis Hamilton        1     25        Monaco             1          No
    ## 12 Valtteri Bottas        3     15        Monaco             2          No
    ## 13  Lewis Hamilton        1     25        Canada             2          No
    ## 14 Valtteri Bottas        4     13        Canada             6         Yes
    ## 15  Lewis Hamilton        1     25        France             1          No
    ## 16 Valtteri Bottas        2     18        France             2          No
    ## 17 Valtteri Bottas        3     15       Austria             3          No
    ## 18  Lewis Hamilton        5     10       Austria             4          No
    ## 19  Lewis Hamilton        1     26 Great Britain             2         Yes
    ## 20 Valtteri Bottas        2     18 Great Britain             1          No
    ## 21  Lewis Hamilton        9      2       Germany             1          No
    ## 22 Valtteri Bottas       15      0       Germany             3          No
    ## 23  Lewis Hamilton        1     25       Hungary             3          No
    ## 24 Valtteri Bottas        8      4       Hungary             2          No
    ## 25  Lewis Hamilton        2     18       Belgium             3          No
    ## 26 Valtteri Bottas        3     15       Belgium             4          No
    ## 27 Valtteri Bottas        2     18         Italy             3          No
    ## 28  Lewis Hamilton        3     16         Italy             2         Yes
    ## 29  Lewis Hamilton        4     12     Singapore             2          No
    ## 30 Valtteri Bottas        5     10     Singapore             5          No
    ## 31  Lewis Hamilton        1     26        Russia             2         Yes
    ## 32 Valtteri Bottas        2     18        Russia             4          No
    ## 33 Valtteri Bottas        1     25         Japan             3          No
    ## 34  Lewis Hamilton        3     16         Japan             4         Yes
    ## 35  Lewis Hamilton        1     25        Mexico             3          No
    ## 36 Valtteri Bottas        3     15        Mexico             6          No
    ## 37 Valtteri Bottas        1     25 United States             1          No
    ## 38  Lewis Hamilton        2     18 United States             5          No
    ## 39  Lewis Hamilton        7      6        Brazil             3          No
    ## 40 Valtteri Bottas       20      0        Brazil             4         Yes
    ## 41  Lewis Hamilton        1     26     Abu Dhabi             1         Yes
    ## 42 Valtteri Bottas        4     12     Abu Dhabi            20          No

### Individual driver performance

We analyzed the individual performance of each driver trough the entire
season by visualize their finish position in each race of the 2019
season.

``` r
ggplot(mercedes_data, aes(x = Track, y = Position, group = Driver)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  scale_y_reverse() +
  labs(title = "Individual Performance of Mercedes Drivers in the 2019 Season",
       x = "Grand Prix",
       y = "Final Position") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  facet_wrap(~ Driver)
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

We see that Lewis Hamilton was more consistent trough the season than
his team mate Valterri bottas \### Points contribution

We analyzed the contribution in points of each driver

``` r
ggplot(mercedes_data, aes(x = Track, y = Points, fill = Driver)) +
  geom_bar(stat = "identity") +
  labs(title = "Points Contribution by Mercedes Drivers in the 2019 Season",
       x = "Grand Prix",
       y = "Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Driver, ncol = 1)
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This is how many points collected the drivers in each race. \## Total
points analysis

### Total points by driver

We analyzed the total points that each driver did in the 2019 season

``` r
total_points <- mercedes_data %>% 
  group_by(Driver) %>% 
  summarise(Totalpoints = sum(Points, na.rm = TRUE)) 

total_points
```

    ## # A tibble: 2 × 2
    ##   Driver          Totalpoints
    ##   <chr>                 <int>
    ## 1 Lewis Hamilton          413
    ## 2 Valtteri Bottas         326

``` r
ggplot(total_points, aes(x = Driver, y = Totalpoints, fill = Driver)) +
  geom_bar(stat = 'identity') +
  labs(title = "Total Points per Driver", 
       x = "Driver",
       y = "Points") +
  theme_minimal()
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We see that Lewis Hamilton score more points than Valterri bottas.

### Total points

Total points of the team trough all the season

``` r
mercedes_points <- mercedes_data %>%
  group_by(Track) %>%
  summarise(TotalPoints = sum(Points, na.rm = TRUE)) %>%
  ungroup()

mercedes_points
```

    ## # A tibble: 21 × 2
    ##    Track         TotalPoints
    ##    <fct>               <int>
    ##  1 Australia              44
    ##  2 Bahrain                43
    ##  3 China                  43
    ##  4 Azerbaijan             43
    ##  5 Spain                  44
    ##  6 Monaco                 40
    ##  7 Canada                 38
    ##  8 France                 43
    ##  9 Austria                25
    ## 10 Great Britain          44
    ## # ℹ 11 more rows

### Track points

We analyzed which was the circuits were the team score more points

``` r
top3_mercedes_circuits <- mercedes_points %>%
  top_n(3, wt = TotalPoints) %>%
  arrange(desc(TotalPoints))

top3_mercedes_circuits
```

    ## # A tibble: 4 × 2
    ##   Track         TotalPoints
    ##   <fct>               <int>
    ## 1 Australia              44
    ## 2 Spain                  44
    ## 3 Great Britain          44
    ## 4 Russia                 44

``` r
ggplot(top3_mercedes_circuits, aes(x = Track, y = TotalPoints, fill = Track))+
  geom_bar(stat = "identity")+
  labs(title = "Points of mercedes in the 2019 season")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

We see that there were 4 best circuits where the team score the same
amount of points.

### Total pints per track

We analyzed how many points scored Mercedes per each circuit

``` r
ggplot(mercedes_points, aes(x = Track, y = TotalPoints, fill = Track))+
  geom_bar(stat = "identity")+
  labs(title = "Points of mercedes in the 2019 season")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(21))
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Total points of the season for the team

``` r
total_points_season <- mercedes_data %>% 
  summarise(TotalPoints = sum(Points, na.rm = TRUE))

total_points_season
```

    ##   TotalPoints
    ## 1         739

At the end of the season, Mercedes scored 739 points and that give them
the Constructors championship the 2019 season of Formula 1 \## Analysis
of poles and victories

### Number of poles per driver

We analyzed the number of poles that each driver did.

``` r
n_poles <- mercedes_data %>% 
  filter(Starting.Grid == 1) %>%
  count(Driver, name = "NumPoles")

n_poles
```

    ##            Driver NumPoles
    ## 1  Lewis Hamilton        5
    ## 2 Valtteri Bottas        5

``` r
ggplot(n_poles, aes(x = Driver, y = NumPoles, fill = Driver)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Poles by Mercedes Drivers in 2019",
       x = "Driver",
       y = "Number of Poles") +
  theme_minimal()
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

We see that each driver made 5 poles.

### Track poles

We analyzed in which circuits the drivers made pole position

``` r
mercedes_data$Track <- as.character(mercedes_data$Track)#Convert track into a vector

track_poles <- mercedes_data %>% 
  filter(Starting.Grid == 1) %>% 
  select(Driver, Track)

track_poles
```

    ##             Driver         Track
    ## 1   Lewis Hamilton     Australia
    ## 2  Valtteri Bottas         China
    ## 3  Valtteri Bottas    Azerbaijan
    ## 4  Valtteri Bottas         Spain
    ## 5   Lewis Hamilton        Monaco
    ## 6   Lewis Hamilton        France
    ## 7  Valtteri Bottas Great Britain
    ## 8   Lewis Hamilton       Germany
    ## 9  Valtteri Bottas United States
    ## 10  Lewis Hamilton     Abu Dhabi

### Number of victories per driver

We analyzed the number of victories that each driver did, and we
compared.

``` r
n_victories <- mercedes_data %>% 
  filter(Position == 1) %>%
  count(Driver, name = "NumVictories")

n_victories
```

    ##            Driver NumVictories
    ## 1  Lewis Hamilton           11
    ## 2 Valtteri Bottas            4

``` r
ggplot(n_victories, aes(x = Driver, y=NumVictories, fill = Driver))+
  geom_bar(stat="identity")+
  labs(title = "Number of Victories by Mercedes Drivers in 2019",
       x = "Driver",
       y = "Number of Victories")+
  theme_minimal()
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We see that Lewis Hamilton got more victories that Valterri bottas.

### Track victories

We analyzed in which circuits the drivers got a victories

``` r
n_victories <- mercedes_data %>% 
  filter(Position == 1)%>%
  count(Driver, name = "NumVictories")

n_victories
```

    ##            Driver NumVictories
    ## 1  Lewis Hamilton           11
    ## 2 Valtteri Bottas            4

### Pole to victory conversion rate

As each driver made pole and victory, we analyzed what was the
conversion rate that each driver made to convert a pole to victory

``` r
poles_victories <- mercedes_data %>%
  group_by(Driver) %>% 
  filter(Starting.Grid == 1) %>%
  summarise(TotalPoles = n(),
            PolesConvertedToWins = sum(Position == 1)) %>%
  mutate(ConversionRate = (PolesConvertedToWins / TotalPoles) * 100)

poles_victories
```

    ## # A tibble: 2 × 4
    ##   Driver          TotalPoles PolesConvertedToWins ConversionRate
    ##   <chr>                <int>                <int>          <dbl>
    ## 1 Lewis Hamilton           5                    3             60
    ## 2 Valtteri Bottas          5                    2             40

``` r
ggplot(poles_victories, aes(x = Driver, y = ConversionRate, fill = Driver)) +
  geom_bar(stat = "identity") +
  labs(title = "Pole-to-Victory Conversion Rate in 2019",
       x = "Driver",
       y = "Conversion Rate (%)") +
  theme_minimal()
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

We see that Lewis Hamilton has a better conversion rate for pole to
victory, so he made that 60% of his poles were a victory

### Poles and victory

This is for in which circuit the driver made pole and get the victory of
the same race

``` r
poles_and_victorie <- mercedes_data %>% 
  filter(Starting.Grid == 1, Position == 1) %>% 
  select(Driver, Track)

poles_and_victorie
```

    ##            Driver         Track
    ## 1 Valtteri Bottas    Azerbaijan
    ## 2  Lewis Hamilton        Monaco
    ## 3  Lewis Hamilton        France
    ## 4 Valtteri Bottas United States
    ## 5  Lewis Hamilton     Abu Dhabi

## Analyze podiums and fastest laps

\#Podiums

We analyzed how many podiums got each driver

``` r
podiums_per_driver <- mercedes_data %>%
  group_by(Driver) %>% 
  filter(Position %in% 1:3) %>%
  count(Driver, name = "NumPodiums")

podiums_per_driver
```

    ## # A tibble: 2 × 2
    ## # Groups:   Driver [2]
    ##   Driver          NumPodiums
    ##   <chr>                <int>
    ## 1 Lewis Hamilton          17
    ## 2 Valtteri Bottas         15

``` r
#Graphs of podiums
ggplot(podiums_per_driver, aes(x = Driver, y = NumPodiums, fill = Driver)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Podios por Piloto de Mercedes en 2019",
       x = "Piloto",
       y = "Número de Podios") +
  theme_minimal()
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
facet_wrap(~Driver)
```

    ## <ggproto object: Class FacetWrap, Facet, gg>
    ##     compute_layout: function
    ##     draw_back: function
    ##     draw_front: function
    ##     draw_labels: function
    ##     draw_panels: function
    ##     finish_data: function
    ##     init_scales: function
    ##     map_data: function
    ##     params: list
    ##     setup_data: function
    ##     setup_params: function
    ##     shrink: TRUE
    ##     train_scales: function
    ##     vars: function
    ##     super:  <ggproto object: Class FacetWrap, Facet, gg>

We see that Lewis Hamilton got more podiums trough the season.

### Fastests laps

We analyzed how many fastests laps got each driver.

``` r
fastest_laps_per_driver <- mercedes_data %>%
  group_by(Driver) %>% 
  filter(Fastest.Lap == "Yes") %>%
  count(Driver, name = "NumFastestLaps")

fastest_laps_per_driver
```

    ## # A tibble: 2 × 2
    ## # Groups:   Driver [2]
    ##   Driver          NumFastestLaps
    ##   <chr>                    <int>
    ## 1 Lewis Hamilton               6
    ## 2 Valtteri Bottas              3

``` r
#graphs of fastest laps
ggplot(fastest_laps_per_driver, aes(x = Driver, y = NumFastestLaps, fill = Driver)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Vueltas Rápidas por Piloto de Mercedes en 2019",
       x = "Piloto",
       y = "Número de Vueltas Rápidas") +
  theme_minimal()
```

![](Mercedes-2019-report_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
facet_wrap(~Driver)
```

    ## <ggproto object: Class FacetWrap, Facet, gg>
    ##     compute_layout: function
    ##     draw_back: function
    ##     draw_front: function
    ##     draw_labels: function
    ##     draw_panels: function
    ##     finish_data: function
    ##     init_scales: function
    ##     map_data: function
    ##     params: list
    ##     setup_data: function
    ##     setup_params: function
    ##     shrink: TRUE
    ##     train_scales: function
    ##     vars: function
    ##     super:  <ggproto object: Class FacetWrap, Facet, gg>

We see that Lewis Hamilton made more fastests laps than Valterri bottas

# Conclusion

This analysis showcases the dominance of Mercedes in the 2019 Formula 1
season, with outstanding performance in poles, victories,tracks and
points accumulated by their drivers. The charts and statistics presented
help visualize and better understand the performance of Mercedes drivers
throughout the season.
