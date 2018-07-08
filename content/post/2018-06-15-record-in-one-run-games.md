---
title: Record in One-run Games
author: ~
date: '2018-06-15'
slug: record-in-one-run-games
categories: []
tags: []
output:
  blogdown::html_page
---

So far this season the Seattle Mariners have won a historically high number of one-run games. As of this writing they are `21 - 9`, and `1/2` game up on the Astros despite having a run-differential of about `100` runs less. This post looks at records in one-run games over the years.

## Libraries


```r
library(RPostgres)
library(DBI)
library(dplyr)
library(ggplot2)
library(magrittr) # for the in-place assignment, %<>%
library(ggrepel)
```

## Data 

I'm using the `game` data from retrosheet. 


```r
conn <- dbConnect(RPostgres::Postgres(), 
                  password=Sys.getenv("PSQL_PASS"), 
                  user=Sys.getenv("PSQL_USER"), 
                  port=Sys.getenv("PSQL_PORT"), 
                  dbname='retrosheet')
df1 = dbGetQuery(conn, "select * from game ")
```

Make two data frames, one where the home team is "the team" and away team is "the opponent", and visa versa, then `rbind` them into a single data frame.


```r
dfA = df1 %>% 
  select(game_id, year_id, team_id=away_team_id, opp_id=home_team_id, team_score=away_score_ct, opp_score=home_score_ct) 
dfH = df1 %>% 
  select(game_id, year_id, team_id=home_team_id, opp_id=away_team_id, team_score=home_score_ct, opp_score=away_score_ct)
dfX = dplyr::bind_rows(dfA, dfH)
```


Compute run differential and an indicator for one-run games.


```r
games = dfX %>% mutate(sc_diff = team_score - opp_score, 
                       is_onerun = abs(sc_diff) == 1, 
                       is_win = team_score > opp_score)
```




```r
records = games %>% 
  group_by(year_id, team_id) %>% 
  summarise(games=n(), 
            wins=sum(is_win), 
            one_run_games=sum(is_onerun), 
            one_run_wins=sum(is_onerun * is_win), 
            wpct=wins/games, 
            wpct_onerun=one_run_wins/one_run_games) %>% 
  ungroup()
```

Let's see which teams had the most one-run wins


```r
top12_wins1 = records %>% 
  arrange(-one_run_wins) %>% 
  head(12) 
  
top12_wins1 %>% knitr::kable()
```



| year_id|team_id | games| wins| one_run_games| one_run_wins|      wpct| wpct_onerun|
|-------:|:-------|-----:|----:|-------------:|------------:|---------:|-----------:|
|    1978|SFN     |   162|   89|            68|           42| 0.5493827|   0.6176471|
|    1940|CIN     |   155|  100|            58|           41| 0.6451613|   0.7068966|
|    1969|NYN     |   162|  100|            64|           41| 0.6172840|   0.6406250|
|    1970|BAL     |   162|  108|            55|           40| 0.6666667|   0.7272727|
|    1974|BAL     |   162|   91|            61|           40| 0.5617284|   0.6557377|
|    1979|HOU     |   162|   89|            66|           39| 0.5493827|   0.5909091|
|    1985|CIN     |   162|   89|            57|           39| 0.5493827|   0.6842105|
|    1967|CHA     |   162|   89|            64|           38| 0.5493827|   0.5937500|
|    1972|CHA     |   154|   87|            58|           38| 0.5649351|   0.6551724|
|    1982|SFN     |   162|   87|            66|           38| 0.5370370|   0.5757576|
|    1993|KCA     |   162|   84|            70|           38| 0.5185185|   0.5428571|
|    1943|NYA     |   153|   96|            60|           37| 0.6274510|   0.6166667|


```r
runs_diff_plot = function(dfX, title='') {
  dfX %>% 
    dplyr::inner_join(games, by=c("team_id", "year_id")) %>% 
    mutate(name=paste(team_id, year_id)) %>% 
    ggplot(aes(x=sc_diff)) + 
    geom_histogram(binwidth = 1) +
    facet_wrap(~name, ncol=3) + 
    theme_minimal(base_size = 16) + 
    labs(x='score differential', 
         title=title)  
}
```


```r
p = runs_diff_plot(top12_wins1, 'score differential - top 12 1-run win-pct teams')
print(p)
```

![plot of chunk unnamed-chunk-8](/post/2018-06-15-record-in-one-run-games_files/figure-html/unnamed-chunk-8-1.png)

Highest win percentage in 1-run games


```r
top12_wpct1 = records %>% 
  arrange(-wpct_onerun) %>% 
  head(12) 
  
top12_wpct1 %>% knitr::kable()
```



| year_id|team_id | games| wins| one_run_games| one_run_wins|      wpct| wpct_onerun|
|-------:|:-------|-----:|----:|-------------:|------------:|---------:|-----------:|
|    2016|TEX     |   162|   95|            47|           36| 0.5864198|   0.7659574|
|    2012|BAL     |   162|   93|            38|           29| 0.5740741|   0.7631579|
|    1981|BAL     |   105|   59|            28|           21| 0.5619048|   0.7500000|
|    1925|WS1     |    81|   53|            23|           17| 0.6543210|   0.7391304|
|    1970|BAL     |   162|  108|            55|           40| 0.6666667|   0.7272727|
|    1954|CLE     |   156|  111|            45|           32| 0.7115385|   0.7111111|
|    1961|CIN     |   154|   93|            48|           34| 0.6038961|   0.7083333|
|    1980|KCA     |   162|   97|            41|           29| 0.5987654|   0.7073171|
|    1940|CIN     |   155|  100|            58|           41| 0.6451613|   0.7068966|
|    1986|BOS     |   161|   95|            34|           24| 0.5900621|   0.7058824|
|    1977|KCA     |   162|  102|            44|           31| 0.6296296|   0.7045455|
|    1959|CHA     |   156|   94|            50|           35| 0.6025641|   0.7000000|


```r
p = runs_diff_plot(top12_wpct1, 'score differential - top 12 1-run win-pct teams')
print(p)
```

![plot of chunk unnamed-chunk-10](/post/2018-06-15-record-in-one-run-games_files/figure-html/unnamed-chunk-10-1.png)

## record in non-one-run games


```r
records %<>% mutate(
  non_onerun_wins = wins-one_run_wins,
  non_onerun_games = games-one_run_games,
  wpct_non_onerun = non_onerun_wins / non_onerun_games
  )
```

Top 12 in **non-one-run games**


```r
top12_winX = records %>% 
  arrange(-non_onerun_wins) %>% 
  head(12)
top12_winX %>% knitr::kable()
```



| year_id|team_id | games| wins| one_run_games| one_run_wins|      wpct| wpct_onerun| non_onerun_wins| non_onerun_games| wpct_non_onerun|
|-------:|:-------|-----:|----:|-------------:|------------:|---------:|-----------:|---------------:|----------------:|---------------:|
|    1998|NYA     |   162|  114|            31|           21| 0.7037037|   0.6774194|              93|              131|       0.7099237|
|    2001|SEA     |   162|  116|            38|           26| 0.7160494|   0.6842105|              90|              124|       0.7258065|
|    1948|CLE     |   156|   97|            30|           10| 0.6217949|   0.3333333|              87|              126|       0.6904762|
|    1927|NYA     |   155|  110|            43|           24| 0.7096774|   0.5581395|              86|              112|       0.7678571|
|    1931|PHA     |   150|  104|            33|           19| 0.6933333|   0.5757576|              85|              117|       0.7264957|
|    2003|ATL     |   162|  101|            42|           17| 0.6234568|   0.4047619|              84|              120|       0.7000000|
|    1998|ATL     |   162|  106|            44|           23| 0.6543210|   0.5227273|              83|              118|       0.7033898|
|    2002|NYA     |   161|  103|            42|           21| 0.6397516|   0.5000000|              82|              119|       0.6890756|
|    2004|BOS     |   162|   98|            34|           16| 0.6049383|   0.4705882|              82|              128|       0.6406250|
|    2017|CLE     |   162|  102|            35|           20| 0.6296296|   0.5714286|              82|              127|       0.6456693|
|    2017|HOU     |   162|  101|            32|           19| 0.6234568|   0.5937500|              82|              130|       0.6307692|
|    1949|BRO     |   156|   97|            28|           16| 0.6217949|   0.5714286|              81|              128|       0.6328125|


```r
p = runs_diff_plot(top12_winX, 'score differential - top 12 non-1-run win teams')
print(p)
```

![plot of chunk unnamed-chunk-13](/post/2018-06-15-record-in-one-run-games_files/figure-html/unnamed-chunk-13-1.png)


```r
top12_wpctX = records %>% 
  arrange(-wpct_non_onerun) %>% 
  head(12)
top12_wpctX %>% knitr::kable()
```



| year_id|team_id | games| wins| one_run_games| one_run_wins|      wpct| wpct_onerun| non_onerun_wins| non_onerun_games| wpct_non_onerun|
|-------:|:-------|-----:|----:|-------------:|------------:|---------:|-----------:|---------------:|----------------:|---------------:|
|    1944|SLN     |   122|   86|            33|           17| 0.7049180|   0.5151515|              69|               89|       0.7752809|
|    1927|NYA     |   155|  110|            43|           24| 0.7096774|   0.5581395|              86|              112|       0.7678571|
|    1932|NYA     |   127|   90|            42|           26| 0.7086614|   0.6190476|              64|               85|       0.7529412|
|    1942|SLN     |   139|   93|            43|           21| 0.6690647|   0.4883721|              72|               96|       0.7500000|
|    1943|SLN     |   138|   93|            56|           33| 0.6739130|   0.5892857|              60|               82|       0.7317073|
|    1942|NYA     |   152|  101|            44|           22| 0.6644737|   0.5000000|              79|              108|       0.7314815|
|    1929|CHN     |   105|   68|            27|           11| 0.6476190|   0.4074074|              57|               78|       0.7307692|
|    1931|PHA     |   150|  104|            33|           19| 0.6933333|   0.5757576|              85|              117|       0.7264957|
|    2001|SEA     |   162|  116|            38|           26| 0.7160494|   0.6842105|              90|              124|       0.7258065|
|    1935|CHN     |   118|   80|            35|           20| 0.6779661|   0.5714286|              60|               83|       0.7228916|
|    1934|NY1     |   120|   80|            39|           22| 0.6666667|   0.5641026|              58|               81|       0.7160494|
|    1939|NYA     |   143|   99|            35|           22| 0.6923077|   0.6285714|              77|              108|       0.7129630|


```r
p = runs_diff_plot(top12_wpctX,  'score differential - top 12 non-1-run win-pct teams')
print(p)
```

![plot of chunk unnamed-chunk-15](/post/2018-06-15-record-in-one-run-games_files/figure-html/unnamed-chunk-15-1.png)


Finally, compare winning percentage in 1-run games to winning percentage in non-one-run games


```r
p = records %>% 
  ggplot(aes(x=wpct_non_onerun, y=wpct_onerun)) + 
  geom_point() + 
  theme_minimal(base_size = 16) + 
  geom_hline(yintercept = 0.5, size=0.5, color='steelblue') + 
  geom_vline(xintercept = 0.5, size=0.5, color='steelblue') + 
  labs(x="Win Pct. - Non 1-run", y="Win Pct. - 1-run")
print(p)
```

![plot of chunk unnamed-chunk-16](/post/2018-06-15-record-in-one-run-games_files/figure-html/unnamed-chunk-16-1.png)

Label the ones that are on the edges


```r
records_quads = records %>% 
  mutate(r2=(wpct_onerun - 0.5)**2 + (wpct_non_onerun - 0.5)**2, 
         quad=as.integer(wpct_non_onerun >= 0.5) + 2 * as.integer(wpct_onerun >= 0.5))
```



```r
lab_df = records_quads %>% 
  group_by(quad) %>% 
  arrange(-r2) %>% 
  mutate(r2_rank=row_number()) %>% 
  filter(r2_rank <= 10) %>% 
  ungroup() %>% 
  mutate(name=paste(team_id, year_id))
```



```r
p2 = p + 
  geom_text_repel(data = lab_df, aes(x=wpct_non_onerun, y=wpct_onerun, label=name))
print(p2)
```

![plot of chunk unnamed-chunk-19](/post/2018-06-15-record-in-one-run-games_files/figure-html/unnamed-chunk-19-1.png)
