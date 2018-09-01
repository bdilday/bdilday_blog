---
title: Catcher Defense with Linear Mixed Effect Models
author: ~
date: '2018-09-01'
slug: catcher-defense-with-linear-mixed-effect-models
categories: []
tags: []
---

In baseball, measuring catcher defense is notoriously tricky. This is because it includes things like pitch framing and game calling that aren't captured in traditional stats sheets. One approach to this is to do a "with-or-without you" analysis, in other words look at the difference in outcome depending on catcher, holding as many other contributing factors constant as possible. One prominent example of this is [Tom Tango's analysis of passed balls and wild pitches](http://tangotiger.net/catchers.html) - events where the responsibility can reasonably be assumed to be limited to the pitcher and catcher. 

A related approach is to use generalized linear mixed effect models. Notably, this is the approach taken by Baseball Prospectus in [their measurements of catcher framing](https://www.baseballprospectus.com/news/article/25514/moving-beyond-wowy-a-mixed-approach-to-measuring-catcher-framing/).

The analysis I present here lies somewhere in between these two - but is somewhat orthogonal as well. It's different than the Baseball Prospectus approach because it controls for many fewer factors and it's orthogonal to both because it measures an outcome that has many more causal factors in addition to pitcher and catcher identity.

## My analysis

My analysis looks at the number of runs scored by the opponent, and tries to assign credit to the catcher. There are lots of factors besides catcher that contribute to runs scored, for example,

* pitcher

* defense

* batters(s)

* park

This makes assigning credit to the catcher inherently more noisy than something more constrained like wild pitches, passed balls, or called strikes. Nevertheless, I'm going to fit the model and see what happens - as I've written before [I don't believe in letting the perfect be the enemy of the good](http://graphswithcodeanddata.xyz/2018/06/17/closeness-index-one-run-games/)

The present analysis is an alternative version of a topic I've looked at before, which is estimating [catcher defense back to 1911](https://rpubs.com/bdilday/205604) using the same linear mixed effect modeling framework. For the work linked above I used only game final scores. What I do here is to count only the runs scored through the end of the 5th inning. The logic behind that choice is that it will mitigate complications of knowing which relief pitchers were brought in - it's mostly true that the starting pitcher will still be there in the 5th. In the current analysis I use retrosheet play-by-play data from 1961 - 2017. 

## linear mixed effect models

Linear mixed effect models are appropriate when there are groups within the data, and there are coefficients that vary across groups. The canonical example I think of is schools - if you model outcomes (test scores, or graduation rates, say) as a function of spending on after school programs or something similar, you would expect a positively correlated relationship between outcomes and spending. However, each school starts from it's own baseline. So if you threw them all in to the same linear regression model, you'd get a model that doesn't match the reality. In the linear mixed effect model framework you can fit a slope for outcome vs spending that is constant across groups (the fixed effect) and a intercept that is different for each group (the random effect). In the baseball context, the catchers, pitchers, park, offense and defense are all modeled as random effects. 

The linear mixed effect model itself is essentially a regression with a L2 penalty - a ridge regression. One main difference is that in a ridge regression you specify the penalty strength, and the same value applies to all variables. In the linear mixed effect model, the penalty is applied only to the random effects, the strength of the penalty varies across groups, and the strength values aren't specified but are estimated based on maximum likelihood analysis of the profiled likelihood - the likelihood integrated over the random effect values. Technically what the model gives as output is the mode of the random effects values. In many cases this is a good approximation to the mean of the posterior probability distribution, but in general has a somewhat squishy interpretation.

A great interactive visualization of mixed effects models [is available here](http://mfviz.com/hierarchical-models/). A good technical discussion is available in the [`lme4` "Computational Methods" vignette](https://cran.r-project.org/web/packages/lme4/index.html)


## The data

For this analysis I pulled event data from my local copy of the retrosheet play-by-play data. I limited the seasons from 1961 through 2017. For each game I took the starting catcher ID, the starting pitcher ID, the home team ID (i.e. the park), the season year, the score through the end of the 5th inning. The data are available as [a gist here](https://gist.github.com/bdilday/6f85ed94c2fe51a623a1ec7ddd0e8151).


```r
library(dplyr)
```


```r
# load the data 
df = readr::read_csv("https://gist.githubusercontent.com/bdilday/6f85ed94c2fe51a623a1ec7ddd0e8151/raw/c1d7254163b5eb777025028734cf0b8f53087270/catcher_defense_data.csv")
#> Parsed with column specification:
#> cols(
#>   game_id = col_character(),
#>   year_id = col_integer(),
#>   bat_home_id = col_integer(),
#>   off_score = col_integer(),
#>   def_pitcher = col_character(),
#>   def_catcher = col_character(),
#>   off_team = col_character(),
#>   def_team = col_character(),
#>   park_id = col_character(),
#>   park = col_character()
#> )
df$year_id = factor(df$year_id)
head(df)
#> # A tibble: 6 x 10
#>   game_id   year_id bat_home_id off_score def_pitcher def_catcher off_team
#>   <chr>     <fct>         <int>     <int> <chr>       <chr>       <chr>   
#> 1 WS219610… 1961              0         3 danib102_1… retzk101_1… MIN_1961
#> 2 WS219610… 1961              0         2 mcclj104_1… retzk101_1… MIN_1961
#> 3 WS219610… 1961              0         1 danib102_1… retzk101_1… KC1_1961
#> 4 WS219610… 1961              0         1 hobae101_1… retzk101_1… BOS_1961
#> 5 WS219610… 1961              0         0 burnp102_1… retzk101_1… BOS_1961
#> 6 WS219610… 1961              0         2 gablg102_1… dalep101_1… CHA_1961
#> # ... with 3 more variables: def_team <chr>, park_id <chr>, park <chr>
```

Note that each pitcher, team, defense, etc have to have the season appended so that each season is treated as a different entity.

The model I use is this,


```r
library(lme4)
#> Loading required package: Matrix
#> Loading required package: methods
model_catcher_def = function(df) {
  lmer_mod = lmer(off_score ~ 
                    1 + year_id + 
                    (1|park_id) + (1|off_team) + 
                    (1|def_team) + (1|def_pitcher) + 
                    (1|def_catcher), data=df)
}
```

## model analysis

Here I execute the model


```r
lmer_mod = model_catcher_def(df)
```

Here's the summary of the standard deviation of the random effects. One interpretation of these is that the higher the value, the more significant that factor is in determining the outcome. So we can see that pitcher is most important, followed by park, offensive team, defensive team and finally, catcher - it would certainly be surprising if we had found catcher was the most important factor! 


```r
summary(lmer_mod)$varcor
#>  Groups      Name        Std.Dev.
#>  def_pitcher (Intercept) 0.324146
#>  def_catcher (Intercept) 0.099711
#>  def_team    (Intercept) 0.182797
#>  off_team    (Intercept) 0.197680
#>  park_id     (Intercept) 0.237530
#>  Residual                2.306134
```

In order to extract the random effect values, I define some helpers


```r
# parse random effects to a data frame
ranef_to_df = function(lmer_mod, ranef_nm) {
  rr = ranef(lmer_mod)
  data.frame(k=rownames(rr[ranef_nm][[1]]),
             value=rr[ranef_nm][[1]][,1],
             ranef_nm = ranef_nm,
             stringsAsFactors = FALSE)

}

# loop over all random effects and parse to data frame
parse_ranefs = function(lmer_mod) {
  rfs = names(ranef(lmer_mod))
  ll = lapply(rfs, function(rf) {
    ranef_to_df(lmer_mod, rf)
  }) %>% dplyr::bind_rows()
}
```

Here I get all the random effects


```r
ranef_summary_df = parse_ranefs(lmer_mod)
```

The columns are:

* `k`: the unique key of the random effect

* `value`: the value, in units of runs per 15 outs

* `ranef_nm`: the name of the random effect

Let's see which keys had the highest (and lowest) values, for each random effect. 

First, the team variables


```r
ranef_summary_df %>% 
  group_by(ranef_nm) %>% 
  filter(value==min(value) | value == max(value)) %>% 
  ungroup() %>% filter(ranef_nm %in% 
                         c("def_team", "off_team", "park_id"))
#> # A tibble: 6 x 3
#>   k         value ranef_nm
#>   <chr>     <dbl> <chr>   
#> 1 ATL_1995 -0.341 def_team
#> 2 DET_1996  0.458 def_team
#> 3 HOU_1964 -0.401 off_team
#> 4 TOR_2015  0.464 off_team
#> 5 COL_1996  0.947 park_id 
#> 6 SDN_1998 -0.470 park_id
```

So the model tells us:

* best defense: 1995 Braves

* worst defense: 1996 Tigers

The "defense" value here is based on runs scored so there's an interdependence between pitchers and defensive play - it's probably more correct to think of it as the best team run prevention.

* best offense: 2015 Blue Jays

* worst offense: 1964 Astros

The 2015 Blue Jays as best offense since 1961 seems surprising. I can double check by looking at the z-score of runs scored, with the `Lahman` data.


```r
Lahman::Teams %>% 
  group_by(yearID) %>% 
  mutate(m=mean(R), s=sd(R), z=(R-m)/s) %>% 
  select(yearID, teamID, R, m, s, z) %>% ungroup() %>% 
  arrange(-z) %>% head(10) %>% as.data.frame() 
#>    yearID teamID    R        m         s        z
#> 1    2015    TOR  891 688.2333  58.76175 3.450657
#> 2    1915    DET  778 592.2083  62.16736 2.988573
#> 3    2007    NYA  968 777.4000  69.05450 2.760139
#> 4    1976    CIN  857 645.5000  78.12420 2.707228
#> 5    1953    BRO  955 714.1250  91.90058 2.621039
#> 6    2016    BOS  878 724.8000  59.78259 2.562619
#> 7    1982    ML4  891 696.5385  76.69432 2.535540
#> 8    2006    NYA  930 786.6333  57.18782 2.506944
#> 9    2005    BOS  910 744.1667  66.35541 2.499168
#> 10   1950    BOS 1027 750.8125 110.94096 2.489500
```

Well, there you have it! 2015 Blue Jays have the highest z-score for runs scored in baseball history! 

* largest park factor: 1996 Colorado

* lowest park factor: 1998 San Diego

Seems plausible.

Now for the player based estimates. First I define a table to match the records and get the actual names instead of the esoteric retrosheet IDs.


```r
pl_lkup = Lahman::Master %>% 
  mutate(nameFull = paste(nameFirst, nameLast)) %>%
  dplyr::select(retroID, nameFull)
```

Get the player based min and max random effects


```r
player_summary = ranef_summary_df %>% 
  group_by(ranef_nm) %>% 
  filter(value==min(value) | value == max(value)) %>% 
  filter(ranef_nm %in% c("def_pitcher", "def_catcher"))

# add a new column to strip the year from the name to merge
# with the player lookup
player_summary$retroID = sapply(
  stringr::str_split(player_summary$k, "_"), 
  function(s) {s[[1]]})

player_summary %>% 
  merge(pl_lkup, by="retroID") %>% 
  dplyr::select(nameFull, k, value, ranef_nm)
#>          nameFull             k      value    ranef_nm
#> 1      Mike Heath heatm001_1985  0.1272693 def_catcher
#> 2 Charles Johnson johnc002_1996 -0.1073388 def_catcher
#> 3   Randy Johnson johnr005_2004 -0.6903628 def_pitcher
#> 4 Todd Van Poppel vanpt001_1996  0.5976675 def_pitcher
```

So we have

* best pitcher: 2004 Randy Johnson

* worst pitcher: 1996 Todd Von Poppel

Passes the sniff test.

* best catcher: 1996 Charles Johnson

* worst catcher: 1985 Mike Heath

I recall Charles Johnson being considered a great defensive catcher. As far as Mike Heath, I have no idea if that's plausible or not.

## yearly and career total values

The random effects values tell us run values on a per game basis. Here I define a function to aggregate over a season, and over a career, to define a total-value counting stat.



```r
ranef_rankings = function(ranef_summary_df, ranef_nm_) {
  pl_lkup = Lahman::Master %>% 
    mutate(nameFull = paste(nameFirst, nameLast)) %>%
    dplyr::select(retroID, nameFull)

  aa = ranef_summary_df %>%
    filter(ranef_nm == ranef_nm_) %>%
    merge(df, by.x="k", by.y=ranef_nm_)

  aa$player_id = sapply(stringr::str_split(aa$k, "_"), 
                         function(s) {s[[1]]})
  aa$season = sapply(stringr::str_split(aa$k, "_"), 
                     function(s) {s[[2]]})
  career = aa %>%
    group_by(player_id) %>%
    summarise(mean_value=mean(value), 
              sum_value=sum(value)) %>% arrange(mean_value) %>%
    mutate(mean_rank = row_number()) %>% ungroup() %>%
    merge(pl_lkup, by.x="player_id", by.y="retroID")

  yearly = aa %>%
    group_by(player_id, season) %>%
        summarise(mean_value=mean(value), 
              sum_value=sum(value)) %>% arrange(mean_value) %>%
    mutate(mean_rank = row_number()) %>% ungroup() %>%
    merge(pl_lkup, by.x="player_id",by.y="retroID")

  list(yearly = yearly, career = career)
}
```

### pitchers

Although the point here was to look at catchers, I'll first apply this to pitchers as a sanity check.


```r
pitcher_rankings = ranef_rankings(ranef_summary_df, "def_pitcher")
```

The top ten runs-per game by pitcher season


```r
pitcher_rankings$yearly %>% 
  arrange(mean_value) %>% 
  head(10) %>% 
  as.data.frame()
#>    player_id season mean_value sum_value mean_rank       nameFull
#> 1   johnr005   2004 -0.6903628 -24.16270         1  Randy Johnson
#> 2   martp001   2000 -0.6803334 -19.72967         1 Pedro Martinez
#> 3   schic002   2004 -0.6311972 -20.19831         1 Curt Schilling
#> 4   johnr005   1995 -0.6275477 -18.82643         2  Randy Johnson
#> 5   maddg002   1995 -0.6234225 -17.45583         1    Greg Maddux
#> 6   santj003   2006 -0.6000581 -20.40197         1  Johan Santana
#> 7   johnr005   2001 -0.5937011 -20.18584         3  Randy Johnson
#> 8   clemr001   1997 -0.5897730 -20.05228         1  Roger Clemens
#> 9   appik001   1993 -0.5600134 -19.04046         1   Kevin Appier
#> 10  martp001   2003 -0.5593239 -16.22039         2 Pedro Martinez
```

Seems plausible. I'd like to see Pedro 2001, and I'm not sure I like seeing Kevin Appier on there, but anyway moving on...

The top ten runs per season


```r
pitcher_rankings$yearly %>% 
  arrange(sum_value) %>% 
  head(10) %>% 
  as.data.frame()
#>    player_id season mean_value sum_value mean_rank       nameFull
#> 1   johnr005   2004 -0.6903628 -24.16270         1  Randy Johnson
#> 2   carls001   1980 -0.5438898 -20.66781         1  Steve Carlton
#> 3   santj003   2006 -0.6000581 -20.40197         1  Johan Santana
#> 4   marij101   1963 -0.5057191 -20.22877         2  Juan Marichal
#> 5   schic002   2004 -0.6311972 -20.19831         1 Curt Schilling
#> 6   johnr005   2001 -0.5937011 -20.18584         3  Randy Johnson
#> 7   clemr001   1997 -0.5897730 -20.05228         1  Roger Clemens
#> 8   martp001   2000 -0.6803334 -19.72967         1 Pedro Martinez
#> 9   marij101   1966 -0.5452669 -19.62961         1  Juan Marichal
#> 10  koufs101   1963 -0.4859726 -19.43891         1   Sandy Koufax
```

The top ten mean per game over the career


```r
pitcher_rankings$career %>% 
  arrange(mean_value) %>% 
  head(10) %>% 
  as.data.frame()
#>    player_id mean_value  sum_value mean_rank        nameFull
#> 1   kersc001 -0.3617644 -104.91167         1 Clayton Kershaw
#> 2   martp001 -0.3222799 -131.81246         2  Pedro Martinez
#> 3   webbb001 -0.3018462  -59.76554         3    Brandon Webb
#> 4   salec001 -0.2989121  -53.80418         4      Chris Sale
#> 5   clemr001 -0.2977737 -210.52597         5   Roger Clemens
#> 6   santj003 -0.2959499  -84.04978         6   Johan Santana
#> 7   koufs101 -0.2847562  -60.08356         7    Sandy Koufax
#> 8   schic002 -0.2841804 -123.90265         8  Curt Schilling
#> 9   johnr005 -0.2605288 -157.09886         9   Randy Johnson
#> 10  hallr001 -0.2538522  -99.00237        10    Roy Halladay
```

The top ten total over the career


```r
pitcher_rankings$career %>% 
  arrange(sum_value) %>% 
  head(10) %>% 
  as.data.frame()
#>    player_id mean_value sum_value mean_rank       nameFull
#> 1   clemr001 -0.2977737 -210.5260         5  Roger Clemens
#> 2   johnr005 -0.2605288 -157.0989         9  Randy Johnson
#> 3   maddg002 -0.1970610 -145.8252        39    Greg Maddux
#> 4   martp001 -0.3222799 -131.8125         2 Pedro Martinez
#> 5   seavt001 -0.2006751 -129.8368        37     Tom Seaver
#> 6   schic002 -0.2841804 -123.9027         8 Curt Schilling
#> 7   mussm001 -0.2168544 -116.2340        28   Mike Mussina
#> 8   palmj001 -0.2120193 -110.4621        32     Jim Palmer
#> 9   blylb001 -0.1596118 -109.3341        85  Bert Blyleven
#> 10  glavt001 -0.1577112 -107.5590        88    Tom Glavine
```
  
### catchers

And finally, the best defensive catchers according to this methodology


```r
catcher_rankings = ranef_rankings(ranef_summary_df, "def_catcher")
```

The top ten runs-per game by catcher season


```r
catcher_rankings$yearly %>% 
  arrange(mean_value) %>% 
  head(10) %>% 
  as.data.frame()
#>    player_id season  mean_value  sum_value mean_rank        nameFull
#> 1   johnc002   1996 -0.10733880 -12.129284         1 Charles Johnson
#> 2   essij001   1980 -0.09486632  -6.261177         1      Jim Essian
#> 3   hernr002   2002 -0.09423886 -11.779858         1 Ramon Hernandez
#> 4   ausmb001   2005 -0.09126695 -10.769500         1     Brad Ausmus
#> 5   pagnt001   1996 -0.08716409  -9.413722         1    Tom Pagnozzi
#> 6   lodup001   2003 -0.08639865 -10.367838         1    Paul Lo Duca
#> 7   kendj001   2007 -0.08443976 -10.977169         1   Jason Kendall
#> 8   lopej001   1994 -0.08405470  -6.051938         1      Javy Lopez
#> 9   cartg001   1979 -0.07873492 -10.629214         1     Gary Carter
#> 10  varij001   2001 -0.07814173  -3.672661         1   Jason Varitek
```

The top ten runs per season


```r
catcher_rankings$yearly %>% 
  arrange(sum_value) %>% 
  head(10) %>% 
  as.data.frame()
#>    player_id season  mean_value  sum_value mean_rank        nameFull
#> 1   johnc002   1996 -0.10733880 -12.129284         1 Charles Johnson
#> 2   hernr002   2002 -0.09423886 -11.779858         1 Ramon Hernandez
#> 3   kendj001   2007 -0.08443976 -10.977169         1   Jason Kendall
#> 4   ausmb001   2005 -0.09126695 -10.769500         1     Brad Ausmus
#> 5   cartg001   1979 -0.07873492 -10.629214         1     Gary Carter
#> 6   lodup001   2003 -0.08639865 -10.367838         1    Paul Lo Duca
#> 7   piazm001   1996 -0.06918476  -9.893421         1     Mike Piazza
#> 8   cartg001   1982 -0.06477695  -9.781320         2     Gary Carter
#> 9   moliy001   2013 -0.07435972  -9.518044         1   Yadier Molina
#> 10  pagnt001   1996 -0.08716409  -9.413722         1    Tom Pagnozzi
```

The top ten mean per game over the career


```r
catcher_rankings$career %>% 
  arrange(mean_value) %>% 
  head(10) %>% 
  as.data.frame()
#>    player_id  mean_value  sum_value mean_rank      nameFull
#> 1   sweer101 -0.03734670  -7.207914         1    Rick Sweet
#> 2   cartg001 -0.03618157 -70.698796         2   Gary Carter
#> 3   maill001 -0.03547138  -2.802239         3    Luke Maile
#> 4   skagd101 -0.03333700  -5.700627         4   Dave Skaggs
#> 5   moliy001 -0.03205300 -52.983609         5 Yadier Molina
#> 6   ausmb001 -0.03072853 -54.266592         6   Brad Ausmus
#> 7   moora001 -0.03063716  -2.236513         7    Adam Moore
#> 8   josec002 -0.03035304  -8.468497         8  Caleb Joseph
#> 9   johnr009 -0.03012177  -6.446059         9   Rob Johnson
#> 10  rodgb102 -0.03002153 -24.377485        10  Buck Rodgers
```

The top ten total over the career


```r
catcher_rankings$career %>% 
  arrange(sum_value) %>% 
  head(10) %>% 
  as.data.frame()
#>    player_id  mean_value sum_value mean_rank         nameFull
#> 1   cartg001 -0.03618157 -70.69880         2      Gary Carter
#> 2   ausmb001 -0.03072853 -54.26659         6      Brad Ausmus
#> 3   moliy001 -0.03205300 -52.98361         5    Yadier Molina
#> 4   piazm001 -0.02755544 -44.14381        16      Mike Piazza
#> 5   penat001 -0.02413506 -42.93628        24        Tony Pena
#> 6   varij001 -0.02997632 -41.12751        11    Jason Varitek
#> 7   piera001 -0.02034347 -37.26924        42 A. J. Pierzynski
#> 8   martr004 -0.02379236 -32.92862        25   Russell Martin
#> 9   fiskc001 -0.01528992 -32.06297        76     Carlton Fisk
#> 10  bencj101 -0.01703744 -27.70288        62     Johnny Bench
```
  
## Conclusion

I've presented a way of using linear mixed effects models to measure overall catcher defense, encompassing framing, game calling, etc. As a consequence of the mixed effect model, we also get pitcher estimates that can be used as a sanity check. There is a tight coupling of pitcher with defense and there's anecdotal evidence that the model has not adequately distinguished the independent effects of those two factors - and a similar, but probably lesser, effect exists for catcher numbers. So these results should be though of as, ahem, ballpark estimates. 

There are a number of ways the study could be improved on in a follow up including,

* use more seasons, based on box score data

* model the runs scored as a non-Gaussian random variable, e.g. zero-inflated negative binomial

* normalize the runs distribution across seasons - as it stands it favors high run environments (because it measures absolute runs and there are more to go around when the environment is high)



