---
title: 'Storytelling With Data (SWD) Challenge: Your Choice Makover'
author: ~
date: '2018-07-07'
slug: storytelling-with-data-swd-challenge-your-choice-makover
categories: []
tags: []
---

# Introduction: SWDChallenge

On her blog and Twitter account, *Storytelling With Data*, Cole Nussbaumer Knaflic is running a [monthly challenge on effective data visualization] (http://www.storytellingwithdata.com/swdchallenge/). Previous challenges have mainly focused on using specific graph types (e.g., [square area](http://www.storytellingwithdata.com/blog/2018/4/2/swdchallenge-square-area-graph), [bar charts](http://www.storytellingwithdata.com/blog/2018/2/28/swdchallenge-basic-bars), [slopegraphs](http://www.storytellingwithdata.com/blog/2018/6/1/swdchallenge-slopegraph)) or elements ([annotation](http://www.storytellingwithdata.com/blog/2018/1/2/introducing-the-2018-swdchallenge), [color](http://www.storytellingwithdata.com/blog/2018/2/8/swdchallenge-education-color-and-words)). This month [the challenge](http://www.storytellingwithdata.com/blog/2018/7/1/swdchallenge-your-choice-makeover) is to remake a less-than-ideal graph. I happen to have one in mind, so I'm jumping in to the challenge!



# The less-than-ideal graph

The graph I'm going to remake comes from the Kansas City, MO, [citizen satisfaction survey for 2012-2013] (http://kcmo.gov/wp-content/uploads/2013/03/2012_13_Kansas_City_Missouri_Citizen_Survey_Final_Report.pdf). It shows the composite citizen satisfaction for Kansas City compared against the National score.

![](/post/img/kc_original.png)

I happened to come across this graph as part of my day-job and it immediately stuck out to me because it's so hostile to visualization rules-of-thumb and best-practices. 

The issues I see with the graph are:

* using color to encode time

* using distance along the x-axis to encode the categorical variable `location`

* placing comparisons far away from one another, e.g. the value at concurrent times for Kansas City cannot be directly compared against the ones for National

* the title is too long and is redundant with the axis labels

* putting the value in text above the bar which is redundant with encoding that data in the length of the bar

* no label in the y-axis

* bars that don't start at `0`

* arbitrary zero-point


On the arbitrary zero-point, it's unclear if the `100` value for the baseline survey denotes the same meaning for Kansas City vs. National. My reading is that the value are normalized so that each are being compared against their respective scores from 2005. So in effect what's being shown is the change from the baseline, but expressed as 100 + change, and plotted as a bar. This seems, um, less than ideal.

Below I will approximately reproduce this chart in `R` and then suggest an alternative

## R libraries

Here are the libraries I need


```r
library(dplyr)
library(ggplot2)
library(tidyr)
```

## data 

For the data, I read the data off the chart and made a simple CSV file and then read it in. The file looks like,

```
year,location,score
2005,Kansas City,100
2010,Kansas City,108
2011,Kansas City,109
2012,Kansas City,111
2005,National,100
2010,National,92
2011,National,91
2012,National,92
```



and once I read it in, the data frame like,


```r
kc_df
#> # A tibble: 8 x 3
#>    year location    score
#>   <int> <chr>       <int>
#> 1  2005 Kansas City   100
#> 2  2010 Kansas City   108
#> 3  2011 Kansas City   109
#> 4  2012 Kansas City   111
#> 5  2005 National      100
#> 6  2010 National       92
#> 7  2011 National       91
#> 8  2012 National       92
```

## The Original Graph

Here's my attempt at reproducing the original.

First I make a dummy x variable that will let me encode both categorical location and year as length along the x-axis. 


```r
plot_df = kc_df %>% 
  mutate(i1 = as.integer(as.factor(location)), 
         i2 = as.integer(as.factor(year))) %>% 
  mutate(dummy_x = i1 + i2 / 4 + ifelse(i1==2, 0.25, 0))
plot_df
#> # A tibble: 8 x 6
#>    year location    score    i1    i2 dummy_x
#>   <int> <chr>       <int> <int> <int>   <dbl>
#> 1  2005 Kansas City   100     1     1    1.25
#> 2  2010 Kansas City   108     1     2    1.5 
#> 3  2011 Kansas City   109     1     3    1.75
#> 4  2012 Kansas City   111     1     4    2   
#> 5  2005 National      100     2     1    2.5 
#> 6  2010 National       92     2     2    2.75
#> 7  2011 National       91     2     3    3   
#> 8  2012 National       92     2     4    3.25
```

Now plot the data with ggplot, fiddling the settings to approximate the original.


```r
long_title = "Overall Composite Customer Satisfaction Index \n for 2005, 2010-11, 2011-12, and 2012-13 \n derived from the mean overall satisfaction rating for the major categories of city services that were \n assessed on the survey (base year 2005 = 100)"

lab_vec = labels=c("2005 Survey", 
                   "2010 to 2011 Survey", 
                   "2011 to 2012 Survey", 
                   "2012 to 2013 Survey")
p = plot_df %>% 
  mutate(ic=as.factor(i2)) %>% 
  ggplot(aes(x=dummy_x, y=score, fill=ic)) + 
  geom_bar(stat='identity',width = 0.25, color='black') + 
  geom_text(aes(label=score), nudge_y = 1.5, color='black') + 
  coord_cartesian(ylim=c(80, 120)) + 
  scale_fill_manual(values = c("orange", "pink", "yellow", "blue"),
                    labels = lab_vec)  +
  scale_x_continuous(breaks = c(1.75, 2.875), 
                     labels = c("Kansas City, MO", "National")) + 
  theme_bw(base_size = 14)  + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 18)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.major.y= element_line(color = 'gray')) + 
  labs(title=long_title, x='', y='') 

print(p)
```

![plot of chunk unnamed-chunk-5](/post/2018-07-07-storytelling-with-data-swd-challenge-your-choice-makover_files/figure-html/unnamed-chunk-5-1.png)

The result isn't a perfect match but is close enough for my purposes. 

## updating the bar graph

As I'll describe below, I don't think a bar graph is the best way to represent this data. However, if we want to stick with bars there are a few updates we could make. 

* shorten the title and avoid redundancy

* remove the redundant text labels above the bars

* separate the location with faceting

* place the individual bar charts nearby so they can be more directly compared

* add a y-axis title



```r
less_long_title = "Composite Customer Satisfaction Index \n compared to 2005 baseline"

lab_vec = labels=c("2005 Survey", 
                   "2010 to 2011 Survey", 
                   "2011 to 2012 Survey", 
                   "2012 to 2013 Survey")
p = plot_df %>% 
  mutate(ic=as.factor(year)) %>% 
  ggplot(aes(x=ic, y=score)) + 
  geom_bar(stat='identity',width = 0.25, color='black') + 
  coord_cartesian(ylim=c(80, 120)) + 
  scale_fill_manual(values = c("orange", "pink", "yellow", "blue"),
                    labels = lab_vec)  +
  theme_bw(base_size = 14)  + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 18)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.major.y= element_line(color = 'gray')) + 
  labs(title=less_long_title, x='Survey Year', y='Index') + 
  facet_wrap(~location, ncol=1)

print(p)
```

![plot of chunk unnamed-chunk-6](/post/2018-07-07-storytelling-with-data-swd-challenge-your-choice-makover_files/figure-html/unnamed-chunk-6-1.png)

An alternative is to use position dodge to put the location side by side


```r
p = plot_df %>% 
  mutate(ic=as.factor(year)) %>% 
  ggplot(aes(x=ic, y=score, color=location, fill=location)) + 
  geom_bar(stat='identity', position = 'dodge') +
  theme_minimal(base_size = 18) + 
    theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 18)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.major.y= element_line(color = 'gray')) + 
  labs(title=less_long_title, x='Survey Year', y='Index') + 
  labs(x="Survey Year", y="Index", title=less_long_title) + 
  coord_cartesian(ylim=c(80, 120)) + 
  scale_fill_brewer(type='qual', palette = 2)

print(p)
```

![plot of chunk unnamed-chunk-7](/post/2018-07-07-storytelling-with-data-swd-challenge-your-choice-makover_files/figure-html/unnamed-chunk-7-1.png)

We should probably start the y-axis at 0 as well


```r
p = plot_df %>% 
  mutate(ic=as.factor(year)) %>% 
  ggplot(aes(x=ic, y=score, color=location, fill=location)) + 
  geom_bar(stat='identity', position = 'dodge') +
  theme_minimal(base_size = 18) + 
    theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 18)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.major.y= element_line(color = 'gray')) + 
  labs(title=less_long_title, x='Survey Year', y='Index') + 
  labs(x="Survey Year", y="Index", title=less_long_title) + 
  coord_cartesian(ylim=c(0, 120)) + 
  scale_fill_brewer(type='qual', palette = 2)

print(p)
```

![plot of chunk unnamed-chunk-8](/post/2018-07-07-storytelling-with-data-swd-challenge-your-choice-makover_files/figure-html/unnamed-chunk-8-1.png)



## remake as a line chart

To go back to what the properties of the data we're working with are:

* a times-series of values

* referenced to an arbitrary baseline

* a two-valued categorical `location` variable

My "best-practices" intuition tells me we should use a line chart, grouped by location, and to subtract the baseline.


```r
lab_df = data.frame(x=2012, y=c(8, -6), location=c('Kansas City', 'National'), 
                    stringsAsFactors = FALSE)

p = kc_df %>% 
  filter(year>2005) %>% 
  ggplot(aes(x=year, y=score-100)) + 
  geom_line(aes(group=location, color=location), size=1.5) + 
  geom_point(aes(group=location, color=location), size=2) + 
  theme_minimal(base_size = 18) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  scale_color_manual(values = c("#1F78B4", "#d95f02")) + 
  labs(x='Year', y=expression(Delta*' Index'), 
       title='Composite Customer Satisfaction Index: \n Change vs 2005') + 
  scale_x_continuous(breaks = c(2010, 2011, 2012)) + 
  theme(legend.position="bottom") + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=18, hjust = 0.5))
print(p)
```

![plot of chunk unnamed-chunk-9](/post/2018-07-07-storytelling-with-data-swd-challenge-your-choice-makover_files/figure-html/unnamed-chunk-9-1.png)

## remake as a table

This data is so simple that I'm not convinced a graph helps to get insight. An additional alternative is to simply use table for the data, for example,


```r
kc_df  %>% spread(year, score) %>% knitr::kable()
```



|location    | 2005| 2010| 2011| 2012|
|:-----------|----:|----:|----:|----:|
|Kansas City |  100|  108|  109|  111|
|National    |  100|   92|   91|   92|
