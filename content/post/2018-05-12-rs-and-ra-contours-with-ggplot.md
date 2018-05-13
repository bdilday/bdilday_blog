---
title: RS and RA contours with ggplot
author: ~
date: '2018-05-12'
slug: rs-and-ra-contours-with-ggplot
categories: [baseball, rstats]
tags: []
---

This post shows how to scrape the current MLB standings and generate a graph of runs scored and runs allowed contours. I will facet the graph according to division, but still compute the contours using all the data. The trick for making this work is to replicate the data, setting a flag for each division, and then using alpha and size aesthetics to hide the non-relevant data.

## Scraping current standings

For scraping current standings there are a variety of choices, e.g., baseball reference, fangraphs, ESPN, MLB, etc. The depth charts page at fangraphs seems to be the most straight forward way of getting both the records and the runs scored and allowed values. <https://www.fangraphs.com/depthcharts.aspx?position=Standings>

First I need to load some libraries 

``` {r}
library(rvest)
library(dplyr)
library(ggplot2)
```

Use `xml2::read_html` to grab the webpage.

``` {r}
url = "https://www.fangraphs.com/depthcharts.aspx?position=Standings"
h = xml2::read_html(url)
```

Inspection of the payload shows that there are 9 tables, and the ones we want for the division standings are numbers 4 through 9.

``` {r}
tables = rvest::html_table(h)[4:9]
```

The tables are in the order AL East, AL Central, AL West, NL East, NL Central, NL West. I could try reading the table headers to figure this out, but for now I'm just hard-coding it. The labeling will break if fangraphs ever decides to switch the order.

``` {r}
div_order = paste(rep(c("AL", "NL"), each=3), c("E", "C", "W"), sep='-')
print(div_order)
[1] "AL-E" "AL-C" "AL-W" "NL-E" "NL-C" "NL-W"
```

The format of a table is as follows. A couple things to note are that the column names are arbitrary and that the first two rows give the html table headers. Also the column names contained in the second row repeat values, i.e. `W` is given three times, once for current standings, once for rest of season estimate, and once for final projected standings. These need to be renamed if you want to use `dplyr::bind_rows`, otherwise you'll get just one of the three sets. However, for my purposes here I just want the current standings.

```
tables[[1]]

         X1                X2                X3                X4                X5                X6
1           2018 Year to Date 2018 Year to Date 2018 Year to Date 2018 Year to Date 2018 Year to Date
2      Team                 G                 W                 L                W%              RDif
3   Yankees                38                26                12              .684                60
4   Red Sox                38                26                12              .684                62
5 Blue Jays                39                21                18              .539                10
6      Rays                35                15                20              .429               -11
7   Orioles                38                11                27              .290               -68
                 X7                X8                            X9                           X10
1 2018 Year to Date 2018 Year to Date 2018 Projected Rest of Season 2018 Projected Rest of Season
2              RS/G              RA/G                             G                             W
3              5.74              4.16                           124                            73
4              5.47              3.84                           124                            73
5              4.95              4.69                           123                            64
6              4.23              4.54                           127                            61
7              3.87              5.66                           124                            58
                            X11                           X12                           X13
1 2018 Projected Rest of Season 2018 Projected Rest of Season 2018 Projected Rest of Season
2                             L                            W%                          RDif
3                            51                          .589                           108
4                            51                          .586                           104
5                            59                          .516                            20
6                            66                          .478                           -26
7                            66                          .466                           -42
                            X14                           X15                        X16
1 2018 Projected Rest of Season 2018 Projected Rest of Season 2018 Projected Full Season
2                          RS/G                          RA/G                          W
3                          5.02                          4.15                         99
4                          5.00                          4.16                         99
5                          4.74                          4.58                         85
6                          4.06                          4.26                         76
7                          4.57                          4.90                         69
                         X17                        X18                        X19                        X20
1 2018 Projected Full Season 2018 Projected Full Season 2018 Projected Full Season 2018 Projected Full Season
2                          L                         W%                       RDif                       RS/G
3                         63                       .611                        168                       5.19
4                         63                       .609                        166                       5.11
5                         77                       .522                         30                       4.79
6                         86                       .467                        -37                       4.09
7                         93                       .425                       -110                       4.40
                         X21
1 2018 Projected Full Season
2                       RA/G
3                       4.15
4                       4.08
5                       4.61
6                       4.32
7                       5.08
```

This `lapply` loop will set the table names, keep only the current standings, set a division label, and pipe the results to `dplyr::bind_rows` to generate a data frame.

```
current_standings = lapply(1:6, function(i) {
  tmp = tables[[i]]
  names(tmp) = tmp[2,]
  tmp = tmp[3:nrow(tmp),1:8] # only current standings + team name, not projections
  tmp$div_id = div_order[[i]]
  tmp
}) %>% dplyr::bind_rows()
```

This code removes weird characters from the column names and converts the character data to integer and numeric, as appropriate.

```
nms = names(current_standings)
nms = stringr::str_replace(nms, "%", "pct")
nms = stringr::str_replace(nms, "/", "_")
names(current_standings) = nms
  
for (nm in c("G", "W", "L", "RDif")) {
  current_standings[[nm]] = as.integer(current_standings[[nm]])
}

for (nm in c("Wpct", "RS_G", "RA_G")) {
  current_standings[[nm]] = as.numeric(current_standings[[nm]])
}
```

Here is the format of the final result

```
head(current_standings)
       Team  G  W  L  Wpct RDif RS_G RA_G div_id
1   Yankees 38 26 12 0.684   60 5.74 4.16   AL-E
2   Red Sox 38 26 12 0.684   62 5.47 3.84   AL-E
3 Blue Jays 39 21 18 0.539   10 4.95 4.69   AL-E
4      Rays 35 15 20 0.429  -11 4.23 4.54   AL-E
5   Orioles 38 11 27 0.290  -68 3.87 5.66   AL-E
6   Indians 37 18 19 0.487    7 4.68 4.49   AL-C
```

## Contour plots

This will plot the data, adding density contours.

```
current_standings %>% 
 ggplot(aes(x=RS_G, y=RA_G)) + geom_point() + 
 geom_text_repel(aes(label=Team)) + 
 theme_minimal(base_size = 16) + 
 stat_density2d(color='gray') + 
 xlim(2.5, 6.5) + ylim(6.5, 2.5) + labs(x="RS / G", y="RA / G")
```

![](/post/img/rs_ra1.png)



### Faceting by division - wrong approach

I would like to see the results facted by division. If you simply apply the facet in `ggplot`, however.

```
current_standings %>% 
 ggplot(aes(x=RS_G, y=RA_G)) + geom_point() + 
 geom_text_repel(aes(label=Team)) + 
 theme_minimal(base_size = 16) + 
 stat_density2d(color='gray') + 
 xlim(2.5, 6.5) + ylim(6.5, 2.5) + labs(x="RS / G", y="RA / G") + 
 facet_wrap(~div_id)
```

![](/post/img/rs_ra2.png)


### Faceting by division - better approach

Since I want the same contour shown on each panel, I replicate the data, and set a division flag, so I can have a way of knowing which data I want to display and which I want to hide.

```
nr = nrow(current_standings)
plot_df = lapply(1:6, function(i) {
tmp = current_standings; ii = rep(0, nr); 
i1 = (i-1)*5+1; i2=i1+4; 
 ii[i1:i2] = 1; tmp$isdiv = ii; tmp$idx = i; tmp
 }) %>% dplyr::bind_rows()
```

With this code the contours get computed the same way for each panel, but we can hide the non-relevant data, by using `alpha` as an aesthetic for the points and `size` for the text.

```
plot_df %>% 
 ggplot(aes(x=RS_G, y=RA_G)) + 
 stat_density2d(color='gray') + 
 theme_minimal(base_size = 16) + 
 facet_wrap(~idx) + 
 geom_point(aes(alpha=isdiv)) + 
 geom_text_repel(aes(alpha=isdiv, label=Team, size=as.factor(isdiv))) + 
 ylim(6.5, 2.5) + xlim(2.5, 6.5) + 
 labs(x="RS / G", y="RA / G") + 
 guides(alpha=FALSE, size=FALSE) + 
 scale_size_manual(values=c(0,5))
```

![](/post/img/rs_ra3.png)
