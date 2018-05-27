---
title: Graphical MLB Standings
author: ~
date: '2018-05-25'
slug: graphical-mlb-standings
categories: []
tags: []
---


This post extends the previous one on runs-scored and runs-allowed contours to show a prototype for a graphical standings table. When I look at a standings table what I want to see is 

* wins and losses

* winning percentage - how many wins does this prorate to at 162 G?

* runs scored and allowed

    * are they beating their Pythagorean record? lagging it?
    
    * are they defense first or offense first?

The goal of the graphical standings table I create here is to give an intuitive representation of this information.

First, load some libraries

``` {r}
library(dplyr)
library(ggplot2)
library(ggrepel)
library(rvest)
library(stringr)
```

As before, we can grab current standings from FanGraphs. 

``` 
fg_current_standings <- function() {
    url = "https://www.fangraphs.com/depthcharts.aspx?position=Standings"
    h = xml2::read_html(url)
    tables = html_table(h)[4:9]
    
    div_order = paste(rep(c("AL", "NL"), each=3), c("E", "C", "W"), sep='-')
    current_standings = lapply(1:6, function(i) {
        tmp = tables[[i]]
        names(tmp) = tmp[2,]
        tmp = tmp[3:nrow(tmp),1:8] # only current standings + team name, not projections
        tmp$div_id = div_order[[i]]
        tmp
    }) %>% dplyr::bind_rows()
    
    nms = names(current_standings)
    nms = str_replace(nms, "%", "pct")
    nms = str_replace(nms, "/", "_")
    names(current_standings) = nms
    
    for (nm in c("G", "W", "L", "RDif")) {
        current_standings[[nm]] = as.integer(current_standings[[nm]])
    }
    
    for (nm in c("Wpct", "RS_G", "RA_G")) {
        current_standings[[nm]] = as.numeric(current_standings[[nm]])
    }
    
    current_standings
}

current_standings = fg_current_standings()
```

## Contours of constant pythagorean win-percentage

In the previous RS / RA graphs, I used density to give context to the values. A more useful and meaningful overlay is contours of constant Pythagorean win percentage (thanks to [Jared Cross](https://twitter.com/steamerpro) for the suggestion). To determine what those curves look like, note that the Pythagorean win-percentage is 


$$w = \frac {{RS^2}} {{RS^2+RA^2}}$$.

We want to cast this as set of curves in the `$RA$` - `$RS$` plane, parameterized by `$w$`, i.e. `$RA = f(RS~|~w)$`. Using some algebra to solve for `$RA$` as a function of `$RS$` leads to 

$$RA = \sqrt{{\frac{{w}}{{1-w}}}} ~RS$$


Here I create a data frame to hold the start and end points of the curves. 

```
# 36 to 126 per 162 games in steps of 10
wseq = seq(81-45, 81+45, 10)  / 162
slopes = sqrt((1-wseq)/wseq)

dfC = data.frame(s=slopes)
dfC$x = 2.5
dfC$y = 2.5 * slopes
dfC$xend = 6.5
dfC$yend = 6.5 * slopes
```

Because the limits of the plot are 2.5 to 6.5, some special handling is needed to make sure the lines stay within those bounds. Specifically, the end points of the line will be set depending on whether the slope is bigger than 1 or less than 1. Note that this is an issue because `geom_segment` in `ggplot2` will remove a line altogether if any part of the segment is outside the window set by `xlim` and `ylim`.


```
cc = which(dfC$y < 2.5)
dfC[cc,]$y = 2.5
dfC[cc,]$x = 2.5 / slopes[cc]

cc = which(dfC$yend > 6.5)
dfC[cc,]$yend = 6.5
dfC[cc,]$xend = 6.5 / slopes[cc]
```

The `wlabs` data frame will be used for labeling the contours, according to the number of wins the corresponding Pythagorean win percentage would be over 162 G.

```
wlabs = data.frame(w=wseq*162)
wlabs$x = 3
wlabs$y = 3 * slopes
cc = which(wlabs$y < 2.5)
wlabs[cc,]$x = 6
wlabs[cc,]$y = 6 * slopes[cc]

```


```
rs1 = 0.5 * (mean(current_standings$RS_G) + mean(current_standings$RS_G))

p = current_standings %>% 
  ggplot(aes(x=RS_G, y=RA_G)) + geom_point() + 
  geom_text_repel(aes(label=Team)) + 
  theme_minimal(base_size = 16) + 
  xlim(2.49, 6.51) + ylim(6.51, 2.49) + 
  labs(x="RS / G", y="RA / G") + 
  geom_segment(data=dfC, aes(x=x, y=y, xend=xend, yend=yend), alpha=0.5) + 
  theme(panel.grid = element_blank()) +
  geom_vline(xintercept=rs1) + geom_hline(yintercept=rs1) +
  facet_wrap(~div_id) + geom_text(data=wlabs, aes(x=x, y=y-0.1, label=w), alpha=0.5)
```

Interestingly this illustrates the asymmetry between `$RA$` and `$RS$` in the Pythagorean record - because the slope of the lines (for both the top-right and bottom-left quadrants) goes away from the point where runs scored and allowed are both average (the "origin" in the below graphs), we can see that - at a fixed Pythagorean winning percentage - the distance from the y-axis when a contour crosses the x-axis is larger than the distance from the x-axis when it crosses the y-axis. The interpretation of this is that the runs-scored-per-game-above-average is higher than the runs-scored-below-average for the same Pythagorean winning percentage.

![](/post/img/graphical_standings1.png)

## Actual record

So this shows Pythagorean win percentage, on the scale of 162 games. It doesn't show, however, the actual record, or whether the teams are beating or lagging their Pythagorean record. To show this, I will find a point in the RS - RA plane such that the Pythagorean record corresponds to their actual record. There is not just a single point that satisfies this, but a curve. To choose a point on the curve, I will draw a line from the Pytagorean wins contour to the "actual wins" contour that's always orthogonal to the Pythagorean winning percentage contours. 

## The orthogonal curve

To find the curve, note that the slope of the constant Pythagorean win percentage contour is, 

$$ \frac{{dy}}{{dx}} = \frac {{y}} {{x}} $$.

A line segment that is orthogonal therefore has the slope, 

$$ \frac{{dy}}{{dx}} = \frac {{-x}} {{y}} $$,

and the curve this describes satisfies,

$$ \int y' ~dy' = - \int x' ~dx'$$

where the limits of the integration are `$y_0$` to `$y$` and `$x_0$` to `$x$`, respectively (I'm having problems rendering the limits with mathjax and blogdown so I'm just stating what they are). The integration leads to,

$$y^2 -y_0^2 = -x^2 + x_0^2$$
$$x^2 + y^2 = x_0^2 + y_0^2 $$

or in other words the curves that are everywhere orthogonal to radial lines are circles. The curve we draw will extend until we reach a point where the Pythagorean winning percentage matches the actual winning percentage.

$$ 
w = \frac{{x^2}}{{x^2 + y^2}} \\
= \frac{{x^2}}{{x_0^2 + y_0^2}}
$$

We can simplify this by noting that 

$$ \frac{{w}}{{\tilde w}} = \frac{{x^2}}{{x^2 + y^2}}\frac{{x_0^2 + y_0^2}}{{x_0^2}} = \frac{{x^2}}{{x_0^2}}$$
$$x^2 = x_0^2 \frac{{w}}{{\tilde w}} $$
$$x = x_0 ~\sqrt{{ \frac{{w}} {{\tilde w}} }}$$

where `$\tilde w$` denotes the Pythagorean winning percentage. To draw the arcs I will generate the x and y values over the appropriate range and then apply `geom_path` from `ggplot2`. The package `ggforce` has a `geom_arc` function but it doesn't appear to work with an inverted y-scale as I'm using here. The names for each team are color-coded, red for lucky (i.e. winning percentage greater than Pythagorean winning percentage), blue for unlucky (visa-versa).


```
# compute the end points for the curve
current_standings = current_standings %>% 
 mutate(wpythag = RS_G**2/(RS_G**2 + RA_G**2), 
        x0 = RS_G * sqrt(Wpct/wpythag), 
        y0 = x0 * sqrt((1-Wpct)/Wpct))

# this function generates 100 points along a curve given by the parameters
parametric_curve = function(x0, y0, xend, yend) {
  dx_seq = seq(0, 1, 0.01)
  lapply(dx_seq, function(dx) {
  tx = x0 + (xend-x0)*dx
  ty = sqrt(x0**2 + y0**2 - tx**2) 
   list(x=tx, y=ty)
  }) %>% bind_rows()
}

# generate the curves for each team
arcs = lapply(1:nrow(current_standings), function(idx) {
 row = current_standings[idx,]
 arc = parametric_curve(row$RS_G, row$RA_G, row$x0, row$y0)
 arc$Team = row$Team
 arc$div_id = row$div_id
 arc$lucky = as.integer(row$x0 > row$RS_G)
 arc
}) %>% bind_rows()


p = current_standings %>% 
  ggplot(aes(x=RS_G, y=RA_G)) + geom_point() + 
  geom_text_repel(aes(x=x0, y=y0, label=Team, color=as.factor(Wpct > wpythag))) + 
  theme_minimal(base_size = 16) + 
  xlim(2.49, 6.51) + ylim(6.51, 2.49) + 
  labs(x="RS / G", y="RA / G") + 
  geom_segment(data=dfC, aes(x=x, y=y, xend=xend, yend=yend), alpha=0.5) + 
  theme(panel.grid = element_blank()) +
  geom_vline(xintercept=rs1) + geom_hline(yintercept=rs1) +
  facet_wrap(~div_id) + geom_text(data=wlabs, 
                                  aes(x=x, y=y-0.1, label=w), 
                                  alpha=0.5) + 
  geom_path(data=arcs, aes(x=x, y=y, group=Team)) + 
  scale_color_manual(values=c("steelblue", "red"), guide="none")

```


![](/post/img/graphical_standings2.png)
