---
title: Facet Wrap with Limit in the Grammar of Graphics?
author: ~
date: '2018-07-29'
slug: facet-wrap-with-limit
categories: []
tags: []
---



  
I have a case where I want to facet the data, using a subset of the groups. This post looks at how one might do this in `ggplot2` and asks how it fits into the grammar of graphics. All work on defining custom stats, geoms, and faceting is based on the expositions in the [`ggplot2` vignette on extending ggplot ](https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html) as well as the chapter on ggplot internals from [Bob Rudis](https://rud.is/books/creating-ggplot2-extensions/demystifying-ggplot2.html)

## Motivations

The purpose behind this faceting of a subset of the data is to have an interactive / rapid-feedback way of visualizing stand-out cases when your data has lots of groups. I can think of a few slightly different use cases. 

1. You want to plot a random sample of the groups

2. You want to plot a limited sample, ordered by some metric

3. You want to plot a limited sample, ordered by an attribute of the plot

Below I'll go through each case in turn. It's the third case I'm most interested in here - the first two can be accomplished by pre-processing the data, but the third is more challenging.

## Load the libraries


```r
library(ggplot2)
library(dplyr)
# set the global theme
ggplot2::theme_set(theme_minimal(base_size = 16))
```


## Data

To illustrate, I'll generate some simulated data. The data are one-dimensional and uniformly distributed between 0 and 1. The number of cases for each group will vary according to a Poisson distribution. To illustrate the issue, the number of groups needs to be fairly large - here I'll use 64.


```r
set.seed(101)
npts_mean = 100 # mean of the Poission distribution
ngroups = 64

df1 = dplyr::bind_rows(
  lapply(1:ngroups, function(i) {
    nsize = rpois(1, npts_mean)
    data.frame(x=rnorm(nsize), g=sprintf("g%02d", i), stringsAsFactors = FALSE)
  })
)

df1$g = factor(df1$g)
```

As a bit of metadata analysis, let's look at how the distribution of n-size came out.


```r
df1 %>% 
  group_by(g) %>% 
  summarise(n=n()) %>%
  ungroup() %>% 
  ggplot(aes(x=n)) + 
  geom_histogram()
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-4](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-4-1.png)

A perfect Poisson distribution!

## All the groups

The crux of the issue with plotting this data is that there are so many groups, it's difficult to fit them all and make sense of it on a single chart. Here's what it looks like if we try,


```r
df1 %>% 
  ggplot(aes(x)) + 
  geom_histogram() + 
  facet_wrap(~g) + theme_minimal(base_size = 8)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-5](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-5-1.png)

Of course 64 isn't *that large* of a number of facets. But it illustrates the point without bogging down the rendering time - one can imagine the case with many more groups, say 1000 or more

## Case 1: random facets

For the first case, we can pre-process the data by choosing random groups


```r
facet_wrap_limit_1 = function(df1, rseed = 101, nrandom = 4) {
  set.seed(rseed)
  all_groups = unique(as.character(df1$g))
  subset_groups = sample(all_groups, min(nrandom, length(all_groups)))
  
  df1 %>% 
    filter(g %in% subset_groups) %>%
    ggplot(aes(x=x)) + geom_histogram() + 
    facet_wrap(~g)
}
```

Now I can get a random subset


```r
p = facet_wrap_limit_1(df1, rseed = 101, nrandom = 4)
print(p)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-7](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-7-1.png)

and vary it by changing the random seed


```r
p = facet_wrap_limit_1(df1, rseed = 102, nrandom = 4)
print(p)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-8](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-8-1.png)

## Case 2: 

Case 2 is similar except that the groups are ordered according to some attribute of the data. In this example I'll use the n-size of each group. I append the n-size to the group name so we can easily check the ordering went correctly by looking at the facet titles. Alternatively I could annotate the plot with `ggplot2::geom_text` or `ggplot2::annotate_custom` or maybe `annotate_textp` from the `ggalt` package. 


```r
facet_wrap_limit_2 = function(df1, nrandom = 4, asc = FALSE) {
  all_groups = unique(as.character(df1$g))
      
  # compute and append rank of nsize 
  ranked_data = df1 %>% 
    group_by(g) %>% 
    summarise(nsize=n()) %>% 
    mutate(r=rank(nsize, ties.method = 'first')) %>% 
    ungroup() %>% 
    inner_join(df1, by="g") %>% 
    mutate(g = paste(g, nsize, sep="_"))
  
  # grab the first nrandom
  if (asc) {
    plot_df = ranked_data %>% 
      filter(r <= nrandom)
  } else {
    plot_df = ranked_data %>% 
      filter(r >= (length(all_groups) - nrandom + 1))
  }
  
  plot_df %>% 
    ggplot(aes(x=x)) + geom_histogram() +
    facet_wrap(~g)
}
```

Now I can order by n-size, either inversely, or in ascending order


```r
p = facet_wrap_limit_2(df1, nrandom = 4, asc=FALSE)
print(p)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-10](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-10-1.png)


```r
p = facet_wrap_limit_2(df1, nrandom = 4, asc=TRUE)
print(p)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-11](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-11-1.png)


## Case 2: an alternate approach

An alternate approach would be to filter the data within the data transformation part of the geom. To do this I create a new geom that behaves like `geom_histogram` but explicitly filters the data. The definition of `geom_histogram` is as follows



```r
print(args(geom_histogram))
#> function (mapping = NULL, data = NULL, stat = "bin", position = "stack", 
#>     ..., binwidth = NULL, bins = NULL, na.rm = FALSE, show.legend = NA, 
#>     inherit.aes = TRUE) 
#> NULL
print(body(geom_histogram))
#> {
#>     layer(data = data, mapping = mapping, stat = stat, geom = GeomBar, 
#>         position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
#>         params = list(binwidth = binwidth, bins = bins, na.rm = na.rm, 
#>             pad = FALSE, ...))
#> }
```

That is, it functions by applying `GeomBar` to a stat of `bin`. To implement a filter we can override `GeomBar` or `StatBin`. In this case I'll override the stat, in the following way.


```r
StatBinFilter <- ggproto("StatBinFilter", StatBin, 
  
  setup_data = function(data, params) {
    all_groups = as.vector(unique(data$group))
    nfilter = min(params$nfilter, length(all_groups))
  
    # take only the first nfilter groups  
    idx = which(data$group %in% all_groups[1:nfilter])
    
    # take a subset of the data and pass it on to the rendering methods
    data[idx,]
  },

  extra_params = c("na.rm", "nfilter")
    
)

stat_bin_filter <- 
  function (mapping = NULL, data = NULL, geom = "bar", position = "stack", 
            ..., binwidth = NULL, bins = NULL, center = NULL, boundary = NULL, 
            breaks = NULL, closed = c("right", "left"), pad = FALSE, 
            na.rm = FALSE, nfilter=NULL, show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = StatBinFilter, geom = geom, 
          position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
          params = list(binwidth = binwidth, bins = bins, center = center, 
                        boundary = boundary, breaks = breaks, closed = closed, 
                        pad = pad, na.rm = na.rm, nfilter = nfilter, ...))
  }
```


Now I can filter the number of groups to show


```r
df1 %>% 
  ggplot(aes(x=x)) + 
  stat_bin_filter(aes(group=g, fill=g), 
                  position='dodge', nfilter = 2)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-14](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-14-1.png)



```r
df1 %>% 
  ggplot(aes(x=x)) + 
  stat_bin_filter(aes(group=g, fill=g), 
                  position='dodge', nfilter = 3)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-15](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-15-1.png)

However, if I try to facet, I get a whole bunch of empty ones - the statistical transformation of filtering hasn't propagated.


```r
df1 %>% 
  ggplot(aes(x=x)) + 
  stat_bin_filter(aes(group=g, fill=g), 
                  nfilter = 2) + 
  facet_wrap(~g) + 
  theme_minimal(base_size = 8)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-16](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-16-1.png)

## Case 3: plot a limited sample, ordered by an attribute of the plot

An additional complication is if we want to order and filter by an attribute of the plot - for example, let's say we order by the largest number in any one bin. As we vary the binning this ordering will change. 

Following is an attempt to filter the data based on the highest bar - in `compute_panel` I generate the histograms 


```r
StatBinFilter <- ggproto("StatBinFilter", StatBin, 
  
  setup_data = function(data, params) {
    data
  },

  compute_panel = function(data, scales, nfilter=NULL, ...) {
  
    all_groups = as.vector(unique(data$group))
    nfilter = min(nfilter, length(all_groups))
    
    tmp = StatBin$compute_panel(data, scales, ...)  
    tmp2 = tmp %>% dplyr::group_by(group) %>% 
      mutate(r=rank(-count, ties.method = "first")) %>% 
      filter(r <= nfilter)
    tmp2
    },
  
  
  
  extra_params = c("na.rm", "nfilter")
    
)

stat_bin_filter <- 
  function (mapping = NULL, data = NULL, geom = "bar", position = "stack", 
            ..., binwidth = NULL, bins = NULL, center = NULL, boundary = NULL, 
            breaks = NULL, closed = c("right", "left"), pad = FALSE, 
            na.rm = FALSE, nfilter=NULL, show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = StatBinFilter, geom = geom, 
          position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
          params = list(binwidth = binwidth, bins = bins, center = center, 
                        boundary = boundary, breaks = breaks, closed = closed, 
                        pad = pad, na.rm = na.rm, nfilter = nfilter, ...))
  }
```


```r
df1 %>% ggplot(aes(x=x)) + stat_bin_filter(aes(group=g), nfilter = 2) + facet_wrap(~g) + theme_minimal(base_size = 8)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-18](/post/2018-07-29-facet-wrap-with-limit_files/figure-html/unnamed-chunk-18-1.png)

Evidently what happened here is it's calling `compute_panel` for each panel in the facted plot - on the other hand I can't apply my filtering based on the highest bar unless I know the scales and the binning parameters - and what I'm trying to avoid is doing that computation in two places. 

## Summary

This is a bit of a WIP on issues surrounding faceting and the grammar of graphics. In summary, filtering the data is a valid statistical transformation. Faceting based on the filtered data seems valid in the grammar of graphics to me, but the technical implementation in `ggplot2` is challenging, due to the ordering of operations. In fairness, I have only started scratching the surface of `ggplot2` internals and it could be the capability is there and I've just missed it. I look forward to continuing this in subsequent studies and posts.




