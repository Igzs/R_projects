---
title       : "Pitch Presentation"
subtitle    : "Presenting our R application"
author      : "Arthur LAUREAU - Igor FIDALGO"
job         : 
date        : "November 28, 2019"
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---
<style>
.title-slide {
  background-color: #EDE0CF; /* #EDE0CF; ; #CA9F9D*/
}
</style>
## 1. Introduction

The Shiny Application project:

- This project is a simple hello world project to learn how to make a **Shiny app** and a related **presentation** using **RStudio**.
- To learn more, see [our github](https://github.com/Igzs/R_shiny_pres).  

---

## 2. Dataset summary

The UCBAdmissions data set is frequently used for illustrating Simpson's paradox, see [Bickel et al (1975)](http://www.jstor.org/stable/1739581). At issue is whether the data show evidence of sex bias in admission practices.  

The UCBAdmissions data set is 3-dimensional array resulting from cross-tabulating 4526 observations on 3 variables. The variables and their levels are as follows: 

| No   |  Name  |       Levels       |
|:----:|:------:|:------------------:|
|   1  |  Admit | Admitted, Rejected |
|   2  | Gender |    Male, Female    |
|   2  |  Dept  |     A,B,C,D,E,F    |
  
  

```r
summary(UCBAdmissions)
```

```
## Number of cases in table: 4526 
## Number of factors: 3 
## Test for independence of all factors:
## 	Chisq = 2000.3, df = 16, p-value = 0
```

---

## 3. Functionnalities
* Navbar architecture
  + Documentation included in the app
  + Overview and details tabs
* Interactive plots
  + Mosaic plot and pie charts reactive to different inputs
* Compare results
  + Highlight Simpson's Paradox

---
## 4. Example


```r
library(ggplot2)
library(ggmosaic)
ggplot(as.data.frame(UCBAdmissions)) +
          geom_mosaic(aes(weight= Freq, x = product(Dept,Admit), fill=Admit)) + 
          facet_grid(Gender~.) + 
          scale_fill_manual(values=c("#56B4E9", "#D46A6A"))
```

![plot of chunk unnamed-chunk-1](assets/fig/unnamed-chunk-1-1.png)
