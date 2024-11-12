---
title: "Problem Set 5"
author: "Tiffany Wu"
format: 
  html:
    code-fold: true
    code-summary: "Show the code"
    embed-resources: true
output: html_document
editor: source
mainfont: Times New Roman
---



## Github Repo Link to Problem Set 5:
[https://github.com/tiffany-wu/Stats506/tree/main/PS5](https://github.com/tiffany-wu/Stats506/tree/main/PS5) 

## Problem 1 - OOP Programming

Create a class to represent rational numbers. Do this using S4.

### a. For the rational class, define the following:

1. A constructor
2.A validator that ensures the denominator is non-zero.
3.A show method.
4.A simplify method, to obtain the simplest form (e.g. simplify(2/4) produces 1/2).
5.A quotient method (e.g. quotient(3/7) produces .42857143...). It should support a digits argument but only in the printing, not the returned result (Hint: what does print return?).
6.Addition, subtraction, multiplication, division. These should all return a rational.
*7.You’ll (probably) need GCD and LCM as part of some of these calculations; include these functions using Rcpp. Even if you don’t need these functions for another calculation, include them.*


::: {.cell}

```{.r .cell-code}
rm(list=ls()) 
```
:::

::: {.cell}

```{.r .cell-code}
# Load Rcpp for GCD and LCM functions
library(Rcpp)

# Define GCD and LCM functions in a single C++ code block
cppFunction("
#include <cmath>       // Include cmath for abs function
#include <numeric>     // Include numeric for std::gcd and std::lcm functions

int C_gcd(int x, int y) {
  return std::gcd(x, y);
}")

cppFunction("
int C_lcm(int x, int y) {
  return (abs(x * y) / std::gcd(x, y));
}
")
```
:::

::: {.cell}

```{.r .cell-code}
# Set up rational class

# Define the S4 class
setClass("Rational",
         slots = c(numerator = "numeric", denominator = "numeric"),
         prototype = list(numerator = 0, denominator = 1)
)

# Validity
setValidity("Rational", function(object) {
  if (object@denominator == 0) {
    return("Denominator cannot be zero.")
  }
  TRUE
})
```

::: {.cell-output .cell-output-stdout}

```
Class "Rational" [in ".GlobalEnv"]

Slots:
                              
Name:    numerator denominator
Class:     numeric     numeric
```


:::

```{.r .cell-code}
# Simplify method-- this is just for internal use within constructor to obtain the simplest form
simplify_fraction <- function(numerator, denominator) {
  divisor <- C_gcd(numerator, denominator)
  numerator <- numerator / divisor
  denominator <- denominator / divisor
  list(numerator = numerator, denominator = denominator)
}

# Constructor
Rational <- function(numerator, denominator = 1) {
  if (denominator == 0) {
    stop("Error: Denominator cannot be zero.")
  }
  # Simplify the fraction upon creation
  simplified <- simplify_fraction(numerator, denominator)
  obj <- new("Rational", numerator = simplified$numerator, denominator = simplified$denominator)
  obj  # Return the valid object
}

# Show method
setMethod("show", "Rational", function(object) {
  cat(object@numerator, "/", object@denominator, "\n")
})

# Simplify method for external use
setGeneric("simplify", function(object) standardGeneric("simplify"))
```

::: {.cell-output .cell-output-stdout}

```
[1] "simplify"
```


:::

```{.r .cell-code}
setMethod("simplify", "Rational", function(object) {
  simplified <- simplify_fraction(object@numerator, object@denominator)
  new("Rational", numerator = simplified$numerator, denominator = simplified$denominator)
})

# Quotient
setGeneric("quotient", function(object, digits = NULL) standardGeneric("quotient"))
```

::: {.cell-output .cell-output-stdout}

```
[1] "quotient"
```


:::

```{.r .cell-code}
setMethod("quotient", "Rational", function(object, digits = NULL) {
  result <- object@numerator / object@denominator
  if (!is.null(digits) && is.numeric(digits)) {
    print(round(result, digits))
  } else {
    print(result)
  }
  invisible(result)
})

# Addition
setMethod("+", signature(e1 = "Rational", e2 = "Rational"), function(e1, e2) {
  lcm_den <- C_lcm(e1@denominator, e2@denominator)
  numerator <- e1@numerator * (lcm_den / e1@denominator) + e2@numerator * (lcm_den / e2@denominator)
  Rational(numerator, lcm_den)  # Automatically simplifies
})

# Subtraction
setMethod("-", signature(e1 = "Rational", e2 = "Rational"), function(e1, e2) {
  lcm_den <- C_lcm(e1@denominator, e2@denominator)
  numerator <- e1@numerator * (lcm_den / e1@denominator) - e2@numerator * (lcm_den / e2@denominator)
  Rational(numerator, lcm_den)  # Automatically simplifies
})

# Multiplication
setMethod("*", signature(e1 = "Rational", e2 = "Rational"), function(e1, e2) {
  Rational(e1@numerator * e2@numerator, e1@denominator * e2@denominator)  # Automatically simplifies
})

# Division
setMethod("/", signature(e1 = "Rational", e2 = "Rational"), function(e1, e2) {
  if (e2@numerator == 0) stop("Cannot divide by a rational number with zero numerator.")
  Rational(e1@numerator * e2@denominator, e1@denominator * e2@numerator)  # Automatically simplifies
})
```
:::




### b. Use your rational class to create three objects:

r1: 24/6
 
r2: 7/230
 
r3: 0/4



::: {.cell}

```{.r .cell-code}
# Creating objects
r1 <- Rational(24, 6)    # This should simplify to 4/1
r2 <- Rational(7, 230)   # This will stay as 7/230
r3 <- Rational(0, 4)     # This should simplify to 0/1

r1 # 4/1
```

::: {.cell-output .cell-output-stdout}

```
4 / 1 
```


:::

```{.r .cell-code}
r2 # 7/230
```

::: {.cell-output .cell-output-stdout}

```
7 / 230 
```


:::

```{.r .cell-code}
r3 #0/1
```

::: {.cell-output .cell-output-stdout}

```
0 / 1 
```


:::
:::



 
Evaluate the following code (remember you can tell Quarto not to stop on errors):



::: {.cell}

```{.r .cell-code}
r1
```

::: {.cell-output .cell-output-stdout}

```
4 / 1 
```


:::

```{.r .cell-code}
r3
```

::: {.cell-output .cell-output-stdout}

```
0 / 1 
```


:::

```{.r .cell-code}
r1 + r2
```

::: {.cell-output .cell-output-stdout}

```
927 / 230 
```


:::

```{.r .cell-code}
r1 - r2
```

::: {.cell-output .cell-output-stdout}

```
913 / 230 
```


:::

```{.r .cell-code}
r1 * r2
```

::: {.cell-output .cell-output-stdout}

```
14 / 115 
```


:::

```{.r .cell-code}
r1 / r2
```

::: {.cell-output .cell-output-stdout}

```
920 / 7 
```


:::

```{.r .cell-code}
r1 + r3
```

::: {.cell-output .cell-output-stdout}

```
4 / 1 
```


:::

```{.r .cell-code}
r1 * r3
```

::: {.cell-output .cell-output-stdout}

```
0 / 1 
```


:::

```{.r .cell-code}
r2 / r3
```

::: {.cell-output .cell-output-error}

```
Error in r2/r3: Cannot divide by a rational number with zero numerator.
```


:::

```{.r .cell-code}
quotient(r1)
```

::: {.cell-output .cell-output-stdout}

```
[1] 4
```


:::

```{.r .cell-code}
quotient(r2)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.03043478
```


:::

```{.r .cell-code}
quotient(r2, digits = 3)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.03
```


:::

```{.r .cell-code}
quotient(r2, digits = 3.14)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.03
```


:::

```{.r .cell-code}
quotient(r2, digits = "avocado")
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.03043478
```


:::

```{.r .cell-code}
q2 <- quotient(r2, digits = 3)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.03
```


:::

```{.r .cell-code}
q2
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.03043478
```


:::

```{.r .cell-code}
quotient(r3)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0
```


:::

```{.r .cell-code}
simplify(r1)
```

::: {.cell-output .cell-output-stdout}

```
4 / 1 
```


:::

```{.r .cell-code}
simplify(r2)
```

::: {.cell-output .cell-output-stdout}

```
7 / 230 
```


:::

```{.r .cell-code}
simplify(r3)
```

::: {.cell-output .cell-output-stdout}

```
0 / 1 
```


:::
:::



Show that your validator does not allow the creation of rational’s with 0 denominator, and check other malformed input to your constructor.


::: {.cell}

```{.r .cell-code}
# Testing the validator for Rational objects with invalid inputs -- should all error
Rational(1, 0)
```

::: {.cell-output .cell-output-error}

```
Error in Rational(1, 0): Error: Denominator cannot be zero.
```


:::

```{.r .cell-code}
Rational("a", 2)
```

::: {.cell-output .cell-output-error}

```
Error in eval(expr, envir, enclos): Not compatible with requested type: [type=character; target=integer].
```


:::

```{.r .cell-code}
Rational(1, "b")
```

::: {.cell-output .cell-output-error}

```
Error in eval(expr, envir, enclos): Not compatible with requested type: [type=character; target=integer].
```


:::

```{.r .cell-code}
Rational("a", "b")
```

::: {.cell-output .cell-output-error}

```
Error in eval(expr, envir, enclos): Not compatible with requested type: [type=character; target=integer].
```


:::
:::




Note that there are a lot of choices to be made here. How are you going to store the class? Two numerics? A vector of length two? A formula? A string? What are users going to pass into the constructor? A string (“24/6”)? Two arguments? A vector?

There is no right answer to those questions. Make the best decision you can, and don’t be afraid to change it if your decision causes unforeseen difficulties.

You may not use any existing R functions or packages that would trivialize this assignment. (E.g. if you found an existing package that does this, or found a function that automatically produces the quotient or simplified version, that is not able to be used.)

Hint: It may be useful to define other functions that I don’t explicitly ask for.


##Problem 2 - plotly

Let’s revisit the art data from the last problem set. Use plotly for these.



::: {.cell}

```{.r .cell-code}
library(plotly)
```

::: {.cell-output .cell-output-stderr}

```
Loading required package: ggplot2
```


:::

::: {.cell-output .cell-output-stderr}

```

Attaching package: 'plotly'
```


:::

::: {.cell-output .cell-output-stderr}

```
The following object is masked from 'package:ggplot2':

    last_plot
```


:::

::: {.cell-output .cell-output-stderr}

```
The following object is masked from 'package:stats':

    filter
```


:::

::: {.cell-output .cell-output-stderr}

```
The following object is masked from 'package:graphics':

    layout
```


:::

```{.r .cell-code}
# Load art sales df
art <- read.csv("df_for_ml_improved_new_market.csv")
```
:::



### a. Regenerate your plot which addresses the second question from last time:

#### ii. Does the distribution of genre of sales across years appear to change?
You may copy your plot from last time, or copy my plot from the solutions, or come up with your own new plot.



::: {.cell}

```{.r .cell-code}
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}

```
── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ lubridate 1.9.3     ✔ tibble    3.2.1
✔ purrr     1.0.2     ✔ tidyr     1.3.1
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks plotly::filter(), stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```


:::

```{.r .cell-code}
# Get year and genre columns, clean up column names, and process two+ genres
genre <- art %>%
  dplyr::select(year, starts_with("Genre")) %>%
  rename_with(~ gsub("Genre___", "", .), starts_with("Genre___")) %>%
  mutate(two_plus = rowSums(select(., Photography, Print, Sculpture, Painting, Others), na.rm = TRUE)) %>%
  mutate("Two+ Genres" = ifelse(two_plus > 1, 1, 0)) %>%
  select(-two_plus)

# Reshape to long format
genre_long <- genre %>%
  pivot_longer(
    cols = -year,  
    names_to = "genre",  
    values_to = "count"
  )

# Summarize total sales for each genre per year
genre_summary <- genre_long %>%
  group_by(year, genre) %>%
  summarize(total_sales = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(genre = factor(genre, levels = c("Photography", "Print", "Sculpture", "Painting", "Others", "Two+ Genres")))
```

::: {.cell-output .cell-output-stderr}

```
`summarise()` has grouped output by 'year'. You can override using the
`.groups` argument.
```


:::

```{.r .cell-code}
# Interactive stacked bar chart using Plotly
plot_genre_distribution <- plot_ly(
  data = genre_summary,
  x = ~year,
  y = ~total_sales,
  color = ~genre,
  type = "bar",
  colors = "Set3"
) %>%
  layout(
    title = "Distribution of Genre of Sales Across Years",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Total Sales"),
    barmode = "stack"
  )

plot_genre_distribution
```

::: {.cell-output-display}

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-518ed2e392581343c77a" style="width:100%;height:464px;"></div>
<script type="application/json" data-for="htmlwidget-518ed2e392581343c77a">{"x":{"visdat":{"2db84acd43":["function () ","plotlyVisDat"]},"cur_data":"2db84acd43","attrs":{"2db84acd43":{"x":{},"y":{},"color":{},"colors":"Set3","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Distribution of Genre of Sales Across Years","xaxis":{"domain":[0,1],"automargin":true,"title":"Year"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Total Sales"},"barmode":"stack","hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[3,3,17,34,50,50,73,86,122,165,158,166,165,184,247,223],"type":"bar","name":"Photography","marker":{"color":"rgba(141,211,199,1)","line":{"color":"rgba(141,211,199,1)"}},"textfont":{"color":"rgba(141,211,199,1)"},"error_y":{"color":"rgba(141,211,199,1)"},"error_x":{"color":"rgba(141,211,199,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[0,0,0,2,7,6,13,7,26,43,43,54,55,37,80,41],"type":"bar","name":"Print","marker":{"color":"rgba(255,255,179,1)","line":{"color":"rgba(255,255,179,1)"}},"textfont":{"color":"rgba(255,255,179,1)"},"error_y":{"color":"rgba(255,255,179,1)"},"error_x":{"color":"rgba(255,255,179,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[5,4,5,53,37,29,70,72,122,129,146,153,149,143,289,235],"type":"bar","name":"Sculpture","marker":{"color":"rgba(190,186,218,1)","line":{"color":"rgba(190,186,218,1)"}},"textfont":{"color":"rgba(190,186,218,1)"},"error_y":{"color":"rgba(190,186,218,1)"},"error_x":{"color":"rgba(190,186,218,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[8,5,8,19,18,11,12,23,32,57,47,31,41,42,95,70],"type":"bar","name":"Painting","marker":{"color":"rgba(251,128,114,1)","line":{"color":"rgba(251,128,114,1)"}},"textfont":{"color":"rgba(251,128,114,1)"},"error_y":{"color":"rgba(251,128,114,1)"},"error_x":{"color":"rgba(251,128,114,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[8,5,8,19,18,11,12,23,32,57,52,35,44,47,101,74],"type":"bar","name":"Others","marker":{"color":"rgba(128,177,211,1)","line":{"color":"rgba(128,177,211,1)"}},"textfont":{"color":"rgba(128,177,211,1)"},"error_y":{"color":"rgba(128,177,211,1)"},"error_x":{"color":"rgba(128,177,211,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[8,5,8,19,18,11,12,23,32,57,47,31,41,42,95,70],"type":"bar","name":"Two+ Genres","marker":{"color":"rgba(253,180,98,1)","line":{"color":"rgba(253,180,98,1)"}},"textfont":{"color":"rgba(253,180,98,1)"},"error_y":{"color":"rgba(253,180,98,1)"},"error_x":{"color":"rgba(253,180,98,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

:::
:::




### b. Generate an interactive plot with plotly that can address both of these questions from last time:

### i. Is there a change in the sales price in USD over time?

### ii. How does the genre affect the change in sales price over time?

This should be a single interactive plot, with which a user can manipulate the view to be able to look at change over time overall, or by genre.



::: {.cell}

```{.r .cell-code}
# Calculate average sales price per genre per year
price_trend <- art %>%
  dplyr::select(year, price_usd, starts_with("Genre")) %>%
  # Get rid of the Genre___ start so we can just use the genre name for plot
  rename_with(~ gsub("Genre___", "", .), starts_with("Genre___")) %>%
  mutate(genre = case_when(Photography == 1 ~ "Photography",
                           Print == 1 ~ "Print",
                           Sculpture == 1 ~ "Sculpture",
                           Painting == 1 ~ "Painting",
                           Others == 1 ~ "Other")) %>%
  group_by(year, genre) %>%
  summarize(
    mean_price = mean(price_usd, na.rm = TRUE),
    median_price = median(price_usd, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(genre = factor(genre, levels = c("Photography", "Print", "Sculpture", "Painting", "Other")))
```

::: {.cell-output .cell-output-stderr}

```
`summarise()` has grouped output by 'year'. You can override using the
`.groups` argument.
```


:::

```{.r .cell-code}
# Create an interactive plotly plot with both mean and median prices
plot <- plot_ly(data = price_trend) %>%
  add_trace(
    x = ~year, y = ~mean_price, color = ~genre, 
    type = 'scatter', mode = 'lines+markers',
    name = ~paste("Mean Price -", genre),
    line = list(dash = 'solid')
  ) %>%
  add_trace(
    x = ~year, y = ~median_price, color = ~genre, 
    type = 'scatter', mode = 'lines+markers',
    name = ~paste("Median Price -", genre),
    line = list(dash = 'dash')
  ) %>%
  layout(
    title = "Sales Price Over Time by Genre (Mean and Median)",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Sales Price (USD)"),
    hovermode = "closest",
    legend = list(orientation = "h", y = -0.3)
  )

plot
```

::: {.cell-output-display}

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-0c17ca027e8583a0421b" style="width:100%;height:464px;"></div>
<script type="application/json" data-for="htmlwidget-0c17ca027e8583a0421b">{"x":{"visdat":{"2db852ee5922":["function () ","plotlyVisDat"]},"cur_data":"2db852ee5922","attrs":{"2db852ee5922":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"type":"scatter","mode":"lines+markers","name":{},"line":{"dash":"solid"},"inherit":true},"2db852ee5922.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"type":"scatter","mode":"lines+markers","name":{},"line":{"dash":"dash"},"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Sales Price Over Time by Genre (Mean and Median)","xaxis":{"domain":[0,1],"automargin":true,"title":"Year"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Sales Price (USD)"},"hovermode":"closest","legend":{"orientation":"h","y":-0.29999999999999999},"showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[2007,2008,2009,2010,2011,2012],"y":[9459.2000000000007,14652.75,10843.666666666666,8421.2000000000007,6093.833333333333,7227.75],"type":"scatter","mode":"lines+markers","name":"Mean Price - Other","line":{"color":"rgba(166,216,84,1)","dash":"solid"},"marker":{"color":"rgba(166,216,84,1)","line":{"color":"rgba(166,216,84,1)"}},"textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[7504.5,4983.8000000000002,6293.75,8287.21052631579,6302.1111111111113,7291.454545454545,10512.25,13016.826086956522,13695.75,13490.596491228071,19447.319148936171,32898.161290322583,22173.926829268294,17181.476190476191,19467.778947368421,16727.485714285714],"type":"scatter","mode":"lines+markers","name":"Mean Price - Painting","line":{"color":"rgba(231,138,195,1)","dash":"solid"},"marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[8498.3333333333339,9884,12464.705882352941,9955.4705882352937,8632.6200000000008,12580.879999999999,15365.424657534246,25208.569767441859,25901.163934426229,56425.060606060608,48917,61728.331325301202,40157.593939393941,49548.168478260872,46813.465587044535,44653.430493273539],"type":"scatter","mode":"lines+markers","name":"Mean Price - Photography","line":{"color":"rgba(102,194,165,1)","dash":"solid"},"marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[6474.5,15340.571428571429,23344.833333333332,8661.461538461539,28618.428571428572,9706.7307692307695,12035.046511627907,32210.069767441859,40204.648148148146,21210.072727272727,18456.18918918919,19837.362499999999,19862.731707317074],"type":"scatter","mode":"lines+markers","name":"Mean Price - Print","line":{"color":"rgba(252,141,98,1)","dash":"solid"},"marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[5185.1999999999998,11949,9745.7999999999993,5098.8301886792451,3765.2972972972975,5723.7241379310344,9627.942857142858,11777.513888888889,15922.508196721312,14610.480620155038,19593.945205479453,20853.882352941175,17735.879194630874,16185.846153846154,17311.522491349482,19864.638297872341],"type":"scatter","mode":"lines+markers","name":"Mean Price - Sculpture","line":{"color":"rgba(141,160,203,1)","dash":"solid"},"marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2007,2008,2009,2010,2011,2012],"y":[6609,8242,7179,9160,5795.5,7520.5],"type":"scatter","mode":"lines+markers","name":"Median Price - Other","line":{"color":"rgba(166,216,84,1)","dash":"dash"},"marker":{"color":"rgba(166,216,84,1)","line":{"color":"rgba(166,216,84,1)"}},"textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[5614,4604,4650,4367,4259.5,4490,5922,5346,5966,6362,7315,13563,9084,13804,8408,8585],"type":"scatter","mode":"lines+markers","name":"Median Price - Painting","line":{"color":"rgba(231,138,195,1)","dash":"dash"},"marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[6605,4683,11250,5324,3762.5,7128,9517,12036.5,12410,25588,23668,22530,19776,22064,23632,19090],"type":"scatter","mode":"lines+markers","name":"Median Price - Photography","line":{"color":"rgba(102,194,165,1)","dash":"dash"},"marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[6474.5,6272,21216,9250,11125,8044.5,6176,8579,19323,13205,13356,10860,13321],"type":"scatter","mode":"lines+markers","name":"Median Price - Print","line":{"color":"rgba(252,141,98,1)","dash":"dash"},"marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012],"y":[3640,8153,5520,3306,2669,3546,4881,6201,6668,5859,8136,11699,9055,6098,7633,7599],"type":"scatter","mode":"lines+markers","name":"Median Price - Sculpture","line":{"color":"rgba(141,160,203,1)","dash":"dash"},"marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

:::
:::




These will be graded similar to last time:

Is the type of graph & choice of variables appropriate to answer the question?
Is the graph clear and easy to interpret?
Is the graph publication ready?

## Problem 3 - data.table
Repeat Problem 1 from PS04 using data.table.



::: {.cell}

```{.r .cell-code}
library(data.table)
```

::: {.cell-output .cell-output-stderr}

```

Attaching package: 'data.table'
```


:::

::: {.cell-output .cell-output-stderr}

```
The following objects are masked from 'package:lubridate':

    hour, isoweek, mday, minute, month, quarter, second, wday, week,
    yday, year
```


:::

::: {.cell-output .cell-output-stderr}

```
The following objects are masked from 'package:dplyr':

    between, first, last
```


:::

::: {.cell-output .cell-output-stderr}

```
The following object is masked from 'package:purrr':

    transpose
```


:::

```{.r .cell-code}
library(nycflights13)
```
:::




### a. Generate a table (which can just be a nicely printed tibble) reporting the mean and median departure delay per airport. Generate a second table (which again can be a nicely printed tibble) reporting the mean and median arrival delay per airport. Exclude any destination with under 10 flights. Do this exclusion through code, not manually.

Additionally,

Order both tables in descending mean delay.
Both tables should use the airport names not the airport codes.
Both tables should print all rows.



::: {.cell}

```{.r .cell-code}
# Departures delay
flights <- data.table(flights)
#setDT(flights)
flights[, faa := dest]
```
:::

::: {.cell}

```{.r .cell-code}
merged <- merge(flights[, faa := origin],
                airports,
                by = "faa",
                all.x = TRUE)
```
:::



First table (reporting the mean and median departure delay per airport):


::: {.cell}

```{.r .cell-code}
merged[, .(N = .N,
           mean_delay = mean(dep_delay, na.rm = TRUE),
           median_delay = median(dep_delay, na.rm = TRUE)),
       by = name] |>
  _[N >= 10, !"N"] |>
  _[order(mean_delay, decreasing = TRUE)]
```

::: {.cell-output .cell-output-stdout}

```
                  name mean_delay median_delay
                <char>      <num>        <num>
1: Newark Liberty Intl   15.10795           -1
2: John F Kennedy Intl   12.11216           -1
3:          La Guardia   10.34688           -3
```


:::
:::

::: {.cell}

```{.r .cell-code}
# Departures delay
flights <- data.table(flights)
#setDT(flights)
#setDT(airports)
flights[, faa := dest]

# Arrival delay
merged <- merge(flights[, faa := dest],
                airports,
                by = "faa",
                all.x = TRUE)
```
:::



Second table (reporting the mean and median arrival delay per airport):


::: {.cell}

```{.r .cell-code}
merged[, .(name = ifelse(is.na(first(name)), first(faa), first(name)),
           N = .N,
           mean_delay = mean(arr_delay, na.rm = TRUE),
           median_delay = median(arr_delay, na.rm = TRUE)),
       by = faa] |>
  _[N >= 10, !c("faa", "N")] |>
  _[order(mean_delay, decreasing = TRUE)] |>
  print(x = _, nrows = 10000)
```

::: {.cell-output .cell-output-stdout}

```
                                     name   mean_delay median_delay
                                   <char>        <num>        <num>
  1:                Columbia Metropolitan  41.76415094         28.0
  2:                           Tulsa Intl  33.65986395         14.0
  3:                    Will Rogers World  30.61904762         16.0
  4:                 Jackson Hole Airport  28.09523810         15.0
  5:                        Mc Ghee Tyson  24.06920415          2.0
  6:               Dane Co Rgnl Truax Fld  20.19604317          1.0
  7:                        Richmond Intl  20.11125320          1.0
  8:        Akron Canton Regional Airport  19.69833729          3.0
  9:                      Des Moines Intl  19.00573614          0.0
 10:                   Gerald R Ford Intl  18.18956044          1.0
 11:                      Birmingham Intl  16.87732342         -2.0
 12:         Theodore Francis Green State  16.23463687          1.0
 13: Greenville-Spartanburg International  15.93544304         -0.5
 14:    Cincinnati Northern Kentucky Intl  15.36456376         -3.0
 15:            Savannah Hilton Head Intl  15.12950601         -1.0
 16:          Manchester Regional Airport  14.78755365         -3.0
 17:                          Eppley Afld  14.69889841         -2.0
 18:                               Yeager  14.67164179         -1.5
 19:                     Kansas City Intl  14.51405836          0.0
 20:                          Albany Intl  14.39712919         -4.0
 21:                General Mitchell Intl  14.16722038          0.0
 22:                       Piedmont Triad  14.11260054         -2.0
 23:               Washington Dulles Intl  13.86420212         -3.0
 24:               Cherry Capital Airport  12.96842105        -10.0
 25:              James M Cox Dayton Intl  12.68048606         -3.0
 26:     Louisville International Airport  12.66938406         -2.0
 27:                  Chicago Midway Intl  12.36422360         -1.0
 28:                      Sacramento Intl  12.10992908          4.0
 29:                    Jacksonville Intl  11.84483416         -2.0
 30:                       Nashville Intl  11.81245891         -2.0
 31:                Portland Intl Jetport  11.66040210         -4.0
 32:               Greater Rochester Intl  11.56064461         -5.0
 33:      Hartsfield Jackson Atlanta Intl  11.30011285         -1.0
 34:                Lambert St Louis Intl  11.07846451         -3.0
 35:                         Norfolk Intl  10.94909344         -4.0
 36:            Baltimore Washington Intl  10.72673385         -5.0
 37:                         Memphis Intl  10.64531435         -2.5
 38:                   Port Columbus Intl  10.60132291         -3.0
 39:                  Charleston Afb Intl  10.59296847         -4.0
 40:                    Philadelphia Intl  10.12719014         -3.0
 41:                  Raleigh Durham Intl  10.05238095         -3.0
 42:                    Indianapolis Intl   9.94043412         -3.0
 43:            Charlottesville-Albemarle   9.50000000         -5.0
 44:               Cleveland Hopkins Intl   9.18161129         -5.0
 45:        Ronald Reagan Washington Natl   9.06695204         -2.0
 46:                      Burlington Intl   8.95099602         -4.0
 47:                 Buffalo Niagara Intl   8.94595186         -5.0
 48:                Syracuse Hancock Intl   8.90392501         -5.0
 49:                          Denver Intl   8.60650021         -2.0
 50:                      Palm Beach Intl   8.56297210         -3.0
 51:                                  BQN   8.24549550         -1.0
 52:                             Bob Hope   8.17567568         -3.0
 53:       Fort Lauderdale Hollywood Intl   8.08212154         -3.0
 54:                          Bangor Intl   8.02793296         -9.0
 55:           Asheville Regional Airport   8.00383142         -1.0
 56:                                  PSE   7.87150838          0.0
 57:                      Pittsburgh Intl   7.68099053         -5.0
 58:                       Gallatin Field   7.60000000         -2.0
 59:                 NW Arkansas Regional   7.46572581         -2.0
 60:                           Tampa Intl   7.40852503         -4.0
 61:               Charlotte Douglas Intl   7.36031885         -3.0
 62:             Minneapolis St Paul Intl   7.27016886         -5.0
 63:                      William P Hobby   7.17618819         -4.0
 64:                         Bradley Intl   7.04854369        -10.0
 65:                     San Antonio Intl   6.94537178         -9.0
 66:                      South Bend Rgnl   6.50000000         -3.5
 67:     Louis Armstrong New Orleans Intl   6.49017497         -6.0
 68:                        Key West Intl   6.35294118          7.0
 69:                        Eagle Co Rgnl   6.30434783         -4.0
 70:                Austin Bergstrom Intl   6.01990875         -5.0
 71:                   Chicago Ohare Intl   5.87661475         -8.0
 72:                         Orlando Intl   5.45464309         -5.0
 73:               Detroit Metro Wayne Co   5.42996346         -7.0
 74:                        Portland Intl   5.14157973         -5.0
 75:                        Nantucket Mem   4.85227273         -3.0
 76:                      Wilmington Intl   4.63551402         -7.0
 77:                    Myrtle Beach Intl   4.60344828        -13.0
 78:    Albuquerque International Sunport   4.38188976         -5.5
 79:         George Bush Intercontinental   4.24079040         -5.0
 80:        Norman Y Mineta San Jose Intl   3.44817073         -7.0
 81:               Southwest Florida Intl   3.23814963         -5.0
 82:                       San Diego Intl   3.13916574         -5.0
 83:              Sarasota Bradenton Intl   3.08243131         -5.0
 84:            Metropolitan Oakland Intl   3.07766990         -9.0
 85:   General Edward Lawrence Logan Intl   2.91439222         -9.0
 86:                   San Francisco Intl   2.67289152         -8.0
 87:                                  SJU   2.52052659         -6.0
 88:                         Yampa Valley   2.14285714          2.0
 89:              Phoenix Sky Harbor Intl   2.09704733         -6.0
 90:            Montrose Regional Airport   1.78571429        -10.5
 91:                     Los Angeles Intl   0.54711094         -7.0
 92:               Dallas Fort Worth Intl   0.32212685         -9.0
 93:                           Miami Intl   0.29905978         -9.0
 94:                       Mc Carran Intl   0.25772849         -8.0
 95:                  Salt Lake City Intl   0.17625459         -8.0
 96:                           Long Beach  -0.06202723        -10.0
 97:                Martha\\\\'s Vineyard  -0.28571429        -11.0
 98:                  Seattle Tacoma Intl  -1.09909910        -11.0
 99:                        Honolulu Intl  -1.36519258         -7.0
100:                                  STT  -3.83590734         -9.0
101:            John Wayne Arpt Orange Co  -7.86822660        -11.0
102:                    Palm Springs Intl -12.72222222        -13.5
                                     name   mean_delay median_delay
```


:::
:::





### b. How many flights did the aircraft model with the fastest average speed take? Produce a tibble with 1 row, and entires for the model, average speed (in MPH) and number of flights.



::: {.cell}

```{.r .cell-code}
planes <- data.table(planes)
#setDT(planes)
```
:::

::: {.cell}

```{.r .cell-code}
merged <- merge(flights,
                planes,
                by = "tailnum",
                all.x = TRUE)
```
:::

::: {.cell}

```{.r .cell-code}
allmodels <- merged[, `:=`(num_flights = .N,
                           avg_mph = mean(distance/(air_time/60), na.rm = TRUE)),
                    by = model]

allmodels[allmodels[, .I[which.max(avg_mph)]],.(model, avg_mph, num_flights)]
```

::: {.cell-output .cell-output-stdout}

```
     model  avg_mph num_flights
    <char>    <num>       <int>
1: 777-222 482.6254           4
```


:::
:::

