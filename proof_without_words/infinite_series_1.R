#'---
#'title: "Proof Without Words with gganimate"
#'author: "notesofdabbler"
#'date: Apr 26, 2020
#'output: html_document
#'---
#'
#' The site [AoPS Online](https://artofproblemsolving.com/wiki/index.php/Proofs_without_words) has
#' several examples that have illustrations to show proofs without words for several mathematical theorems and identities.
#' Here I used [gganimate](https://gganimate.com/index.html) to create the illustration for the following results (using the approach in [AoPS Online](https://artofproblemsolving.com/wiki/index.php/Proofs_without_words))
#' $$ \frac{1}{4} + \frac{1}{4^2} + \frac{1}{4^3} + \ldots = \frac{1}{3} $$
#' 
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

#+
library(ggplot2)
library(gganimate)
library(dplyr)
library(transformr)

#+
# color list from tableau color palette from ggthemes package
col_list = c("white", "#8CD17D", "#FABFD2")

#' Here I am creating a dataframe in the following sequence
#' 
#' * First I create a dataframe with (x, y) for a 1 x 1 square. This is state 0
#' * Next I create dataframe with (x, y) for three 1/2 x 1/2 squares at bottom left, top left, and bottom right of the 1 x 1 square.This is state 1
#' * Then I repeat the above step with a 1/2 x 1/2 square at the top right of the 1 x 1 square. This is state 2
#' * Then I repeat the above step with a 1/4 x 1/4 square at the top right of the 1 x 1 square. This is state 3
#' * I repeat a similar process for a few more times
#' 
#+
df = list()

k = 1
df[[k]] = tibble(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), type = "main", fillgrp = col_list[1], state = 0)

l = 1
h = 0
k = k + 1

for(i in 1:10) {
  df[[k]] = tibble(x = c(h, h + l/2, h + l/2, h), y = c(h, h, h + l/2, h + l/2), type = paste0("blue", i), fillgrp = col_list[2], state = i)
  if (i <= 3) {
    df[[k]] = df[[k]] %>% mutate(xt = c(h + l/4, NA, NA, NA), yt = c(h + l/4, NA, NA, NA), lbl = c(paste0("frac(1, 2^", 2*i,")"), NA, NA, NA))
  }
  df[[k + 1]] = tibble(x = c(h, h + l/2, h + l/2, h), y = c(h + l/2, h + l/2, h + l, h + l), type = paste0("red1_", i), fillgrp = col_list[3], state = i)
  df[[k + 2]] = tibble(x = c(h + l/2, h + l, h + l, h + l/2), y = c(h, h, h + l/2, h + l/2), type = paste0("red2_", i), fillgrp = col_list[3], state = i)
  k = k + 3
  l = l / 2
  h = h + 1 / 2^i
}

df_all = bind_rows(df)

#' A static plot of the dataframe is shown below
#+
p = ggplot(df_all) + geom_polygon(aes(x = x, y = y, group = type, fill = fillgrp), color = "black", alpha = 0.5)
p = p + geom_text(aes(x = xt, y = yt, label = lbl), fontface = "bold", parse = TRUE)
p = p + scale_fill_identity() + guides(fill = FALSE)
p = p + theme_void() 
p

#' The above plot is animated to transition between states
#+
p = ggplot(df_all) + geom_polygon(aes(x = x, y = y, group = type, fill = fillgrp), color = "black", alpha = 0.5)
p = p + geom_text(aes(x = xt, y = yt, label = lbl), fontface = "bold", parse = TRUE)
p = p + scale_fill_identity() + guides(fill = FALSE)
p = p + theme_void() 
anim = p + transition_states(state, transition_length = 2, state_length = 4) +
           shadow_mark()
animate(anim, renderer = gifski_renderer("figures/infinite_series_1.gif"))

#+
sessionInfo()



