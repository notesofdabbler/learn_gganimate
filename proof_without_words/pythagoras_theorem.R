#'---
#'title: "Proof Without Words with gganimate"
#'author: "notesofdabbler"
#'date: Apr 26, 2020
#'output: html_document
#'---
#'
#' ## Pythagoras Theorem:
#' According to Pythagoras theorem, $$ a^2 + b^2 = c^2 $$
#' where $a$, $b$, $c$ are sides of a right angled triangle (with $c$ being the side opposite $90^o$ angle)
#' 
#' There was an illustration of the proof of pythogoras theorem in a [video](https://www.youtube.com/watch?v=T2K11eFepcs) from [echalk](http://www.eChalk.co.uk).
#' My goal here is to use [gganimate](https://gganimate.com/index.html) package to produce a similar illustration.
#' 
#'
#'

#+ setup, include=FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

#+
library(ggplot2)
library(gganimate)
library(ggthemes)
library(dplyr)
library(transformr)

#' I tried to create 4 states following the video. The dataframe for first state
#' and the associated plot is shown below
#'
#+ 
# State - a1
df1 = tibble(x = c(0, 7, 7, 0), y = c(0, 0, 7, 7), type = "sq", 
             xt = c(2.5, 6, NA, NA), yt = c(4, 1, NA, NA), lbl = c("bold(a^2)", "bold(b^2)",NA, NA))
df2 = tibble(x = c(0, 0, 5), y = c(0, 2, 0), type = "tr1")
df3 = tibble(x = c(0, 5, 5), y = c(2, 2, 0), type = "tr2",
             xt = c(2.5, 2.5, 4.8), yt = c(1.2, 2.2, 1.2), lbl = c("c", "a", "b"))
df4 = tibble(x = c(5, 7, 7), y = c(2, 2, 7), type = "tr3")
df5 = tibble(x = c(5, 5, 7), y = c(2, 7, 7), type = "tr4",
             xt = c(4.8, 6, 6), yt = c(4.7, 6.8, 4.7), lbl = c("a", "b", "c"))

df_a1 = bind_rows(df1, df2, df3, df4, df5) %>% mutate(state = "a1")

col_list = c("#F1CE63", "#E15759", "#76B7B2", "#E15759", "#76B7B2")
p = ggplot(df_a1) + geom_polygon(aes(x = x, y = y, group = type, fill = type))
p = p + geom_text(aes(x = xt, y = yt, label = lbl), parse = TRUE, size = 5)
p = p + scale_fill_manual(values = col_list) + guides(fill = FALSE)
p = p + theme_void()
p

#' The dataframe for the second state
#' and the associated plot is shown below
#'
#+ 
# State - a2
df1_a = tibble(x = c(0, 7, 7, 0), y = c(0, 0, 7, 7), type = "sq")
df5_a = tibble(x = c(0, 0, 2), y = c(2, 7, 7), type = "tr4",
               xt = c(0.2, 1, 1), yt = c(4.7, 6.8, 4.7), lbl = c("a", "b", "c"))
df_a2 = bind_rows(df1_a, df2, df3, df4, df5_a) %>% mutate(state = "a2")


p = ggplot(df_a2) + geom_polygon(aes(x = x, y = y, group = type, fill = type))
p = p + geom_text(aes(x = xt, y = yt, label = lbl), parse = TRUE, size = 5)
p = p + scale_fill_manual(values = col_list) + guides(fill = FALSE)
p = p + theme_void()
p

#' The dataframe for the third state
#' and the associated plot is shown below
#'
#+
# state - a3
df4_a = tibble(x = c(5, 7, 7), y = c(0, 0, 5), type = "tr3")
df_a3 = bind_rows(df1_a, df2, df3, df4_a, df5_a) %>% mutate(state = "a3")

p = ggplot(df_a3) + geom_polygon(aes(x = x, y = y, group = type, fill = type))
p = p + geom_text(aes(x = xt, y = yt, label = lbl), parse = TRUE, size = 5)
p = p + scale_fill_manual(values = col_list) + guides(fill = FALSE)
p = p + theme_void()
p

#' The dataframe for the fourth state
#' and the associated plot is shown below
#'
#+ 
# state - a4
df1_b = tibble(x = c(0, 7, 7, 0), y = c(0, 0, 7, 7), type = "sq", 
               xt = c(3.5, NA, NA, NA), yt = c(4, NA, NA, NA), lbl = c("bold(c^2)", NA ,NA, NA))
df3_a = tibble(x = c(2, 7, 7), y = c(7, 7, 5), type = "tr2",
               xt = c(4.5, 4.5, 6.8), yt = c(6.2, 6.8, 6.2), lbl = c("c", "a", "b"))
df_a4 = bind_rows(df1_b, df2, df3_a, df4_a, df5_a) %>% mutate(state = "a4")

p = ggplot(df_a4) + geom_polygon(aes(x = x, y = y, group = type, fill = type))
p = p + geom_text(aes(x = xt, y = yt, label = lbl), parse = TRUE, size = 5)
p = p + scale_fill_manual(values = col_list) + guides(fill = FALSE)
p = p + theme_void()
p

#' Now the dataframes for the 4 states are combined into a single dataframe
#' and gganimate is used to transition between states to construct the animation

#+ 
# combining the dataframe into a single dataframe for animation
df = bind_rows(df_a1, df_a2, df_a3, df_a4)

p = ggplot(df) + geom_polygon(aes(x = x, y = y, group = type, fill = type))
p = p + geom_text(aes(x = xt, y = yt, label = lbl), parse = TRUE, size = 5)
p = p + scale_fill_manual(values = col_list) + guides(fill = FALSE)
p = p + theme_void()

anim = p + transition_states(state, transition_length = 1, state_length = 2, wrap = FALSE)
animate(anim, renderer = gifski_renderer("figures/pythagoras_theorem.gif"))



