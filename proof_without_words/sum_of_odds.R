#'---
#'title: "Proof Without Words with gganimate: Sum of Odds"
#'author: "notesofdabbler"
#'date: Apr 26, 2020
#'output: html_document
#'---
#'
#' The site [AoPS Online](https://artofproblemsolving.com/wiki/index.php/Proofs_without_words) has
#' several examples that have illustrations to show proofs without words for several mathematical theorems and identities.
#' Here I used [gganimate](https://gganimate.com/index.html) to create the illustration for the following result (using the approach in [AoPS Online](https://artofproblemsolving.com/wiki/index.php/Proofs_without_words))
#' $$ 1 + 3 + 5 + \ldots + (2n - 1) = n^2 $$
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

# color scheme from ggthemes tableau color scheme
col_list = tableau_color_pal("Tableau 10")(10)

#' I created a function to generate block representation of odd number $(2i - 1)$
#+
get_blocks = function(i, n, pickcol = col_list) {
  df = tibble(x = (n - i) + seq(1, 2 * i - 1), y = n - i + 1, type = paste0(i, "_1"), bcol = pickcol[i])
  df = df %>% mutate(xt = c(0, rep(NA, 2 *i - 2)), yt = c(n - i + 1, rep(NA, 2 * i - 2)), lbl = c(2*i - 1, rep(NA, 2 * i - 2)))
  return(df)
}

#' Below is a static plot with stacking of blocks for odd numbers from 1 to 4
#+
df1 = bind_rows(get_blocks(1, 4), get_blocks(2, 4), get_blocks(3, 4), get_blocks(4, 4))
p = ggplot(df1) + geom_tile(aes(x = x, y = y, fill = bcol), color = "black")
p = p + geom_text(aes(x = xt, y = yt, label = lbl), parse = TRUE, fontface = "bold", size = 5)
p = p + scale_fill_identity()
p = p + theme_void()
p

#' The blocks generate for $(2i-1)$ can be rearranged in an alternate manner (which helps to show the identity). I
#' created a function to get rearranged blocks for $(2i-1)$
#+
get_shifted_blocks = function(i, n, pickcol = col_list) {
  df = tibble(x = n - 1 + c(seq(1, i), rep(i, i - 1)), 
              y = c(rep(n - i + 1, i - 1), seq(n - i + 1, n)), type = paste0(i, "_1"), bcol = pickcol[i])
  df = df %>% mutate(xt = rep(NA, 2 *i - 1), yt = rep(NA, 2 * i - 1), lbl = rep(NA, 2 * i - 1))
  return(df)
}

#' Below is a static plot with rearranged blocks for odd numbers 1 to 4
#+
df2 = bind_rows(get_shifted_blocks(1, 4), get_shifted_blocks(2, 4), 
                get_shifted_blocks(3, 4), get_shifted_blocks(4, 4))
p = ggplot(df2) + geom_tile(aes(x = x, y = y, fill = bcol), color = "black")
p = p + scale_fill_identity()
p = p + theme_void()
p

#' For animation, the following sequence of states with a dataframe for each is created
#' 
#' * Create the first arrangement of blocks for 1, 3, 5, 7, 9 - State 1
#' * Replace blocks for 3 with rearranged blocks - State 2
#' * Replace blocks for 5 with rearranged blocks - State 3
#' * Replace blocks for 7 with rearranged blocks - State 4
#' * Replace blocks for 9 with rearranged blocks - State 5
#' 
#' Combine all the dataframes into a single dataframe which is used to transition
#' over states
#' 

#+
n = 5

df_animL = list()
df_anim = list()
dfL = list()
for(i in 1:n) {
  dfL[[i]] = get_blocks(i, n)
}
df_animL[[1]] = dfL
df_anim[[1]] = bind_rows(df_animL[[1]]) %>% mutate(state = 1)

for(i in 2:n) {
  tmp = df_animL[[i - 1]]
  tmp[[i]] = get_shifted_blocks(i, n)
  df_animL[[i]] = tmp
  df_anim[[i]] = bind_rows(df_animL[[i]]) %>% mutate(state = i)
}

df_anim_all = bind_rows(df_anim)

# p = ggplot(df_anim_all %>% filter(state == 2)) + geom_tile(aes(x = x, y = y, fill = bcol), color = "black")
# p = p + geom_text(aes(x = xt, y = yt, label = lbl), parse = TRUE, fontface = "bold", size = 5)
# p = p + scale_fill_identity()
# p = p + theme_void()
# p

p = ggplot(df_anim_all) + geom_tile(aes(x = x, y = y, fill = bcol), color = "black")
p = p + geom_text(aes(x = xt, y = yt, label = lbl), parse = TRUE, fontface = "bold", size = 5)
p = p + scale_fill_identity()
p = p + theme_void()

anim = p + transition_states(state, transition_length = 2, state_length = 4)
animate(anim, renderer = gifski_renderer("figures/sum_of_odds.gif"))

#+
sessionInfo()