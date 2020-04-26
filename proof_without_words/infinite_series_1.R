#
# https://www.maa.org/press/periodicals/convergence/proofs-without-words-and-beyond-proofs-without-words-20
# https://mathoverflow.net/questions/8846/proofs-without-words
# https://artofproblemsolving.com/wiki/index.php/Proofs_without_words
#


library(ggplot2)
library(gganimate)
library(dplyr)
library(transformr)

col_list = c("white", "#8CD17D", "#FABFD2")

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


p = ggplot(df_all) + geom_polygon(aes(x = x, y = y, group = type, fill = fillgrp), color = "black", alpha = 0.5)
p = p + geom_text(aes(x = xt, y = yt, label = lbl), fontface = "bold", parse = TRUE)
#p = p + scale_fill_manual(values = col_list) + guides(fill = FALSE)
p = p + scale_fill_identity() + guides(fill = FALSE)
p = p + theme_void() 
p

p = ggplot(df_all) + geom_polygon(aes(x = x, y = y, group = type, fill = fillgrp), color = "black", alpha = 0.5)
#p = p + scale_fill_manual(values = col_list) + guides(fill = FALSE)
p = p + geom_text(aes(x = xt, y = yt, label = lbl), fontface = "bold", parse = TRUE)
p = p + scale_fill_identity() + guides(fill = FALSE)
p = p + theme_void() 
anim = p + transition_states(state, transition_length = 2, state_length = 4) +
           shadow_mark()
animate(anim, renderer = gifski_renderer("proof_without_words/figures/infinite_series_1.gif"))





