#'---
#'title: "Proof Without Words with gganimate: Sum of Cubes"
#'author: "notesofdabbler"
#'date: Apr 26, 2020
#'output: html_document
#'---
#'
#' The site [AoPS Online](https://artofproblemsolving.com/wiki/index.php/Proofs_without_words) has
#' several examples that have illustrations to show proofs without words for several mathematical theorems and identities.
#' Here I used [gganimate](https://gganimate.com/index.html) to create the illustration for the following result (using the approach in [AoPS Online](https://artofproblemsolving.com/wiki/index.php/Proofs_without_words))
#' $$ 1^3 + 2^3 + 3^3 + \ldots + n^3 = (1 + 2 + \ldots + n)^2 $$
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

#' I created a function to generate block representation of $i^3$
#+
get_blocks = function(i, palname = "Blue") {
  
  if(i %% 2 == 1) {
    col_list = tableau_seq_gradient_pal(palname)(seq(0, 1, length = i))
  } else {
    col_list = tableau_seq_gradient_pal(palname)(seq(0, 1, length = i + 1))
  }
  
  dfL = list()
  for (j in 1:(i-1)) {
    x = expand.grid(x = (j - 1) * (i + 1) + seq(1, i), y = i * (i - 1)/2 + (i - 1) + seq(1, i))
    dfL[[j]] = x %>% mutate(state = paste0(i, "_", j), bcol = col_list[j])
  }
  if (i %% 2 == 0) {
    x = expand.grid(x = (i - 1) * (i + 1) + seq(1, i/2), y = i * (i - 1)/2 + (i - 1) + seq(1, i))
    dfL[[i]] = x %>% mutate(state = paste0(i, "_", i), bcol = col_list[i])
    x = expand.grid(x = (i - 1) * (i + 1) + i/2 + 1 + seq(1, i/2), y = i * (i - 1)/2 + (i - 1) + seq(1, i))
    dfL[[i + 1]] = x %>% mutate(state = paste0(i, "_", (i + 1)), bcol = col_list[i + 1])
  } else {
    x = expand.grid(x = (i - 1) * (i + 1) + seq(1, i), y = i * (i - 1)/2 + (i - 1) + seq(1, i))
    dfL[[i]] = x %>% mutate(state = paste0(i, "_", i), bcol = col_list[i])
  }
  for(j in 1:length(dfL)) {

    nr = nrow(dfL[[j]])
    yavg = mean(dfL[[j]]$y)
    if (j == 1) {
      dfL[[j]] = dfL[[j]] %>% mutate(lbl = c(paste0(i, "^3"), rep(NA, nr - 1)),
                                     xt = c(-2, rep(NA, nr - 1)),
                                     yt = c(yavg, rep(NA, nr - 1)))
    } else {
      dfL[[j]] = dfL[[j]] %>% mutate(lbl = rep(NA, nr),
                                     xt = rep(NA, nr),
                                     yt = rep(NA, nr))
      
    }
  }
  return(dfL)
}

#' A static plot of blocks generate for $1^3, 2^3, 3^3, 4^3, 5^3$ is shown below
#+
df_base = tibble(x = 1, y = 1, bcol = "black", xt = -2, yt = 1, lbl = "1^3")

df = bind_rows(df_base, get_blocks(2, "Blue"), 
               get_blocks(3, "Red"), 
               get_blocks(4, "Orange"), 
               get_blocks(5, "Purple"))

p = ggplot(df) + geom_tile(aes(x = x, y = y, fill = bcol), color = "black") +
  scale_fill_identity() + 
  geom_text(aes(x = xt, y = yt, label = lbl), fontface = "bold", size = 5, parse = TRUE) +
  theme_void()
p

#' The blocks generate for $i^3$ can be rearranged in an alternate manner (which helps to show the identity). I
#' created a function to take the blocks generated for $i^3$ and rearrange them
#+
get_shifted_blocks = function(blocks) {
  
  dfL = blocks
  i = sqrt(nrow(dfL[[1]]))
  
  dfaL = list()
  
  if (i %% 2 == 1) {
    for (j in 1:i) {
      k = (i - 1)/2 + 1
      if(j <= k) {
        tmp = dfL[[j]]
        tmp = tmp %>% mutate(x = x - (j - 1), y = y - (i - 1))
        dfaL[[j]] = tmp
      } else {
        tmp = dfaL[[k]]
        pickcol = dfL[[j]]$bcol[1]
        tmp = tmp %>% mutate(y = y - i * (j - k), bcol = pickcol)
        dfaL[[j]] = tmp
      }
    }
  } else {
    tmp = dfL[[i]]
    tmp = tmp %>% mutate(x = x - (i + 1) * (i - 1), y = y - (i - 1))
    dfaL[[1]] = tmp
    for (j in 1:(i - 1)) {
      if (j <= i / 2) {
        tmp = dfL[[j]]
        tmp = tmp %>% mutate(x = x - (j - 1) + i/2, y = y - (i - 1))
        dfaL[[j + 1]] = tmp
      } else {
        tmp = dfaL[[i/2 + 1]]
        pickcol = dfL[[j]]$bcol[1]
        tmp = tmp %>% mutate(y = y - i * (j - i/2), bcol = pickcol)
        dfaL[[j + 1]] = tmp
      }
    }
    tmp = dfL[[i + 1]]
    pickcol = tmp$bcol[1]
    xseq = seq(i * (i/2 - 1) + i/2 + 1, i * (i + 1)/2)
    tmp2 = tibble(x = rep(xseq, i/2), 
               y = rep(seq(1, i/2), each = length(xseq)), type = paste0(i,"_", (i + 1)))
    tmp2 = tmp2 %>% mutate(bcol = pickcol)
    dfaL[[i + 1]] = tmp2
  }
  
  return(dfaL)
  
}

#' A static plot of rearragend blocks generated for $1^3, 2^3, 3^3, 4^3, 5^3$ is shown below
#+
df = bind_rows(tibble(x = 1, y = 1, bcol = "black"), get_shifted_blocks(get_blocks(2, "Blue")), 
               get_shifted_blocks(get_blocks(3, "Red")),
               get_shifted_blocks(get_blocks(4, "Orange")),
               get_shifted_blocks(get_blocks(5, "Purple")),
               )

p = ggplot(df) + geom_tile(aes(x = x, y = y, fill = bcol), color = "black") +
  scale_fill_identity() + theme_void()
p

#' To enable an animation to, I created a sequence of dataframes where
#' each state rearranges blocks for a particular $i^3$
#' 
#+
df1 = bind_rows(df_base, get_blocks(2, "Blue"), 
               get_blocks(3, "Red"), 
               get_blocks(4, "Orange"), 
               get_blocks(5, "Purple")) %>% mutate(state = 1)
df2 = bind_rows(df_base, get_shifted_blocks(get_blocks(2, "Blue")), 
                get_blocks(3, "Red"), 
                get_blocks(4, "Orange"), 
                get_blocks(5, "Purple")) %>% mutate(state = 2)
df3 = bind_rows(df_base, get_shifted_blocks(get_blocks(2, "Blue")), 
                get_shifted_blocks(get_blocks(3, "Red")), 
                get_blocks(4, "Orange"), 
                get_blocks(5, "Purple")) %>% mutate(state = 3)
df4 = bind_rows(df_base, get_shifted_blocks(get_blocks(2, "Blue")), 
                get_shifted_blocks(get_blocks(3, "Red")), 
                get_shifted_blocks(get_blocks(4, "Orange")), 
                get_blocks(5, "Purple")) %>% mutate(state = 4)
df5 = bind_rows(df_base, get_shifted_blocks(get_blocks(2, "Blue")), 
                get_shifted_blocks(get_blocks(3, "Red")), 
                get_shifted_blocks(get_blocks(4, "Orange")), 
                get_shifted_blocks(get_blocks(5, "Purple"))) %>% mutate(state = 5)

df_all = bind_rows(df1, df2, df3, df4, df5)

p = ggplot(df_all) + geom_tile(aes(x = x, y = y, fill = bcol), color = "black") +
  scale_fill_identity() + 
  geom_text(aes(x = xt, y = yt, label = lbl), fontface = "bold", size = 5, parse = TRUE) +
  theme_void()


anim = p + transition_states(state, transition_length = 1, state_length = 2)
animate(anim, renderer = gifski_renderer("figures/sum_of_cubes.gif"))

#+
sessionInfo()