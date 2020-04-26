
#
# https://www.maa.org/press/periodicals/convergence/proofs-without-words-and-beyond-proofs-without-words-20
# https://mathoverflow.net/questions/8846/proofs-without-words
# https://artofproblemsolving.com/wiki/index.php/Proofs_without_words
#


library(ggplot2)
library(gganimate)
library(dplyr)
library(transformr)


dfL = list()
n = 5
for (k in 1:n) {
  x = c()
  y = c()
  for (i in 1:n) {
    if(i <= k) {
      if(i == 1) {
        x = c(x, 1)
        y = c(y, n)
      } else {
        x = c(x, seq(1, i), rep(i, i - 1))
        y = c(y, rep(n - i + 1, i), seq(n, n - i + 2))
      }
    } else {
      x = c(x, seq(1, 2*i - 1))
      y = c(y, rep(n - i + 1, 2*i - 1))
    }
  }
  dfL[[k]] = tibble(x = x, y = y) %>% mutate(state = k)
}

df = bind_rows(dfL)

p = ggplot(df) + geom_point(aes(x = x, y = y), size = 10)

anim = p + transition_states(factor(state))
animate(anim, renderer = gifski_renderer("gganim.gif"))

df1 = tibble(x = c(1, 1, 2, 3, 1, 2, 3, 4, 5), y = c(3, 2, 2, 2, 1, 1, 1, 1, 1), time = 1)
df2 = tibble(x = c(1, 1, 2, 2, 1, 2, 3, 4, 5), y = c(3, 2, 2, 3, 1, 1, 1, 1, 1), time = 2)
df3 = tibble(x = c(1, 1, 2, 2, 1, 2, 3, 3, 3), y = c(3, 2, 2, 3, 1, 1, 1, 3, 2), time = 3)

df = bind_rows(df1, df2, df3)

p = ggplot(df) + geom_point(aes(x = x, y = y), size = 10)
p

anim = p + transition_time(time)
animate(anim, renderer = gifski_renderer("gganim.gif"))

anim = p + transition_states(factor(time))
animate(anim, renderer = gifski_renderer("gganim.gif"))

df = tibble(x = c(0, 1), y = c(0, 0), bcol = c("red", "blue"))
ggplot() + geom_tile(data = df, aes(x = x, y = y, fill = bcol)) 

df1 = tibble(x = c(0, 7, 7, 0), y = c(0, 0, 7, 7), type = "sq")
df2 = tibble(x = c(0, 0, 5), y = c(0, 2, 0), type = "tr1")
df3 = tibble(x = c(0, 5, 5), y = c(2, 2, 0), type = "tr2")
df4 = tibble(x = c(5, 7, 7), y = c(2, 2, 7), type = "tr3")
df5 = tibble(x = c(5, 5, 7), y = c(2, 7, 7), type = "tr4")

df_a1 = bind_rows(df1, df2, df3, df4, df5) %>% mutate(state = "a1")

p = ggplot() + geom_polygon(data = df_a1, aes(x = x, y = y, group = type, fill = type))
p = p + annotate("text", c(2, 0.2, 0.2, 6, 2), c(0.2, 4, 1, 0.2, 1), label = c("a", "a", "b", "b", "c"))
p

df5_a = tibble(x = c(0, 0, 2), y = c(2, 7, 7), type = "tr4")
df_a2 = bind_rows(df1, df2, df3, df4, df5_a) %>% mutate(state = "a2")

df4_a = tibble(x = c(5, 7, 7), y = c(0, 0, 5), type = "tr3")
df_a3 = bind_rows(df1, df2, df3, df4_a, df5_a) %>% mutate(state = "a3")

df3_a = tibble(x = c(2, 7, 7), y = c(7, 7, 5), type = "tr2")
df_a4 = bind_rows(df1, df2, df3_a, df4_a, df5_a) %>% mutate(state = "a4")

df = bind_rows(df_a1, df_a2, df_a3, df_a4)

p = ggplot() + geom_polygon(data = df, aes(x = x, y = y, group = type, fill = type))
p = p + annotate("text", c(2, 0.2, 0.2, 6, 2), c(0.2, 4, 1, 0.2, 1), label = c("a", "a", "b", "b", "c"))
anim = p + transition_states(state)
animate(anim, renderer = gifski_renderer("gganim.gif"))

df = tibble(x = c(0, 2, 2), y = c(0, 0, 3), xt = c(1.5, NA, NA), yt = c(1, NA, NA), 
               lbl = c(NA, NA, "hello"))
p = ggplot(df) + geom_polygon(aes(x = x, y = y), fill = "yellow")
p = p + geom_text(aes(x = x, y = y, label = lbl), size = 10)
p
