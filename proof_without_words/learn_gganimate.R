library(gganimate)
#> Loading required package: ggplot2

# We'll start with a static plot
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()

plot(p)

anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1) +
  ggtitle('Now showing {closest_state}',
        subtitle = 'Frame {frame} of {nframes}')


animate(anim, renderer = gifski_renderer("gganim.gif"))

anim <- ggplot(mtcars, aes(factor(gear), mpg)) +
  geom_boxplot() +
  transition_manual(gear, cumulative = TRUE)

animate(anim, renderer = gifski_renderer("gganim.gif"))

anim <- ggplot(iris, aes(Petal.Width, Petal.Length, colour = Species)) +
  geom_point() +
  transition_filter(
    transition_length = 2,
    filter_length = 1,
    Setosa = Species == 'setosa',
    Long = Petal.Length > 4,
    Wide = Petal.Width > 2
  ) +
  ggtitle(
    'Filter: {closest_filter}',
    subtitle = '{closest_expression}'
  ) +
  enter_fade() +
  exit_fly(y_loc = 0)

animate(anim, renderer = gifski_renderer("gganim.gif"))

