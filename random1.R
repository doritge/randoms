########################################
#
#  Object of random points bound in ellipse with a one common start point to form segments:
#     The points are randomly grouped, each group having a random index.
#
#######################################
library(tidyverse)

build_obj <- function(n_points = 100,
                      n_groups = 10,
                      radius_x = 10,
                      radius_y = 10,
                      start_x = 0,
                      start_y = 0)
{
  # create random ellipse parameters
  x = runif(n_points) * radius_x
  y = runif(n_points) * radius_y
  a = runif(n_points) * 2*pi
  groups <- tibble(group = 1:n_groups, rind = runif(n_groups))
  
  # create data frame
  segments <- tibble(x = x*cos(a), y = y*sin(a)) %>%    # random point in ellipse
    mutate(sx = start_x, sy = start_y,                  # start point
           group = sample(1:n_groups, n_points, replace = TRUE)) %>%  # distribute points in groups
    left_join(groups)
  
  return(segments)
}

###### Build basic object ##############
object <- build_obj(n_points = 150,
                    n_groups = 150,
                    radius_x = 1, 
                    radius_y = 1,
                    start_x = runif(1, -1, 1),
                    start_y = runif(1, -1,1)
)

###### Split object ###################
n_splits <- 8
splits <- object %>%
  mutate(split = row_number() %% n_splits) %>%
  group_split(split)

###### Animate object (rotation) ######
rotate <- function(obj, angle){
  obj %>%
    mutate(xr = x*cos(angle) - y*sin(angle),
           yr = x*sin(angle) + y*cos(angle),
           x = xr + mean(x) - mean(xr),
           y = yr + mean(y) - mean(yr)) %>%
    select(-xr, -yr)
}

n_iter <- 25
rotated <- accumulate(1:(n_iter-1), ~rotate(object, .y*2*pi/n_iter), .init = object) %>%
  reduce(rbind) %>%
  mutate(iter = rep(1:n_iter, each = nrow(object)))

###### draw objects ###################
# Visual parameters are:
#   curvature, color (single, palette from scico scale, gradient), alpha (single, scale),
#   size(single, scale), background color
#######################################
library(scico)

set_theme <- function(bg_color = "black"){
  theme(plot.background = element_rect(fill = bg_color),
        panel.background = element_rect(fill = bg_color),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
}

curvature <- 0.4
color = "red"
alpha = 1
size <- 0.5
palette <- "bamako"
bg_color = "black"

###### draw a single object ###########
p1 <- ggplot() +
  geom_curve(data = object,
             aes(x = sx, y = sy, xend = x, yend = y, color = rind, alpha = rind, size = rind),
             curvature = curvature,
             color = color,
             #alpha = alpha,
             size = size,
             show.legend = FALSE) +
  #scale_color_scico(palette = palette) +
  #scale_color_gradient2(low = 	"#FFFFFF", mid = "#F0F0F0",  high = "#C0C0C0") +
  scale_alpha(range = c(0.2, 0.8)) +
  #scale_size(range object.size() +
  coord_equal() +  
  set_theme(bg_color = bg_color)

###### draw a split object ###########
p <- ggplot()

for (i in 1:n_splits){
  p <- p +
    geom_curve(data = splits[[i]],
               aes(x = sx, y = sy, xend = x, yend = y, color = rind, alpha = rind, size = rind),
               curvature = (-1)^i * curvature * 0.5*i,
               #color = color,
               alpha = alpha,
               size = size,
               show.legend = FALSE)
}

p3 <- p +
  #scale_color_scico(palette = palette) +
  scale_color_gradient2(low = 	"#264d00", mid = "#446600", high = "#E8E8E8") +
  #scale_alpha(range = c(0.2, 1)) +
  #scale_size(range object.size()
  coord_equal() +  
  set_theme(bg_color = bg_color)

ggsave("output/tripple.png", height = 4, width = 12)

###### animate ####################
library(gganimate)

pa <- ggplot() +
  geom_curve(data = rotated,
             aes(x = sx, y = sy, xend = x, yend = y, color = rind, alpha = rind, size = rind),
             curvature = curvature,
             color = color,
             #alpha = alpha,
             size = size,
             show.legend = FALSE) +
  #scale_color_scico(palette = palette) +
  #scale_color_gradient(low = 	"#334d00", high = "#E8E8E8") +
  scale_alpha(range = c(0.4, 1)) +
  #scale_size(range object.size()
  coord_equal() +  
  set_theme(bg_color = bg_color)

anim <- pa +
  transition_states(iter, transition_length = 1, state_length = 1) +
  ease_aes("sine-in-out")

animate(anim, renderer = magick_renderer(loop = TRUE), nframes = 100, fps = 20,
        height = 400, width = 400)

#anim_save("output/p8_anim.gif", animattion = last_animation())  # default
#dev.off()
