library(tidyverse)
library(ambient)
library(ggforce)
library(camcorder)

# Background art generation -----------------------------------------------

# UCR color gradient
colors <- c("#9c9191", "#910a14", "#dc1624", "#db535d", "#ffffff")

## background images

# random fractal noise selector
sample_bg_noise <- function(seed) {
  set.seed(seed)
  
  noises <- c(gen_simplex, gen_perlin, gen_cubic)
  octaves <- 2^(0:5)
  
  list(noise = sample(noises, 1), 
       octaves = sample(octaves, 1))
}

# define plot data
build_art <- function(data, seed) {
  
  # select noise parameters
  bg_params <- sample_bg_noise(seed)
  
  set.seed(seed)
  
  # generate id as fractal noise
  data |> 
    mutate(id = fracture(bg_params$noise[[1]], fbm, 
                         octaves = bg_params$octave, x = x, y = y),
           id = normalize(id))
}

# plot raster image colored by id
draw_art <- function(d, palette) {
  d |> 
    ggplot(aes(x = x, y = y)) +
    geom_raster(aes(fill = id), alpha = 0.5) +
    scale_fill_gradientn(colors = palette) +
    scale_size_continuous(range = c(0.02, 4)) +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(-1, -1, -1, -1, "in"))
}

# initialize data
d <- long_grid(x = seq(0, 1, length.out = 1000),
               y = seq(0, 1, length.out = 1000)) 

# generate images with selected seeds
walk(c(3,4,8,14,18,26,37,47,52,59),
     ~ build_art(d, .x) |>
       draw_art(colors) |>
       ggsave(paste0("assets/bg", ..1, ".png"), plot = _,
              height = 12, width = 20))


# GIFs

# random noise selector for points
sample_noise <- function(seed) {
  set.seed(seed)
  
  noises <- c(gen_perlin)
  frequency <- seq(0.125, 4, 0.125)
  octaves <- 2^(0:5)
  step <- 10^(-3:-1)
  
  list(noise = sample(noises, 1), 
       frequency = sample(frequency, 1),
       octaves = sample(octaves, 1),
       step = sample(step, 1))
}

# add fractal noise to original data
add_fractal_curl <- function(data, params) {
  curl <- curl_noise(generator = params$noise[[1]], x = data$x, y = data$y, 
                     fractal = fbm, frequency = params$frequency, octaves = params$octaves)
  data |> mutate(x = x + params$step * curl$x, 
                 y = y + params$step * curl$y,
                 iter = iter + 1)
}

# define plot data
build_art <- function(data, seed, n, bg_res = 800) {
  
  # select background and point noise parameters
  noise_params <- sample_noise(seed)
  bg_params <- sample_bg_noise(seed)
  
  set.seed(seed)
  
  # define background as fractal noise
  bg <- long_grid(x = seq(0, 1, length.out = bg_res),
                  y = seq(0, 1, length.out = bg_res)) |> 
    mutate(id = fracture(bg_params$noise[[1]], fbm, 
                         octaves = bg_params$octave, x = x, y = y),
           id = normalize(id),
           cat = "bg")
  
  # generate n iterations of curl for the points
  data |> 
    mutate(id = gen_perlin(x, y),
           iter = 1,
           cat = "point") |> 
    accumulate(1:n, ~add_fractal_curl(.x, noise_params), .init = _) |> 
    bind_rows() |> 
    mutate(across(c(x, y, id), normalize)) |> 
    bind_rows() |> 
    bind_rows(bg) |> 
    as_tibble()
}

# plot points on raster background
draw_art <- function(d, maxn, palette) {
  # show only past 30 iterations of curl
  d <- d |> filter((iter <= maxn & iter > maxn - 30) | is.na(iter))
  d |> 
    ggplot(aes(x = x, y = y)) +
    geom_raster(data = filter(d, cat == "bg"), aes(fill = id)) +
    geom_point(data = filter(d, cat == "point"), 
               aes(color = id, size = -iter, alpha = iter)) +
    scale_color_gradientn(colors = palette) +
    scale_fill_gradientn(colors = palette) +
    scale_size_continuous(range = c(0.06, 12)) +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(-2, -2, -2, -2, "in"))
}

# initialize points as asymmetrical grid
d <- long_grid(x = seq(0, 1, length.out = 18),
               y = seq(0, 1, length.out = 14)) |> 
  mutate(x = 1-sqrt(x), y = 1-sqrt(y))

# generate gifs with selected seeds
#gif5=seed2
#gif6=seed51
#gif4=seed35
plot_data <- d |> build_art(51, n = 180, bg_res = 400)

# record plots to tempfile
gg_record(
  dir = file.path(tempfile()),
  device = "png", # device to use to save images
  width = 10,     # width of saved image
  height = 6,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)
for (i in c(seq(60, 180, 2), seq(180, 60, -2))) {
  draw_art(plot_data, i, colors) |> print()
}

# save GIF
gg_playback(
  name = "assets/gif6.gif",
  first_image_duration = 0.5,
  last_image_duration = 0.5,
  frame_duration = .15,
  background = "white"
)

# delete tempfiles
gg_stop_recording()