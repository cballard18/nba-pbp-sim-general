# Import the win-probability neural net ensemble from the "shot optimization - refresh" repo
# (https://github.com/cballard18/NBA-Shot-Optimization) into inst/models/wp_nn.rds.
#
# Each of the 20 nets is tiny (4 inputs -> 16 relu -> 8 relu -> 1 sigmoid), so only the
# weights and the recipe's 0-1 scaling ranges are kept and the forward pass runs in plain R
# (R/wp.R). nbasim then needs neither keras nor recipes. keras3 and recipes are only needed here.
#
# Usage, from the package root:
#   Rscript data-raw/import_wp.R ["path/to/shot optimization - refresh"]
devtools::load_all(quiet = TRUE)
suppressMessages({ library(keras3); library(recipes) })

args <- commandArgs(trailingOnly = TRUE)
src <- if (length(args)) args[1] else "~/Desktop/Sports/Blog/shot optimization - refresh"
src <- path.expand(src)

rec <- readRDS(file.path(src, "models/wp_recipe.rds"))
step <- rec$steps[[1]]
stopifnot(inherits(step, "step_range"), isTRUE(step$clipping))
inputs <- c("home_margin", "home_team_spread", "end_game_seconds_remaining", "home_team_pos")
stopifnot(identical(colnames(step$ranges), inputs))

paths <- sort(list.files(file.path(src, "models/ensemble"), "^wp_b\\d+\\.keras$", full.names = TRUE))
stopifnot(length(paths) > 0)
nets <- lapply(paths, function(p) {
  w <- get_weights(load_model(p))
  stopifnot(length(w) %% 2 == 0)
  lapply(seq(1, length(w), 2), function(i) list(W = w[[i]], b = as.numeric(w[[i + 1]])))
})
wp_nn <- list(inputs = inputs, min = step$ranges["mins", ], max = step$ranges["maxs", ], nets = nets,
              source = "NBA-Shot-Optimization (shot optimization - refresh)")

# check the plain-R forward pass against keras + recipes, including out-of-range inputs
set.seed(1)
nd <- data.frame(home_margin = sample(-80:80, 5000, TRUE), home_team_spread = runif(5000, -25, 25),
                 end_game_seconds_remaining = runif(5000, 0, 3000), home_team_pos = sample(0:1, 5000, TRUE),
                 home_win = 0L)
x <- as.matrix(bake(rec, new_data = nd)[, inputs])
ref <- rowMeans(sapply(paths, function(p) as.numeric(predict(load_model(p), x, verbose = 0))))
ours <- nn_predict(wp_nn, as.matrix(nd[, inputs]))
cat("max abs difference vs keras:", max(abs(ref - ours)), "\n")
stopifnot(max(abs(ref - ours)) < 1e-5)

saveRDS(wp_nn, "inst/models/wp_nn.rds")
cat("wrote inst/models/wp_nn.rds (", length(nets), "nets )\n")
