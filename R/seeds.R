#' @title Generate a set of seeds
#' @description For simulation studies a set of seeds corresponding to each repeat can be useful for reproducibility and investigating rare cases. This function creates that `big` and `small` useful for separating studies to have different seeds.
#' @details Integers are sampled up to `.Machine$integer.max` after setting the seed according to `big` and `small`: `big * 1E5 + small`. So for each distinct pair of `big` and `small`, the set of seeds will be different `small < 1E5`.
#' @param big An integer to increment over large groups of simulations, such as a study.
#' @param small An integer to increment faster than `big`, for example over simulations experiments within a study.
#' @export
seeds <- function(big, small, reps = 1000){
  set.seed(big * 1E5 + small)
  sample.int(.Machine$integer.max, size = reps) #about 1E9
}
