plot_prediction_region <- function(x, y, pred_vec, test_grid, ...){
  stopifnot(ncol(x) == 2, nrow(x) == length(y), !is.matrix(y), is.matrix(x), all(abs(x) <= 5), 
            all(y %in% c(0,1)))
  stopifnot(length(pred_vec) == nrow(test_grid), sqrt(nrow(test_grid) %% 1) == 0,
            all(pred_vec %in% c(0,1)))
  
  grid_val <- test_grid[1:sqrt(nrow(test_grid)),1]
  stopifnot(all(grid_val == sort(grid_val, decreasing = F)), grid_val[1] == -5, 
            grid_val[length(grid_val)] == 5)
  
  pred_mat <- matrix(pred_vec, nrow = length(grid_val))
  plot(x[,1], x[,2], col = c(1:2)[y+1], ...)
  graphics::.filled.contour(grid_val, grid_val, z = pred_mat, 
                            col = c(rgb(0.5,0.5,0.5,0.5), rgb(0.5,0,0,0.5)), 
                            levels = c(-0.5, 0.5, 1.5))
  
  invisible()
}
