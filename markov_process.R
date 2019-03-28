library(gtools)
markov_process <- function(n_nodes, params = list()) {
  # check input is an integer value
  if (length(n_nodes) != 1) stop("n_nodes must be a single scalar integer")
  if (class(n_nodes) %in% c("integer", "numeric")) {
    if (as.integer(n_nodes) == n_nodes) {
      n_nodes <- as.integer(n_nodes)
    } else {
      stop("n_nodes must be integer")
    }
  } else {
    stop("n_nodes must be integer")
  }
  if (n_nodes < 2) stop("n_nodes must be 2 or more")
  
  node_init <- "uniform"
  if (!is.null(params$beta_priors)) {
    if (class(params$beta_priors) == "matrix") {
      if (ncol(params$beta_priors) != n_nodes | 
          nrow(params$beta_priors) != n_nodes) stop("beta_priors should be square matrix of size n_nodes")
      } else {
      stop("beta_priors should a matrix")      
    }
    node_init <- "beta"
  }
  
  # set up the nodes
  nodes <- list()
  for (n in 1:n_nodes) {
    if (node_init == "uniform") {
      # by uniform dist
      probs <- runif(n_nodes, 0, 1)
      nodes[[n]] <- probs/sum(probs)  
    }
    
    # by beta / dirichlet
    if (node_init == "beta") {
      # by uniform dist
      nodes[[n]] <- as.vector(rdirichlet(1, alpha = params$beta_priors[n, ]))
    }
    
  }
  return(nodes)
}
