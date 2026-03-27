# ==============================================================================
# SOP: Network Analysis & Dyadic Regression Models (AME)
# BUSINESS CASE: Analyzing flows (trade, communications, supply chain) between 
#                entities and predicting link formation/intensity.
# ==============================================================================

# Libraries
library(amen)
library(igraph)
library(lattice)

# ------------------------------------------------------------------------------
# 1. NETWORK STRUCTURE & CENTRALITY (Exploratory Data Analysis)
# ------------------------------------------------------------------------------

# TIP: Input must be an adjacency matrix (Y). 
# Mode: 'directed' for asymmetric flows (Exports), 'undirected' for relations.

get_network_metrics <- function(adj_matrix, directed = TRUE) {
  # Fix NAs for igraph compatibility
  adj_matrix[is.na(adj_matrix)] <- 0
  mode <- ifelse(directed, "directed", "undirected")
  
  net <- graph_from_adjacency_matrix(adj_matrix, mode = mode, weighted = TRUE, diag = FALSE)
  
  metrics <- list(
    density    = edge_density(net),
    diameter   = diameter(net),
    reciprocity = reciprocity(net),
    centrality = betweenness(net) # High betweenness = key 'bridge' nodes
  )
  return(metrics)
}

# ------------------------------------------------------------------------------
# 2. SOCIAL RELATION MODEL (SRM) - Variance Decomposition
# ------------------------------------------------------------------------------

# TIP: Use SRM to decompose the variance into sender effects, receiver effects, 
# and dyadic correlation (reciprocity).

fit_srm_model <- function(Y) {
  # Y: Matrix of dyadic relations (e.g., log-exports)
  # family: "nrm" (Normal), "bin" (Binary/Probit), "ord" (Ordinal)
  
  fit <- ame(Y, family = "nrm", plot = FALSE, print = FALSE)
  
  # Strategic Interpretation:
  # - fit$APM: Sender effects (Who drives the flow?)
  # - fit$BPM: Receiver effects (Who attracts the flow?)
  # - cor(ahat, bhat): Correlation between sending and receiving potential
  
  return(fit)
}

# ------------------------------------------------------------------------------
# 3. ADVANCED MULTIPLICATIVE EFFECTS (AME) - Regression with Latent Factors
# ------------------------------------------------------------------------------

# TIP: AME is the 'Gold Standard' for network data as it accounts for:
# 1. Nodal covariates (Xr, Xc) - e.g., GDP, Population
# 2. Dyadic covariates (Xd) - e.g., Distance, Common Language
# 3. Latent factors (R) - Captures unobserved third-order dependencies (transitivity)

run_strategic_network_regression <- function(Y, X_nodal, X_dyadic, R = 2) {
  
  # Y: Dependent variable matrix
  # Xd: Array of dyadic predictors
  # Xr/Xc: Matrices of nodal predictors (senders/receivers)
  # R: Number of latent factors (R=2 is usually sufficient for clustering)
  
  fit_ame <- ame(Y, Xd = X_dyadic, Xr = X_nodal, Xc = X_nodal, R = R, family = "nrm")
  
  # TIP: Check summary(fit_ame) for 'BETA' coefficients (Direct impacts)
  # Use circplot() to visualize latent communities/clusters
  
  return(fit_ame)
}

# ------------------------------------------------------------------------------
# EXAMPLE WORKFLOW (Application on IR90s Trade Data)
# ------------------------------------------------------------------------------
data(IR90s)
gdp <- IR90s$nodevars[,2]
top_idx <- which(gdp >= sort(gdp, decreasing = TRUE)[30])

# Pre-processing
Y_trade <- log(IR90s$dyadvars[top_idx, top_idx, 2] + 1)
X_node  <- log(IR90s$nodevars[top_idx, 1:2] + 1) # GDP and Polity
X_dyad  <- IR90s$dyadvars[top_idx, top_idx, c(1,3,4)] # Distance, Conflict, etc.

# Model Execution
final_model <- run_strategic_network_regression(Y_trade, X_node, X_dyad)
summary(final_model)
