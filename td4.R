set.seed(123)

# Paramètres
n <- 200
phi_values <- c(0.5, 0.9, 1)  # Valeurs de phi
shock_value <- 20
shock_time <- 100

# Fonction de simulation AR(1) avec choc
simulate_AR1 <- function(phi, n, shock_time, shock_value) {
  e <- rnorm(n)
  y <- numeric(n)
  for (t in 2:n) {
    y[t] <- phi * y[t-1] + e[t]
    if (t == shock_time) y[t] <- y[t] + shock_value
  }
  return(y)
}

# Simulation des 3 séries
series <- lapply(phi_values, simulate_AR1, n = n, shock_time = shock_time, shock_value = shock_value)

# Tracer les séries
plot(series[[1]], type = "l", col = "blue", ylim = range(unlist(series)),
     ylab = "Valeur", xlab = "Temps", main = "AR(1) avec choc à t=100")
lines(series[[2]], col = "green")
lines(series[[3]], col = "red")
legend("topleft", legend = c("phi = 0.5", "phi = 0.9", "phi = 1 (racine unitaire)"),
       col = c("blue", "green", "red"), lwd = 2)


==> Choc permanent 



simulate_ar1 <- function(phi, choc_value, choc_time = 100, n = 200) {
  Y <- numeric(n)
  eps <- rnorm(n, mean = 0, sd = 1)
  
  for (t in 2:n) {
    Y[t] <- phi * Y[t - 1] + eps[t]
    if (t == choc_time) {
      Y[t] <- Y[t] + choc_value
    }
  }
  return(Y)
}

phi_vals <- c(0.5, 0.9, 1.0)
chocs <- c(20, 40, -20, -40)

# Stocker les résultats dans une data.frame
results <- data.frame()

for (choc in chocs) {
  for (phi in phi_vals) {
    serie <- simulate_ar1(phi, choc)
    df <- data.frame(
      t = 1:200,
      Y = serie,
      phi = paste0("phi = ", phi),
      choc = paste0("Choc = ", choc)
    )
    results <- rbind(results, df)
  }
}
ggplot(results, aes(x = t, y = Y, color = phi)) +
  geom_line() +
  facet_wrap(~choc, scales = "free_y") +
  labs(title = "Impact de chocs à t=100 pour différentes valeurs de ϕ₁",
       x = "Temps", y = "Y_t") +
  theme_minimal() +
  theme(legend.position = "top")

