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


library(ggplot2)
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

















# exercice 2 : 

# Partie 1 
set.seed(123)

# Paramètres
n <- 200  # Nombre d'observations
sigma2 <- 1/4  # Variance du bruit

epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma2))  # Génération du bruit blanc

# Simulation des processus
Y_DT <- numeric(n)
Y_TS <- numeric(n)

for (t in 1:n) {
  Y_DT[t] <- 0.2 * t + epsilon[t]  
  if (t == 1) {
    Y_TS[t] <- epsilon[t]  
  } else {
    Y_TS[t] <- 0.2 + Y_TS[t - 1] + epsilon[t]  
  }
}

# Création du DataFrame
data <- data.frame(
  Time = rep(1:n, 2),
  Value = c(Y_DT, Y_TS),
  Process = rep(c("DT: Yt = 0.2t + εt", "TS: Yt = 0.2 + Yt-1 + εt"), each = n)
)

# Graphique
ggplot(data, aes(x = Time, y = Value, color = Process)) +
  geom_line() +
  labs(title = "Comparaison entre un processus déterministe et un processus stochastique dans une loi N(0,1/4)",
       x = "Temps",
       y = "Valeur de la série") +
  theme_minimal()
  


# Partie 2 1/2

set.seed(123)

# Paramètres
n <- 200  # Nombre d'observations
sigma2 <- 1/2  # Variance du bruit

epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma2))  # Génération du bruit blanc

# Simulation des processus
Y_DT <- numeric(n)
Y_TS <- numeric(n)

for (t in 1:n) {
  Y_DT[t] <- 0.2 * t + epsilon[t]  
  if (t == 1) {
    Y_TS[t] <- epsilon[t]  
  } else {
    Y_TS[t] <- 0.2 + Y_TS[t - 1] + epsilon[t]  
  }
}

# Création du DataFrame
data <- data.frame(
  Time = rep(1:n, 2),
  Value = c(Y_DT, Y_TS),
  Process = rep(c("DT: Yt = 0.2t + εt", "TS: Yt = 0.2 + Yt-1 + εt"), each = n)
)

# Graphique
ggplot(data, aes(x = Time, y = Value, color = Process)) +
  geom_line() +
  labs(title = "Comparaison entre un processus déterministe et un processus stochastique dans une loi N(0,1/2)",
       x = "Temps",
       y = "Valeur de la série") +
  theme_minimal()

# Partie 3 1

set.seed(123)

# Paramètres
n <- 200  # Nombre d'observations
sigma2 <- 1  # Variance du bruit

epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma2))  # Génération du bruit blanc

# Simulation des processus
Y_DT <- numeric(n)
Y_TS <- numeric(n)

for (t in 1:n) {
  Y_DT[t] <- 0.2 * t + epsilon[t]  
  if (t == 1) {
    Y_TS[t] <- epsilon[t]  
  } else {
    Y_TS[t] <- 0.2 + Y_TS[t - 1] + epsilon[t]  
  }
}

# Création du DataFrame
data <- data.frame(
  Time = rep(1:n, 2),
  Value = c(Y_DT, Y_TS),
  Process = rep(c("DT: Yt = 0.2t + εt", "TS: Yt = 0.2 + Yt-1 + εt"), each = n)
)

# Graphique
ggplot(data, aes(x = Time, y = Value, color = Process)) +
  geom_line() +
  labs(title = "Comparaison entre un processus déterministe et un processus stochastique dans une loi N(0,1)",
       x = "Temps",
       y = "Valeur de la série") +
  theme_minimal()





# Exercice 3 : Régressions fallacieuses


set.seed(123)

# Paramètres
n <- 200       # Longueur des séries
N <- 5000      # Nombre de simulations
rejets <- numeric(N)

for (i in 1:N) {
  # Simuler deux marches aléatoires indépendantes
  X <- cumsum(rnorm(n))
  Y <- cumsum(rnorm(n))
  
  # Régression Y ~ X
  model <- lm(Y ~ X)
  p_value <- summary(model)$coefficients[2, 4]  # p-valeur du coefficient de X
  
  # Rejet de H0 si p-value < 0.05
  rejets[i] <- ifelse(p_value < 0.05, 1, 0)
}

# Pourcentage de rejets
pourcentage_rejets <- mean(rejets) * 100
cat("Pourcentage de rejets de H0 (β1 = 0) au seuil de 5% :", round(pourcentage_rejets, 2), "%\n")





























