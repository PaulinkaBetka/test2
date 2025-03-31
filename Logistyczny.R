library(deSolve)
rm(list=ls())

# Definiujemy układ równań różniczkowych
model <- function(t, y, par) {
  with(as.list(c(y, par)), {  # Dodajemy par do zmiennych w obrębie funkcji
    dydt <- y * r * ((K - y) / K)  # Równanie ODE
    list(dydt)
  })
}

# Warunki początkowe
y0 <- c(y = 25)

# Zakres czasu
time <- seq(0, 100, by = 0.01)

# Parametry
par <- c(r = 0.2, K = 5)

# Rozwiązanie za pomocą lsode
solution <- lsode(y = y0, times = time, func = model, parms = par)

# Wizualizacja wyników
plot(solution[, "time"], solution[, "y"], type = "l", col = "blue", lwd = 2,
     xlab = "Czas", ylab = "y", main = "Rozwiązanie ODE za pomocą lsode")