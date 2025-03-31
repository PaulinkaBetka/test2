library(deSolve)
rm(list=ls())
# Definiujemy układ równań różniczkowych
model <- function(t, y, r) {
  with(as.list(y), {
    dydt <- r * y  # Przykładowe równanie ODE: dy/dt = -0.5 * y
    list(dydt)
  })
}

# Warunki początkowe
y0 <- c(y = 0.5)

# Zakres czasu
time <- seq(0, 100
            , by = 0.01)

r <- c(r=0.125)

# Rozwiązanie za pomocą lsode
solution <- lsode(y = y0, times = time, func = model, parms = r)

# Wizualizacja wyników
plot(solution[, "time"], solution[, "y"], type = "l", col = "blue", lwd = 2,
     xlab = "Czas", ylab = "y", main = "Rozwiązanie ODE za pomocą lsode")
