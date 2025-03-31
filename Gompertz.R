library(deSolve)
rm(list=ls())
# Definiujemy układ równań różniczkowych
model <- function(t, y, par) {
  with(as.list(y), {
    dydt <- y * exp(-b*exp(z*t)-1)  # Przykładowe równanie ODE: dy/dt = -0.5 * y
    list(dydt)
  })
}

# Warunki początkowe
y0 <- c(y = 0.5) 

# Zakres czasu
time <- seq(0, 100, by = 0.01)

b <- c(r=0.125)
z <- c(a=5)

par <- c(b, z)

# Rozwiązanie za pomocą lsode
solution <- lsode(y = y0, times = time, func = model, parms = par)

# Wizualizacja wyników
plot(solution[, "time"], solution[, "y"], type = "l", col = "blue", lwd = 2,
     xlab = "Czas", ylab = "y", main = "Rozwiązanie ODE za pomocą lsode")