library(forecast)
library(tseries)
library(zoo)

# Estimasi lambda dan transformasi Box-Cox
lambda <- BoxCox.lambda(data_ts)
data_ts_boxcox <- BoxCox(data_ts, lambda)

# Plot sebelum dan sesudah Box-Cox
par(mfrow=c(2,1))
plot(data_ts, main="Sebelum Transformasi Box-Cox", ylab="NTP JAMBI")
plot(data_ts_boxcox, main=paste("Setelah Box-Cox (λ =", round(lambda, 3), ")"), ylab="Transformed")

# Uji ADF
cat("ADF Sebelum Box-Cox:\n")
print(adf.test(data_ts))
cat("\nADF Sesudah Box-Cox:\n")
print(adf.test(data_ts_boxcox))

# Rolling variance untuk lihat kestabilan variansi
rollvar_before <- rollapply(data_ts, width = 12, FUN = var, fill = NA, align = 'right')
rollvar_after  <- rollapply(data_ts_boxcox, width = 12, FUN = var, fill = NA, align = 'right')

plot(rollvar_before, type = 'l', col = 'red', ylab = 'Rolling Variance', main = 'Rolling Variance Sebelum & Sesudah Box-Cox')
lines(rollvar_after, col = 'blue')
legend("topright", legend = c("Sebelum", "Sesudah"), col = c("red", "blue"), lty = 1)

