data <- read.csv("main12.csv")
ts_data <- ts(data$NTP.JAMBI, frequency = 12)

plot(ts_data, main = "Nilai Tukar Petani Jambi", ylab = "Nilai", xlab = "Waktu")

adf.test(ts_data)

arima_model <- auto.arima(ts_data)
summary(arima_model)

forecast_arima <- forecast(arima_model, h = 12)
residuals_arima <- residuals(arima_model)

Box.test(residuals_arima, lag = 20, type = "Ljung-Box")

residuals_scaled <- rescale(residuals_arima, to = c(0, 1))

lagged_data <- data.frame(
  x1 = lag(residuals_scaled, -1),
  x2 = lag(residuals_scaled, -2),
  x3 = lag(residuals_scaled, -3),
  y = residuals_scaled
)
lagged_data <- na.omit(lagged_data)

set.seed(123)
train_index <- createDataPartition(lagged_data$y, p = 0.8, list = FALSE)
train_data <- lagged_data[train_index, ]
test_data <- lagged_data[-train_index, ]

ann_model <- nnet(y ~ ., data = train_data, size = 5, linout = TRUE, maxit = 500)

pred_ann <- predict(ann_model, test_data[, -4])

forecast_start <- time(test_data)[1]
arima_forecast_test <- window(forecast(arima_model, h = length(pred_ann))$mean, start = forecast_start)

final_forecast <- arima_forecast_test + pred_ann

plot(final_forecast, type = "l", col = "blue", lwd = 2,
     main = "Prediksi Hybrid: ARIMA + ANN", ylab = "Nilai Tukar Petani")
lines(ts_data[(length(ts_data)-length(pred_ann)+1):length(ts_data)], col = "red", lty = 2)
legend("topleft", legend = c("Prediksi Hybrid", "Aktual"), col = c("blue", "red"), lty = c(1,2))

