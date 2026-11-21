---
title: "Code_Sample_Roger"
author: "Roger Zamora"
date: "10/9/2024"
output:
  pdf_document: default
  html_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# R Coding Sample
```{r}
# This is a coding sample from an independent research study I conducted
# Titled: "Essays on Remittance Costs and Their Impact on GDP:
# Forecasts for 2024 and 2025, Evidence from Honduras.
# All micro-data was sourced from the World Bank's Remittance
# Prices Worldwide database, available at "http://remittanceprices.worldbank.org, 2024."
# This example demonstrates my coding proficiency;
# however, a more complex code sample can be provided if necessary.

getwd()
setwd("/Users/rogerzamora/Documents/ECONOMETRIA")

# Load necessary libraries for data handling, forecasting, and visualization
library(readxl)
library(forecast)
library(ggplot2)
library(tseries)
library(patchwork)

datos <- read_excel('arimas.xlsx')
#View(datos)

```
```{r}
# These variables represent important economic indicators related to remittances and GDP in Honduras
tri <- datos$trimestre  # Quarter (time period variable)
costos200_esp <- datos$costo_200_ESPHND  # Cost of remittances of 200 USD from Spain to Honduras
costos500_esp <- datos$costo_500_ESPHND  # Cost of remittances of 500 USD from Spain to Honduras
costos200_usa <- datos$costo_200_USAHND  # Cost of remittances of 200 USD from the U.S. to Honduras
costos500_usa <- datos$costo_500_USAHND  # Cost of remittances of 500 USD from the U.S. to Honduras
PIB <- datos$PIB  # Gross Domestic Product (GDP) of Honduras
remesas <- datos$remesas  # Total remittances to Honduras
esp <- datos$esp  # Remittances from Spain to Honduras
usa <- datos$usa  # Remittances from the U.S. to Honduras
```

```{r}
# Create time series objects for each economic indicator, starting from 2013 with quarterly frequency
remesas_ts <- ts(remesas, start = c(2013, 1), frequency = 4)
PIB_ts <- ts(PIB, start = c(2013, 1), frequency = 4)
costos200_esp_ts <- ts(costos200_esp, start = c(2013, 1), frequency = 4)
costos500_esp_ts <- ts(costos500_esp, start = c(2013, 1), frequency = 4)
costos200_usa_ts <- ts(costos200_usa, start = c(2013, 1), frequency = 4)
costos500_usa_ts <- ts(costos500_usa, start = c(2013, 1), frequency = 4)
esp_ts <- ts(esp, start = c(2013, 1), frequency = 4)
usa_ts <- ts(usa, start = c(2013, 1), frequency = 4)
```

```{r}
plot(PIB_ts, main = "Time-series", ylab = "GDP", xlab = "Quarter", col = "blue")

par(mfrow = c(3, 2))  
plot(remesas_ts, main = "Total Remittances", ylab = "Millons of USD", col = "blue")
plot(PIB_ts, main = "GDP Quarterly", ylab = "Millions of USD", col = "green")
plot(costos200_esp_ts, main = "200$ Remittance Costs from Spain", ylab = "Costo (%)", col = "red")
plot(costos500_esp_ts, main = "500$ Remittance Costs from Spain", ylab = "Costo (%)", col = "purple")
plot(costos200_usa_ts, main = "200$ Remittance Costs from USA", ylab = "Costo (%)", col = "orange")
plot(costos500_usa_ts, main = "500$ Remittance Costs from USA", ylab = "Costo (%)", col = "brown")

par(mfrow = c(1, 1))

```

```{r}
# Apply the Augmented Dickey-Fuller (ADF) test to assess stationarity for each time series
# Stationarity is a key assumption for many time series models, including ARIMA
adf_remesas <- adf.test(remesas_ts)
adf_PIB <- adf.test(PIB_ts)
adf_costos200_esp <- adf.test(costos200_esp_ts)
adf_costos500_esp <- adf.test(costos500_esp_ts)
adf_costos200_usa <- adf.test(costos200_usa_ts)
adf_costos500_usa <- adf.test(costos500_usa_ts)
adf_esp <- adf.test(esp_ts)
adf_usa <- adf.test(usa_ts)

# Output the results of the ADF tests for each time series
# A p-value < 0.05 indicates stationarity, meaning the series does not have a unit root
cat("ADF Test Results:\n")
cat("Total Remittances:\n", adf_remesas$p.value, "\n")
cat("GDP:\n", adf_PIB$p.value, "\n")
cat("200$ Remittance Costs from Spain:\n", adf_costos200_esp$p.value, "\n")
cat("500$ Remittance Costs from Spain:\n", adf_costos500_esp$p.value, "\n")
cat("200$ Remittance Costs from USA:\n", adf_costos200_usa$p.value, "\n")
cat("500$ Remittance Costs from USA\n", adf_costos500_usa$p.value, "\n")
cat("Spanish Remittances:\n", adf_esp$p.value, "\n")
cat("U.S. Remittances:\n", adf_usa$p.value, "\n")
```

```{r}
# Differentiating the time series to make them stationary, if necessary
# Differencing removes trends and makes the series suitable for ARIMA modeling
diff_remesas_ts <- diff(remesas_ts)
diff_PIB_ts <- diff(PIB_ts)
diff_costos200_esp_ts <- diff(costos200_esp_ts)
diff_costos500_esp_ts <- diff(costos500_esp_ts)
diff_costos200_usa_ts <- diff(costos200_usa_ts)
diff_costos500_usa_ts <- diff(costos500_usa_ts)
diff_esp_ts <- diff(esp_ts)
diff_usa_ts <- diff(usa_ts)
```

```{r}
# Apply the ADF test again to the differentiated series to confirm stationarity
adf_diff_remesas <- adf.test(diff_remesas_ts)
adf_diff_PIB <- adf.test(diff_PIB_ts)
adf_diff_costos200_esp <- adf.test(diff_costos200_esp_ts)
adf_diff_costos500_esp <- adf.test(diff_costos500_esp_ts)
adf_diff_costos200_usa <- adf.test(diff_costos200_usa_ts)
adf_diff_costos500_usa <- adf.test(diff_costos500_usa_ts)
adf_diff_esp <- adf.test(diff_esp_ts)
adf_diff_usa <- adf.test(diff_usa_ts)

# Output the ADF test results for the differentiated series
cat("ADF Test Results for Differentiated Series:\n")
cat("Differenced Total Remittances:\n", adf_diff_remesas$p.value, "\n")
cat("Differenced GDP:\n", adf_diff_PIB$p.value, "\n")
cat("Differenced 200$ Remittance Costs from Spain:\n", adf_diff_costos200_esp$p.value, "\n")
cat("Differenced 500$ Remittance Costs from Spain:\n", adf_diff_costos500_esp$p.value, "\n")
cat("Differenced 200$ Remittance Costs from USA:\n", adf_diff_costos200_usa$p.value, "\n")
cat("Differenced 500$ Remittance Costs from USA:\n", adf_diff_costos500_usa$p.value, "\n")
cat("Differenced Spanish Remittances:\n", adf_diff_esp$p.value, "\n")
cat("Differenced Remittances from the U.S.:\n", adf_diff_usa$p.value, "\n")
```

```{r}
# Fit ARIMA models to the time series
# ARIMA models are commonly used for forecasting economic time series
auto_arima_remesas <- auto.arima(remesas_ts)
auto_arima_PIB <- auto.arima(PIB_ts)
auto_arima_costos200_esp <- auto.arima(costos200_esp_ts)
auto_arima_costos500_esp <- auto.arima(costos500_esp_ts)
auto_arima_costos200_usa <- auto.arima(costos200_usa_ts)
auto_arima_costos500_usa <- auto.arima(costos500_usa_ts)
auto_arima_esp <- auto.arima(esp_ts)
auto_arima_usa <- auto.arima(usa_ts)
```

```{r}
# Output the summary of ARIMA models to understand their performance
summary(auto_arima_remesas)
summary(auto_arima_PIB)
summary(auto_arima_costos200_esp)
summary(auto_arima_costos500_esp)
summary(auto_arima_costos200_usa)
summary(auto_arima_costos500_usa)
summary(auto_arima_esp)
summary(auto_arima_usa)
```

```{r}
# Plot residuals and check model diagnostics for the model
checkresiduals(auto_arima_remesas)
checkresiduals(auto_arima_PIB)
checkresiduals(auto_arima_costos200_esp)
checkresiduals(auto_arima_costos500_esp)
checkresiduals(auto_arima_costos200_usa)
checkresiduals(auto_arima_costos500_usa)
checkresiduals(auto_arima_esp)
checkresiduals(auto_arima_usa)
```

```{r}
library(ggplot2)
library(forecast)
library(patchwork)

par(family = "serif")
plot_density_residuals <- function(model, title) {
  residuals_data <- residuals(model)
  ggplot(data = data.frame(residuals = residuals_data), aes(x = residuals)) +
    geom_density(fill = "purple", alpha = 0.5) +
    ggtitle(title) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, family = "serif"),  
      axis.title.x = element_text(family = "serif"),  
      axis.text.x = element_text(family = "serif"),  
      axis.title.y = element_blank(),  
      axis.text.y = element_text(family = "serif"),
      legend.position = "none"  
    )
}

density_costo200esp <- plot_density_residuals(auto_arima_costos200_esp, "Density of Residuals - 200 USD ESP")
density_costo500esp <- plot_density_residuals(auto_arima_costos500_esp, "Density of Residuals - 500 USD ESP")
density_costo200usa <- plot_density_residuals(auto_arima_costos200_usa, "Density of Residuals - 200 USD USA")
density_costo500usa <- plot_density_residuals(auto_arima_costos500_usa, "Density of Residuals - 500 USD USA")

combined_density_plot <- (
  (density_costo200esp | density_costo500esp) /
  (density_costo200usa | density_costo500usa)
)

combined_density_plot

```

```{r}
library(ggplot2)
library(forecast)
library(patchwork)

plot_density_residuals <- function(model, title) {
  residuals_data <- residuals(model)
  ggplot(data = data.frame(residuals = residuals_data), aes(x = residuals)) +
    geom_density(fill = "purple", alpha = 0.5) +
    ggtitle(title) +
    xlab("Residuals") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, family = "serif"),  
      axis.title.x = element_text(family = "serif"),  
      axis.text.x = element_text(family = "serif"),  
      axis.title.y = element_blank(),  
      axis.text.y = element_text(family = "serif"),
      legend.position = "none"  
    )
}

density_PIB <- plot_density_residuals(auto_arima_PIB, "ARIMA (0,1,0) - Quarterly GDP at Market Prices")
density_remesas <- plot_density_residuals(auto_arima_remesas, "ARIMA (0,1,0) - Total Remittances")
density_remesas_esp <- plot_density_residuals(auto_arima_esp, "ARIMA (0,1,0) - Total Remittances ESP-HND")
density_remesas_usa <- plot_density_residuals(auto_arima_usa, "ARIMA (0,1,0) - Total Remittances USA-HND")

combined_density_plot <- (
  (density_PIB | density_remesas) /
  (density_remesas_esp | density_remesas_usa)
)

combined_density_plot

```

```{r}

library(ggplot2)
library(forecast)
library(patchwork)

plot_acf <- function(model, title) {
  residuals_data <- residuals(model)
  acf_data <- acf(residuals_data, plot = FALSE)  
  n <- length(residuals_data)
  ci <- 1 / sqrt(n)  
 
  ggplot(data = data.frame(lag = acf_data$lag, acf = acf_data$acf)) +
    geom_bar(aes(x = lag, y = acf), stat = "identity", fill = "purple", alpha = 0.5) +
    geom_hline(yintercept = ci, linetype = "dashed", color = "black") +
    geom_hline(yintercept = -ci, linetype = "dashed", color = "black") +
    ggtitle(title) +
    xlab("Lag") +  
    ylab("ACF") +  
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, family = "serif"),  
      axis.title.x = element_text(family = "serif"),  
      axis.title.y = element_text(family = "serif"),  
      axis.text.x = element_text(family = "serif"),  
      axis.text.y = element_text(family = "serif")  
    )
}

acf_costos200_esp <- plot_acf(auto_arima_costos200_esp, "ARIMA (2,1,2) - 200 USD ESP-HND Costs")
acf_costos500_esp <- plot_acf(auto_arima_costos500_esp, "ARIMA (3,1,0) - 500 USD ESP-HND Costs")
acf_costos200_usa <- plot_acf(auto_arima_costos200_usa, "ARIMA (0,1,0) - 200 USD USA-HND Costs")
acf_costos500_usa <- plot_acf(auto_arima_costos500_usa, "ARIMA (1,0,0) - 500 USD USA-HND Costs")

combined_acf_plot <- (
  (acf_costos200_esp | acf_costos500_esp) /
  (acf_costos200_usa | acf_costos500_usa)
)

combined_acf_plot

```

```{r}
library(ggplot2)
library(forecast)
library(patchwork)

plot_acf <- function(model, title) {
  residuals_data <- residuals(model)
  acf_data <- acf(residuals_data, plot = FALSE)  
  n <- length(residuals_data)
  ci <- 1 / sqrt(n)  # 95% confidence interval
 
  ggplot(data = data.frame(lag = acf_data$lag, acf = acf_data$acf)) +
    geom_bar(aes(x = lag, y = acf), stat = "identity", fill = "purple", alpha = 0.5) +
    geom_hline(yintercept = ci, linetype = "dashed", color = "black") +
    geom_hline(yintercept = -ci, linetype = "dashed", color = "black") +
    ggtitle(title) +
    xlab("Lag") +  
    ylab("ACF") +  
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, family = "serif"),
      axis.title.x = element_text(family = "serif"),  
      axis.title.y = element_text(family = "serif"),  
      axis.text.x = element_text(family = "serif"),  
      axis.text.y = element_text(family = "serif")  
    )
}

acf_PIB <- plot_acf(auto_arima_PIB, "ARIMA (0,1,0) - Quarterly GDP at Market Prices")
acf_remesas <- plot_acf(auto_arima_remesas, "ARIMA (0,1,0) - Total Remittances")
acf_remesas_esp <- plot_acf(auto_arima_esp, "ARIMA (0,1,0) - Total Remittances ESP-HND")
acf_remesas_usa <- plot_acf(auto_arima_usa, "ARIMA (0,1,0) - Total Remittances USA-HND")

# Combine the plots into a 2x2 grid using patchwork for easy comparison
combined_acf_plot <- (
  (acf_PIB | acf_remesas) /
  (acf_remesas_esp | acf_remesas_usa)
)
combined_acf_plot

```

```{r}
# Forecast future values for each time series using the fitted ARIMA models
# Forecasting the next 8 quarters (2 years)
forecast_remesas <- forecast(auto_arima_remesas, h = 8)
forecast_PIB <- forecast(auto_arima_PIB, h = 8)
forecast_costos200_esp <- forecast(auto_arima_costos200_esp, h = 8)
forecast_costos500_esp <- forecast(auto_arima_costos500_esp, h = 8)
forecast_costos200_usa <- forecast(auto_arima_costos200_usa, h = 8)
forecast_costos500_usa <- forecast(auto_arima_costos500_usa, h = 8)
forecast_esp <- forecast(auto_arima_esp, h = 8)
forecast_usa <- forecast(auto_arima_usa, h = 8)
```


```{r}
# These plots are important for understanding the future trends in economic variables such as remittances and GDP
par(family = "serif")
plot(forecast_remesas, main = "Forecast of Total Remittances to Honduras", ylab = "Millions of USD")
plot(forecast_PIB, main = "Forecast of Honduras GDP", ylab = "Millions of USD")
plot(forecast_costos200_esp, main = "Forecast of 200 USD Remittance Cost from Spain", ylab = "Percentage")
plot(forecast_costos500_esp, main = "Forecast of 500 USD Remittance Cost from Spain", ylab = "Percentage")
plot(forecast_costos200_usa, main = "Forecast of 200 USD Remittance Cost from the U.S.", ylab = "Percentage")
plot(forecast_costos500_usa, main = "Forecast of 500 USD Remittance Cost from the U.S.", ylab = "Percentage")
plot(forecast_esp, main = "Forecast of Remittances from Spain", ylab = "Millions of USD")
plot(forecast_usa, main = "Forecast of Remittances from the U.S.", ylab = "Millions of USD")

par(mfrow = c(1, 1))
```

# Complementary data regarding remittance costs in Honduras through the U.S. corridor
```{r}
getwd()
setwd("/Users/rogerzamora/Documents/ECONOMETRIA")
library(readxl)
datos=read_excel('eusa_bch.xlsx')
```

```{r}
library(dplyr)
tri= datos$period
salida= datos$source_name
destino= datos$destination_name
empresa= datos$firm
velocidad= datos$`speed actual`
remesa200 = datos$`cc1 denomination amount`
costo200 = datos$`cc1 total cost %`
remesa500 = datos$`cc2 denomination amount`
costo500 = datos$`cc2 total cost %`
corredor= datos$corridor

nuevo_dataframe <- datos %>%
  select(
    tri = period,
    salida = source_name,
    destino = destination_name,
    empresa = firm,
    velocidad = `speed actual`,
    remesa200 = `cc1 denomination amount`,
    costo200 = `cc1 total cost %`,
    remesa500 = `cc2 denomination amount`,
    costo500 = `cc2 total cost %`,
    corredor = corridor
  )

head(nuevo_dataframe)
```

```{r}
empresa_frecuencia <- table(empresa)
empresa_frecuencia <- sort(empresa_frecuencia, decreasing = TRUE)
top_10_empresas <- head(empresa_frecuencia, 10)
top_10_empresas
```

```{r}
promedios_empresas <- nuevo_dataframe %>%
  filter(empresa %in% c("MoneyGram", "Western Union", "Viamericas", "Dolex Dollar Express",
                        "Xoom", "Intermex", "Walmart2World", "Ficohsa Express", "Ria", "Pangea")) %>%
  group_by(empresa) %>%
  summarize(
    promedio_costo200 = mean(costo200, na.rm = TRUE),
    promedio_costo500 = mean(costo500, na.rm = TRUE)
  )
print(promedios_empresas)
```

```{r}
library(dplyr)
# Plotting the top 10 used remittance channels used by Honduran immigrants in the United States, and their respective average cost during the 2013-2023 decade.

par(family = "serif")
promedios_empresas <- nuevo_dataframe %>%
  filter(empresa %in% c("MoneyGram", "Western Union", "Viamericas",
                        "Dolex Dollar Express", "Xoom", "Intermex",
                        "Walmart2World", "Ficohsa Express", "Ria", "Pangea")) %>%
  group_by(empresa) %>%
  summarize(
    promedio_costo200 = mean(costo200, na.rm = TRUE),
    promedio_costo500 = mean(costo500, na.rm = TRUE)
  )

promedios_empresas <- promedios_empresas %>%
  arrange(desc(promedio_costo200))


par(mfrow = c(1, 2))

plot(promedios_empresas$promedio_costo200,
     type = "b",  
     col = "blue",
     pch = 19,    
     xlab = "",
     ylab = "Average Cost (%)",
     xaxt = "n",
     main = "Average Cost per firm $200 USA-HND",
     cex.main = 0.8,
     cex.lab = 0.7)  
axis(1, at = 1:length(promedios_empresas$empresa), labels = rep("", length(promedios_empresas$empresa)))  
text(x = 1:length(promedios_empresas$empresa),
     y = par("usr")[3] - 0.15,  
     labels = promedios_empresas$empresa,
     srt = 45,  
     adj = 1,  
     cex = 0.47,  
     xpd = TRUE)  
grid()  

promedios_empresas <- nuevo_dataframe %>%
  filter(empresa %in% c("MoneyGram", "Western Union", "Viamericas",
                        "Dolex Dollar Express", "Xoom", "Intermex",
                        "Walmart2World", "Ficohsa Express", "Ria", "Pangea")) %>%
  group_by(empresa) %>%
  summarize(
    promedio_costo200 = mean(costo200, na.rm = TRUE),
    promedio_costo500 = mean(costo500, na.rm = TRUE)
  ) %>%
  arrange(desc(promedio_costo500))

plot(promedios_empresas$promedio_costo500,
     type = "b",  
     col = "blue",
     pch = 19,  
     xlab = "",
     ylab = "Average Cost (%)",
     xaxt = "n",
     main = "Average Cost per firm $500 USA-HND",
     cex.main = 0.8,
     cex.lab = 0.7)  
axis(1, at = 1:length(promedios_empresas$empresa), labels = rep("", length(promedios_empresas$empresa)))
text(x = 1:length(promedios_empresas$empresa),
     y = par("usr")[3] - 0.15,  
     labels = promedios_empresas$empresa,
     srt = 45,  
     adj = 1,
     cex = 0.47,  
     xpd = TRUE)  
grid()  
par(mfrow = c(1, 1))
```
