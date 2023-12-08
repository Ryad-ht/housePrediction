data <-read.csv("/Users/ryadhadj-tahar/houseprediction/housing_price_dataset.csv")
summary(data)


head(data)
summary(data)

# Vérification des valeurs manquantes
sapply(data, function(x) sum(is.na(x)))

# Analyse de corrélation
correlation_matrix <- cor(data)
print(correlation_matrix)

# Construction du modèle de régression linéaire multiple
modele_regression <- lm(Price ~ SquareFeet + Bedrooms + Bathrooms + Neighborhood + YearBuilt, data = data)

# Résumé du modèle
summary(modele_regression)

# Évaluation du modèle
predicted_prices <- predict(modele_regression, data)
RMSE <- sqrt(mean((data$Price - predicted_prices)^2))
R_squared <- summary(modele_regression)$r.squared

print(paste("RMSE :", RMSE))
print(paste("R-squared :", R_squared))
