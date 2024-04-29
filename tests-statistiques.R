library(car)
#test stats sur Vaccinium avec les outliers dans le régime alimentaire 
#verfication des CA du test 
# visuelle
aov1 <- aov(Vaccinium ~ espece_gen, data = vaccinium)
opar <- par(mfrow = c(2, 2))
plot(aov1)
par(opar)
# Test de Levene pour l'homogénéité des variances
print(leveneTest(Vaccinium ~ espece_gen, data = vaccinium))
#test de bartlett homogénéité des variance
print (bartlett.test(Vaccinium ~ espece_gen, data = vaccinium))
# Test de Shapiro-Wilk pour la normalité des résidus
print (shapiro.test(residuals(anova_result)))#distribution non normale 

# Test de l'ANOVA
anova_result <- aov(Vaccinium ~ espece_gen, data = vaccinium_clean)
summary(anova_result)

#model linéaire 
lm_result <- lm(Vaccinium ~ espece_gen, data = vaccinium_clean)
anova(lm_result)

TukeyHSD(aov(lm_result), ordered = T)

#test stats sur Vaccinium sans les outliers dans le régime alimentaire 
#verfication des CA du test 
# visuelle
aov1 <- aov(Vaccinium ~ espece_gen, data = vaccinium_clean)
opar <- par(mfrow = c(2, 2))
plot(aov1)
par(opar)
# Test de Levene pour l'homogénéité des variances
print(leveneTest(Vaccinium ~ espece_gen, data = vaccinium_clean))
#test de bartlett homogénéité des variance
print (bartlett.test(Vaccinium ~ espece_gen, data = vaccinium_clean))
# Test de Shapiro-Wilk pour la normalité des résidus
print (shapiro.test(residuals(anova_result)))#distribution non normale 

# Test de l'ANOVA
anova_result <- aov(Vaccinium ~ espece_gen, data = vaccinium_clean)
summary(anova_result)

#model linéaire 
lm_result <- lm(Vaccinium ~ espece_gen, data = vaccinium_clean)
anova(lm_result)

TukeyHSD(aov(lm_result), ordered = T)

