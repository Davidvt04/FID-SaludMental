#Ejercicio 1
#Apartado 1
kang_nose <-read.delim("data/kanguros.csv", sep = "\t", head = TRUE)
head(kang_nose)

#Apartado 2
colnames(kang_nose) <- c('nose_length', 'nose_width') 
colnames(kang_nose) 

#Apartado 3
plot(x=kang_nose$nose_width,y=kang_nose$nose_length)
lm_kang <- lm(nose_length~ ., data = kang_nose)

lm_kang$coefficients #Se interpreta como Y= 27,89 + 2,7 * nose_width

nose_width_new <- kang_nose[1,]
nose_lenght_est <- predict(lm_kang, newdata = nose_width_new)


abline(lm_kang$coefficients, col = "red")

res <- kang_nose$nose_length - nose_lenght_est
rmse <- sqrt(mean(res^2))
rmse

#Apartado 4
# Si el R^2 es superior al 70% o 75%, podemos considerar bueno el modelo
#Por cada unidad que aumenta el largo de la nariz, el ancho aumenta ~2,70
# El P value (Pr(>|t|)) es 1.34e-15. Las variables están relacionadas con un p value < 0.05

# PR>t P valor individual  
# Esto indica que la relación entre el ancho y el largo de la nariz es altamente significativa estadísticamente
# Es 0,61 (mayor que 0,05), lo que indica que ele intercepto no es estadísticamente diferente de 0
summary(lm_kang)
summary(lm_kang)$r.squared 


