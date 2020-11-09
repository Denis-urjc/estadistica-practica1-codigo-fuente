rm(list=ls(all=TRUE))
datos=read.table("grupo18.txt",sep = ";",dec = ".",na.strings="NA",header = TRUE)
write.table(datos)
summary(datos)
##############
######Genero##
table(datos$gender)
pie(table(datos$gender),labels=c("Mujeres","Hombres"))
######Permanencia## (FIGURA 2)
par(mar = c(4, 4.2, 1, 2)) 
table(datos$tenure)
hist(datos$tenure, xlab = "Meses",ylab = "Frecuencia",main = NULL,
     cex.lab = 1.5,
     cex.axis= 1.5,
     col = rgb(0,1,1,0.4))
######Servicio telefonico## 
table(datos$PhoneService)
pie(table(datos$PhoneService),labels=c("No","Sí"))
######Servicio de Internet##
table(datos$InternetService)
pie(table(datos$InternetService),labels=c("ADSL","Fibra óptica","Sin internet"))
######Pago mensual##  FIGURA 9
data.frame(datos$MonthlyCharges)
summary(datos$MonthlyCharges)
par(mar = c(4.2, 1, 1, 1))
boxplot(datos$MonthlyCharges,horizontal = T,
        col = rgb(0,1,1,.5),
        xlab = 'Cobros mensuales (€)',
        main = NULL,
        cex.axis = 1.1,
        cex.lab = 1.5)
######Pago Total## FIGURA 10
data.frame(datos$TotalCharges)

par(mar = c(4.2, 4.2, 2, 2))
hist(datos$TotalCharges,xlab = "Pagos totales(€)",
     ylab = "Frecuencia",
     main = NULL,
     col = rgb(0,1,1,.5),
     cex.axis = 1.1,
     cex.lab = 1.5)
######Abandono## (FIGURA 1)
par(mar = c(2, 2, 2, 2))
table(datos$Churn)
pie(table(datos$Churn),labels=c("No","Sí"),
    col = c(rgb(0,1,1,0.4),
            rgb(1,0,1,.4)))
######Edad## {FIGURA 3}
par(mar = c(4, 1, 1, 1)) 
data.frame(datos$age)
boxplot(data.frame(datos$age), horizontal = TRUE, col = rgb(0,1,1,0.4),
         cex.lab = 1.5,
         cex.axis= 1.5,
        xlab = 'Edad (años)')
hist(datos$age)
#####################################
maleFullData <- datos[!(datos$gender == 'Female'),]
femaleFullData <- datos[!(datos$gender == 'Male'),]

dev.off()
par(mar = c(4, 4.2, 2, 2))
boxplot(datos$MonthlyCharges ~ datos$gender,
        col = c(rgb(1,0,1,0.5),
                rgb(0,1,1,0.5)),
        xlab = 'Género',
        names = c('Mujeres', 'Hombres'),
        ylab = 'Cobro mensual (€)',
        cex.axis = 1.1,
        cex.lab = 1.5)
#############Pagomensual/Serviciointernet## #Figura5.1
dev.off()
par(mar = c(4, 4.2, 2, 2))
boxplot(datos$MonthlyCharges ~ datos$InternetService,
        col = c(rgb(0,1,1,0.5),
                rgb(0,1,1,0.2),
                rgb(1,0,1,0.5)),
        xlab = 'Servicio de internet contratado',
        ylab = 'Cobro mensual (€)',
        names = c('DSL', 'Fibra óptica', 'Sin internet'),
        cex.axis = 1.1,
        cex.lab = 1.5)
#############Pagomensual/Abandono##### FIGURA 4
dev.off()
par(mar = c(4, 4.2, 2, 2))
boxplot(datos$MonthlyCharges ~ datos$Churn,
        col = c(rgb(0,1,1,0.5),
                rgb(1,0,1,0.5)),
        xlab = 'Han abandonado',
        ylab = 'Cobro mensual (€)',
        names = c('No', 'Sí'),
        cex.axis = 1.5,
        cex.lab = 1.5)
#############Pagototal/genero##########
dev.off()
par(mar = c(4, 4.2, 2, 2)) 
boxplot(datos$TotalCharges ~ datos$gender,
        col = c(rgb(1,0,1,0.5),
                rgb(0,1,1,0.5)),
        xlab = 'Género',
        names = c('Mujeres', 'Hombres'),
        ylab = 'Cobro total (€)',
        cex.lab = 1.5,
        cex.axis = 1.1)
############Pagototal/Serviciointernet### #FIGURA 5.2
dev.off()
par(mar = c(4, 4.2, 2, 2))
boxplot(datos$TotalCharges ~ datos$InternetService,
        col = c(rgb(0,1,1,0.5),
                rgb(0,1,1,0.2),
                rgb(1,0,1,0.5)),
        xlab = 'Servicio de internet contratado',
        ylab = 'Cobro total (€)',
        names = c('DSL', 'Fibra óptica', 'Sin internet'),
        cex.axis = 1.1,
        cex.lab = 1.5)
############Pagototal/Abandono###
dev.off()
par(mar = c(4, 4.2, 2, 2)) 
boxplot(datos$TotalCharges ~ datos$Churn,
        col = c(rgb(0,1,1,0.5),
                rgb(1,0,1,0.5)),
        xlab = 'Han abandonado',
        ylab = 'Cobro total (€)',
        names = c('No', 'Sí'),
        cex.axis = 1.5,
        cex.lab = 1.5)
##########Relacion bivariante, MonthlyCharges/tenure#####
dev.off()
par(mar = c(4, 4.2, 2, 2))
plot(datos$tenure, datos$MonthlyCharges,
     xlab = 'Permanencia de los clientes (meses)',
     ylab = 'Cobro mensual (€)',
     xlim = c(min(datos$tenure),max(datos$tenure)),
     ylim = c(min(datos$MonthlyCharges),max(datos$MonthlyCharges)),
     cex.axis = 1.1,
     cex.lab = 1.5,
     col = rgb(0,1,1))
abline(lm(datos$MonthlyCharges ~ datos$tenure), col = rgb(0,.4,.4))
##########Relacion bivariante, MonthlyCharges/tenure/genero###
dev.off()
par(mar = c(4, 4.2, 2, 2)) 
plot(maleFullData$tenure, maleFullData$MonthlyCharges,
     xlim = c(min(datos$tenure),max(datos$tenure)),
     ylim = c(min(datos$MonthlyCharges),max(datos$MonthlyCharges)),
     col = rgb(0,1,1),
     xlab = 'Permanencia de los clientes (meses)',
     ylab = 'Cobro mensual (€)',
     cex.axis = 1.1,
     cex.lab = 1.5)
legend('topleft', 
       c('Mujeres','Hombres'), 
       fill=c(rgb(1,0,1),rgb(0,1,1)))
abline(lm(maleFullData$MonthlyCharges ~ maleFullData$tenure), col = rgb(0,1,1))
par(new = T)
plot(femaleFullData$tenure, femaleFullData$MonthlyCharges, col = rgb(1,0,1),
     xlim = c(min(datos$tenure),max(datos$tenure)),
     ylim = c(min(datos$MonthlyCharges),max(datos$MonthlyCharges)),
     ann = F, axes = F)
abline(lm(femaleFullData$MonthlyCharges ~ femaleFullData$tenure), col = rgb(1,0,1))














