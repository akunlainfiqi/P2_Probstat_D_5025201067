install.packages("BSDA")
install.packages("ggpubr")
install.packages("readr")
install.packages("dplyr")
install.packages("multcompView")
library(BSDA)
#1
x <- c(78,75,67,77,70,72,78,74,77)
y <- c(100,95,70,90,90,90,89,90,100)
#1A
dif = y-x

n <- length(dif)
rata <- mean(dif)
var <- 0

for (i in dif) {var <- var + (i-rata)^2}
var

var <- var/(n-1)
sdev <- var^(0.5)
sdev

#1B
t.test(y,x,paired=TRUE)

#1C
mean2 = 0
t <- (rata - mean2)*(n^(0.5))/sdev
if (2 * dt(t, n-1)<=0.05){
  print("terdapat pengaruh yang signifikan")
} else {
  print("tidak terdapat pengaruh yang signifikan")
}
#2
zsum.test(mean.x=23500,sigma.x=3900,n.x=100,mu=20000,alternative="two.sided", conf.level = 0.95)
#2A
#tidak setuju

#2B
#nilai p-value <2.2e-16 dan estimasi reratanya adalah 22735.61<mean<242264.39

#2C
#nilai p value lebih kecil daripada significant level sehingga null hipotesis ditolak

#3

#3a
#H0 = rerata bandung - rerata bali = 0
#H1 = rerata bandung - rerata bali !=0

#3b
sampel<- (((19-1)*(1.67^2)+(27-1)*(1.32^2))/(19+27-2))^0.5
sampel

#3c
tsum.test(3.64, 1.67, 19, 2.79, 1.32, 27, var.equal=TRUE)

#3d
#nilai kritikal = 0.05 dan p-value = 0.06049

#3e
#null hipotesa diterima

#3f
#rerata kedua populasi sama

#4
dsKucing <- read.delim(file.choose())
dsKucing$Group <- as.factor(dsKucing$Group)
library(ggpubr)

#4a
qplot(dsKucing$Group, dsKucing$Length, xlab="Group", ylab = "Length")

#4b
bartlett.test(Length ~ Group, data = dsKucing)

#4c
model1 <- aov(Length ~ Group, data = dsKucing)
summary(model1)

#4d
#gunakan data dari c

#4e
TukeyHSD(model1)

#4f
ggboxplot(dsKucing, "Group","Length",xlab = "Treatment",ylab="Weight")

#5
library(readr)
library(multcompView)
library(dplyr)
library(ggplot2)
data <- read.csv(file.choose())
data$Temp <- as.factor(data$Temp)
#5a
qplot(data$Temp,data$Light,xlab="temp", ylab="light", color=data$Glass)

#5b
hasil <- aov(Light ~ Glass + Temp + Glass:Temp, data)
summary(hasil)

#5c
group_by(data, Glass, Temp)%>%
summarise(mean=mean(Light), sd=sd(Light))

#5e
multcompLetters4(hasil,hasilTukey)
