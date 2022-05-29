install.packages("BSDA")
library(BSDA)
#1
x <- c(78,75,67,77,70,72,28,74,77)
y <- c(100,95,70,90,90,90,89,90,100)
#1A
selisih = y-x
sdev <- sd(selisih)
sdev

#1B
t.test(x,y,paired=TRUE)

#1C
t.test(x,y,mu=0,var.equal=TRUE)

#2A
#setuju

#2B
tsum.test(mean.x=23500,s.x=3900,n.x=100,mu=20000,alternative="greater",var.equal=TRUE)

#2C
#kesimpulan yang didapatkan adalah rata-rata mobil lebih dari 20rb km/tahun

#3