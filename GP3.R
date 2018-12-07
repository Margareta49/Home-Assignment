
require (MonteCarlo)
require(WebPower)
require(pwr)
require (ggplot2)


pwr.anova.test(f=0.35,k=4,power=0.50,sig.level=0.05) # 13x4=120
pwr.anova.test(f=0.35,k=4,power=0.90,sig.level=0.05) # 30x4=120

#calculate object with a range from 40-200
x0 = 40
N <- seq(x0, 200, 1)
N
?qf

#calculate lambda 
L= N* (0.35)^2

#find critical f value
df2 <- N-4
f.cri <- qf(.95, 3, df2,  lower.tail=TRUE, log.p=FALSE)
qf(.95,3,36, lower.tail=TRUE, log.p=FALSE) #check the example
View(f.cri)

#cri as critical F value, use to compute power
Power <- 1-pf(f.cri,3,df2,L)

plot (Power ~ N, xlab= "Sample Size", col ="dark red", cex.lab = 1.5)
 #Task 3

p <- c(Power)
sample <- c(N)

table1 <- data.frame(p,sample)
View(table1)

#create new table
table2 <- table1[ c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81,85,89,93,97,101,105,109,113,117,121,125,129,133,137,141,145,149,153,157,161), ]

View(table2)

plot(p ~ sample, data= table2, col = "dark red", xlab="Participants per Group", ylab = "Power", cex = 1.5, cex.lab =1.5)

#add new column to get per group

table2["Participants"] <- table2$sample/ 4
plot(p ~ Participants, data= table2, col = "dark red", xlab="Sample Size", ylab = "Power",  cex.lab = 1.5, cex = 1)
