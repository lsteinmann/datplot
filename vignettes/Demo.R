df <- read.table("./inst/data/testset_beazley_1000.csv", sep = ";")
str(df)
head(df)

result <- datsteps(df, stepsize = 10)

dens <- density(x = result$DAT_Step, weights = result$weight, bw = "SJ")
plot(dens)

dens <- density(x = result$DAT_Step, weights = result$weight)
plot(dens)

dens <- density(x = result$DAT_Step)
plot(dens)


result <- scaleweight(result, result$Technique)


library(ggplot2)
ggplot(result, aes(x=DAT_Step, color = Technique, fill = Technique)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.8)

ggplot(data = result, aes(x = DAT_Step,
                      color = Technique,
                      fill = Technique)) +
  geom_density(aes(weight = weight), alpha = 0.5) +
  xlab("Dating") +
  theme(panel.background = element_blank())
