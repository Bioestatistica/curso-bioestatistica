######################
## Exemplo Anorexia ##
######################

library(RColorBrewer)
mycol <- brewer.pal(8, "Dark2")
## display.brewer.pal(8, "Dark2")

setwd("~/Dropbox/Unicamp/Graduacao/2015S2/ME623_2s2015/Aulas/Exemplos/")
dados <- read.table("DadosAnorexia.txt", header=TRUE)
ybar_i <- aggregate(dados$ganho, by=list(dados$grupo), FUN=mean)$x
ys_i <- aggregate(dados$ganho, by=list(dados$grupo), FUN=sd)$x

pdf("Figuras/Anorexia_boxplot.pdf")
boxplot(ganho ~ factor(grupo), col=mycol[2], data=dados, 
        xlab="Grupo", ylab="Ganho de Peso (libras)", 
        cex.lab=1.4, cex.axis=1.5)
dev.off()

pdf("Figuras/Anorexia_dotplot.pdf")
stripchart(ganho ~ grupo, data=dados, method="stack",
           vertical=TRUE, jitter=0, xlab="Grupo", 
           ylab="Ganho de Peso (libras)", cex.lab=1.4, cex.axis=1.5,
           pch=19, cex=1.5, col=mycol[2])
stripchart(ybar_i ~ unique(grupo), data=dados, method="stack", vertical=TRUE, 
           jitter=0, pch=18, cex=2, add=TRUE)
dev.off()

## ANOVA
fit <- lm(ganho ~ grupo, data=dados)
anova(fit)
## xtable(fit)
mse <- anova(fit)[3][2,1]

## Análise dos Resíduos
## plot(fit, col="red")

pdf("Figuras/Anorexia_qqplot.pdf")
qqnorm(fit$residuals, datax=TRUE, col=mycol[1], pch=18, cex=2, cex.main=1.5, cex.lab=1.3, cex.axis=1.5, 
       main="Gráfico de Probabilidade Normal", ylab="Resíduos", xlab="Quantis Teóricos")
qqline(fit$residuals, datax=TRUE, lwd=2, lty=3, col="red")
dev.off()

st.residuals <- residuals(fit)/sqrt(mse)
pdf("Figuras/Anorexia_qqplotst.pdf")
qqnorm(st.residuals, datax=TRUE, col=mycol[1], pch=18, cex=2, cex.main=1.5, cex.lab=1.3, cex.axis=1.5, 
       main="Gráfico de Probabilidade Normal", ylab="Resíduos Padronizados", xlab="Quantis Teóricos")
qqline(st.residuals, datax=TRUE, lwd=2, lty=3, col="red")
dev.off()

pdf("Figuras/Anorexia_ResiduosXAjustados.pdf")
plot(residuals(fit) ~ fitted(fit), col=mycol[1], pch=18, cex=2, 
     cex.main=1.5, cex.lab=1.3, cex.axis=1.5, xlab="Valores Ajustados",
     ylab="Resíduos", main="Resíduos vs Valores Ajustados", xlim=c(-1, 8), ylim=c(-13, 18))
abline(h=0, lwd=2, lty=3, col="red")
dev.off()

pdf("Figuras/Anorexia_ResiduosXNiveis.pdf")
stripchart(st.residuals ~ factor(grupo), data=dados, method="stack",
           vertical=TRUE, jitter=0, xlab="Grupo", ylab="Resíduos Padronizados", 
           pch=18, cex=2, cex.main=1.5, cex.lab=1.3, cex.axis=1.5, col=mycol[1],
           main="Resíduos por Tratamentos")
abline(h=0, lwd=2, lty=3, col="red")
dev.off()

bartlett.test(ganho ~ factor(grupo), data=dados)

pairwise.t.test(dados$ganho, dados$grupo)

t.test(ganho[1:34] ~ grupo[1:34], var.equal=TRUE, data=dados)

pairwise.t.test(dados$ganho, dados$grupo, p.adjust="bonferroni")

testTukey <- TukeyHSD(aov(ganho ~ grupo, data=dados))
pdf("Figuras/Anorexia_TukeyTest.pdf")
  plot(testTukey)
dev.off()
