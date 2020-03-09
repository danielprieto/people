#Script for building Figure 4
#Creation date 2019-12-01
#Current version: 2020-03-05
#Author: Daniel Prieto dprieto@fcien.edu.uy
##
#Data from Uruguay
data.uy <- read.csv(file = "/home/daniel/Documentos/Articulos/Scientific_Development/submission_STHV/scripts_and_data/uru_unesco.csv", header = TRUE, dec = ".")
data.uy <- as.data.frame(data.uy[,1:5])
colnames(data.uy) <- c("Year", "%GDP R+D", "GDP per capita (U$S)", "Researchers FTE", "Population")
library("ggplot2")
library("ggpmisc")
my.formula <- y ~ x
#
res100k <- as.data.frame(data.uy[,4]*100000/data.uy[,5])
colnames(res100k) <- "Researchers per 100K ppl"
data.uy <- cbind.data.frame(data.uy, res100k)
#
#===Panel A===#
fig2 <- ggplot(data.uy, aes(`Year`))+# xlim(c(0,5))+
geom_point(aes (y = `Researchers per 100K ppl`), colour = "gray48") + xlim(1995,2035) +
  stat_smooth(method = "lm", fullrange = TRUE, formula = y ~ x, mapping = aes(y=`Researchers per 100K ppl`), color="black")+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`Researchers per 100K ppl`), 
               parse = TRUE)+
  theme_bw()  + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10, color="black"),
    axis.title.x = element_text(size=9),
    axis.title.y = element_text(size=9),
    legend.position="none")
#
#===Panel B===#
fig3 <- ggplot(data.uy, aes(`Year`))+# xlim(c(0,5))+
  geom_point(aes (y = `%GDP R+D`), colour = "gray48") + xlim(1995,2035) + #ylim(0, 0.7) +
  stat_smooth(method = "lm", fullrange = T, formula = y ~ x, mapping = aes(y=`%GDP R+D`), color="black")+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`%GDP R+D`), 
               parse = TRUE)+
  theme_bw()  + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10, color="black"),
    axis.title.x = element_text(size=9),
    axis.title.y = element_text(size=9),
    legend.position="none")
fig3 <- fig3 + annotate(geom = "text", x = 2035, y = 0.2, label = "UY", hjust = "right")
#
library('plyr')
##
library("grid")
library("gridExtra")
##
#Data from Argentina
data.ar <- read.csv(file = "/home/daniel/Documentos/Articulos/Scientific_Development/submission_STHV/scripts_and_data/arg_unesco.csv", header = TRUE, dec = ".")
data.ar <- as.data.frame(data.ar[,2:7])
colnames(data.ar) <- c("Year", "Researchers FTE", "%GDP R+D", "GDP per capita (U$S)", "Population", "Researchers per 100K ppl")
#
#===Panel C===#
fig5 <- ggplot(data.ar, aes(x=`Year`))+# xlim(c(0,5))+
  geom_point(aes (y = `Researchers per 100K ppl`), colour = "gray48") + xlim(1995,2035) +
  stat_smooth(method = "lm", fullrange = TRUE, formula = y ~ x, mapping = aes(y=`Researchers per 100K ppl`), color="black")+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`Researchers per 100K ppl`), 
               parse = TRUE)+
  theme_bw()  + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10, color="black"),
    axis.title.x = element_text(size=9),
    axis.title.y = element_text(size=9),
    legend.position="none")
#
#===Panel D===#
fig6 <- ggplot(data.ar, aes(`Year`))+# xlim(c(0,5))+
  geom_point(aes (y = `%GDP R+D`), colour = "gray48") + xlim(1995,2035) + #ylim(0, 0.7) +
  stat_smooth(method = "lm", fullrange = T, formula = y ~ x, mapping = aes(y=`%GDP R+D`), color="black")+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`%GDP R+D`), 
               parse = TRUE)+
  theme_bw()  + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10, color="black"),
    axis.title.x = element_text(size=9),
    axis.title.y = element_text(size=9),
    legend.position="none")
fig6 <- fig6 + annotate(geom = "text", x = 2035, y = 0.35, label = "AR", hjust = "right")
##
#Data from Brazil
data.br <- read.csv(file = "/home/daniel/Documentos/Articulos/Scientific_Development/submission_STHV/scripts_and_data/bra_unesco.csv", header = TRUE, dec = ",")
data.br <- data.br[,2:7]
colnames(data.br) <- c("Year", "Researchers FTE", "%GDP R+D", "GDP per capita (U$S)", "Population", "Researchers per 100K ppl")
#
#===Panel E===#
fig8 <- ggplot(data.br, aes(x=`Year`))+# xlim(c(0,5))+
  geom_point(aes (y = `Researchers per 100K ppl`), colour = "gray48") + xlim(1995,2035) +
  stat_smooth(method = "lm", fullrange = TRUE, formula = y ~ x, mapping = aes(y=`Researchers per 100K ppl`), color="black")+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`Researchers per 100K ppl`), 
               parse = TRUE)+
  theme_bw()  + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10, color="black"),
    axis.title.x = element_text(size=9),
    axis.title.y = element_text(size=9),
    legend.position="none")
#
#===Panel F===#
fig9 <- ggplot(data.br, aes(`Year`))+# xlim(c(0,5))+
  geom_point(aes (y = `%GDP R+D`), colour = "gray48") + xlim(1995,2035) + #ylim(0, 0.7) +
  stat_smooth(method = "lm", fullrange = T, formula = y ~ x, mapping = aes(y=`%GDP R+D`), color="black")+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`%GDP R+D`), 
               parse = TRUE)+
  theme_bw()  + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10, color="black"),
    axis.title.x = element_text(size=9),
    axis.title.y = element_text(size=9),
    legend.position="none")
fig9 <- fig9 + annotate(geom = "text", x = 2035, y = 0.8, label = "BR", hjust = "right")
#
library("ggpubr")
#grid arrange
figure4 <- ggarrange(fig5, fig6, fig8, fig9, fig2, fig3, ncol = 2, nrow = 3, labels = c("A", "B", "C", "D", "E", "F"))
pdf("/home/daniel/Figure4.pdf", height = 10, width = 7)
figure4
dev.off()
##
#EOF