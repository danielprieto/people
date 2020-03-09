#Script for building Figures 1-3
#Creation date 2019-10-08
#Current version: 2020-03-05
#Author: Daniel Prieto dprieto@fcien.edu.uy
##
#Load data
datos <- read.csv2("/home/daniel/Documentos/Articulos/Scientific_Development/submission_STHV/scripts_and_data/scimagojr2.csv")
#
#Calculate #papers/100K people vs. %GDP invested in S&T
#
papersNorm <- as.data.frame(datos[,5]/datos[,13])#datos$Documents/datos$Pop
colnames(papersNorm) <- "Papers per inh"#define names
papersMillionInh <- as.data.frame((datos[,5]*100000)/datos[,13])#normalizing to 100K ppl
colnames(papersMillionInh) <- "Papers per 100K people"#define names
invCyTNorm <- as.data.frame(as.numeric(datos[,15])/as.numeric(datos[,12])*100)#normalizing to %gdp
colnames(invCyTNorm) <- "S&T %GDP"#define names
papers.inversion <- cbind(datos, papersNorm, papersMillionInh, invCyTNorm)
##
#Calculate researchers per 100K people
invMillionP <- as.data.frame(as.numeric(datos[,16])*100000)/as.numeric(datos[,13])#normalize to 100K ppl
colnames(invMillionP) <- "Researchers per 100K people"#define names
papers.inversion <- cbind(papers.inversion, invMillionP)
##
#normality testing  
shapiro.test(papers.inversion$`Papers per 100K people`)#NON NORMAL> W = 0.85463, p-value = 2.277e-06
shapiro.test(papers.inversion$`S&T %GDP`)#NN> W = 0.87499, p-value = 1.032e-05
shapiro.test(papers.inversion$`Researchers per 100K people`)#NN> W = 0.91438, p-value = 0.0007288
#
#some plotting
library("ggplot2")
#
#===FIGURE 1===#
#Plot papers per 100K people vs. %GDP invested in S&T
fig1 <-  ggplot(papers.inversion, aes(`S&T %GDP`, `Papers per 100K people`))+ 
    xlim(c(0,4.5)) + ylim(c(0,8000)) +
    geom_point(color = "gray48") +
    geom_text(data=subset(papers.inversion, Country=="Uruguay"), aes(`S&T %GDP`, `Papers per 100K people`, label="UY"), colour="black", size=3, nudge_x = 0.15, nudge_y = 200) +#triangle
      geom_point(data=subset(papers.inversion, Country=="Uruguay"), aes(`S&T %GDP`, `Papers per 100K people`), colour="black", shape=2) +#triangle
    geom_text(data=subset(papers.inversion, Country=="China"), aes(`S&T %GDP`, `Papers per 100K people`, label="CN"), colour="black", size=3, nudge_x = 0.2) +#plus
      geom_point(data=subset(papers.inversion, Country=="China"), aes(`S&T %GDP`, `Papers per 100K people`), colour="black", shape=3) +#plus
    geom_text(data=subset(papers.inversion, Country=="Switzerland"), aes(`S&T %GDP`, `Papers per 100K people`, label="CH"), colour="black", size=3, nudge_x = 0.2) +#x
      geom_point(data=subset(papers.inversion, Country=="Switzerland"), aes(`S&T %GDP`, `Papers per 100K people`), colour="black", shape=4) +#x
    geom_text(data=subset(papers.inversion, Country=="United States"), aes(`S&T %GDP`, `Papers per 100K people`, label="US"), colour="black", size=3, nudge_x = 0.15, nudge_y = -200) +#diamond
      geom_point(data=subset(papers.inversion, Country=="United States"), aes(`S&T %GDP`, `Papers per 100K people`, label="US"), colour="black", shape=5) +#diamond
    geom_text(data=subset(papers.inversion, Country=="Germany"), aes(`S&T %GDP`, `Papers per 100K people`, label="DE"), colour="black",  size=3, nudge_x = 0.2) +#inverted triangle
      geom_point(data=subset(papers.inversion, Country=="Germany"), aes(`S&T %GDP`, `Papers per 100K people`, label="DE"), colour="black", shape=6) +#inverted triangle
    geom_text(data=subset(papers.inversion, Country=="Argentina"), aes(`S&T %GDP`, `Papers per 100K people`, label="AR"), colour="black", size=3, nudge_x = 0.15, nudge_y = -100) +#square
      geom_point(data=subset(papers.inversion, Country=="Argentina"), aes(`S&T %GDP`, `Papers per 100K people`, label="AR"), colour="black", shape=7, nudge_x = 0.2) +#square
    geom_text(data=subset(papers.inversion, Country=="Brazil"), aes(`S&T %GDP`, `Papers per 100K people`, label="BR"), colour="black", size=3, nudge_x = 0.2)+ #asterisk
      geom_point(data=subset(papers.inversion, Country=="Brazil"), aes(`S&T %GDP`, `Papers per 100K people`, label="BR"), colour="black", shape=8, nudge_x = 0.2)+ #asterisk
  theme_bw()  + 
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, color="black"),
        legend.position="none")+
  stat_smooth(method = 'lm', formula= (y ~ x), color="black")+#add regression line
  ggpmisc::stat_poly_eq(formula = y~x,
               eq.with.lhs = "italic(hat(y))~`=`~",
              aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`Papers per 100K people`), 
             parse = TRUE)#add equation into panel
#export to pdf
pdf("/home/daniel/Figure1.pdf", height = 4, width = 4)
fig1
dev.off()
##
#===FIGURE 2===#
#Plot papers vs researchers [normalized against population]
fig2 <- ggplot(papers.inversion, aes(`Researchers per 100K people`, `Papers per 100K people`))+# ylim(c(0,100000))+
  geom_point(color="gray48")+#aes(colour=`Country`, alpha=0.5, size=`S&T %GDP`))+#size = 4)) +
  geom_text(data=subset(papers.inversion, Country=="Uruguay"), aes(`Researchers per 100K people`, `Papers per 100K people`, label="UY"), colour="black", size=3, nudge_x = -15, nudge_y = 300) +#triangle
    geom_point(data=subset(papers.inversion, Country=="Uruguay"), aes(`Researchers per 100K people`, `Papers per 100K people`), colour="black", shape=2) +#triangle
  geom_text(data=subset(papers.inversion, Country=="China"), aes(`Researchers per 100K people`, `Papers per 100K people`, label="CN"), colour="black", size=3, nudge_x = 25, nudge_y = -250) +#plus
    geom_point(data=subset(papers.inversion, Country=="China"), aes(`Researchers per 100K people`, `Papers per 100K people`), colour="black", shape=3) +#plus
  geom_text(data=subset(papers.inversion, Country=="Switzerland"), aes(`Researchers per 100K people`, `Papers per 100K people`, label="CH"), colour="black", size=3, nudge_x = 35) +#x
    geom_point(data=subset(papers.inversion, Country=="Switzerland"), aes(`Researchers per 100K people`, `Papers per 100K people`), colour="black", shape=4) +#x
  geom_text(data=subset(papers.inversion, Country=="United States"), aes(`Researchers per 100K people`, `Papers per 100K people`, label="US"), colour="black", size=3, nudge_x = 25, nudge_y = -200) +#diamond
    geom_point(data=subset(papers.inversion, Country=="United States"), aes(`Researchers per 100K people`, `Papers per 100K people`), colour="black", shape=5) +#diamond
  geom_text(data=subset(papers.inversion, Country=="Germany"), aes(`Researchers per 100K people`, `Papers per 100K people`, label="DE"), colour="black", size=3, nudge_x = 25, nudge_y = -200) +#inverted triangle
    geom_point(data=subset(papers.inversion, Country=="Germany"), aes(`Researchers per 100K people`, `Papers per 100K people`), colour="black", shape=6) +#inverted triangle
  geom_text(data=subset(papers.inversion, Country=="Argentina"), aes(`Researchers per 100K people`, `Papers per 100K people`, label="AR"), colour="black", size=3, nudge_x = 35, nudge_y = 200) +#square
    geom_point(data=subset(papers.inversion, Country=="Argentina"), aes(`Researchers per 100K people`, `Papers per 100K people`), colour="black", shape=7) +#square
  geom_text(data=subset(papers.inversion, Country=="Brazil"), aes(`Researchers per 100K people`, `Papers per 100K people`, label="BR"), colour="black", size=3, nudge_y = -250) +#asterisk
    geom_point(data=subset(papers.inversion, Country=="Brazil"), aes(`Researchers per 100K people`, `Papers per 100K people`), colour="black", shape=8) +#asterisk
  stat_smooth(method = 'lm', colour = "black")  +
    ggpmisc::stat_poly_eq(formula = y~x,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`Papers per 100K people`, x=`Researchers per 100K people`), 
                        parse = TRUE, inherit.aes = FALSE)+
  stat_ellipse(type = "t", linetype = 2, level = 0.75, 
               aes(`Researchers per 100K people`, `Papers per 100K people`, color=`Researchers per 100K people`>200)) +#75% confidence assuming a multivariate t-distribution
  scale_colour_manual(values = c("black", "black"))+
  theme_bw()  + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10, color="black"),
    legend.position="none")
#save to pdf
pdf("/home/daniel/Figure2.pdf", height = 4, width = 4)
fig2 
  dev.off()
##
#===FIGURE 3===#
#Plot Researchers/100K ppl vs %GDP
fig3 <- ggplot(papers.inversion, aes(`S&T %GDP`,`Researchers per 100K people`, color=`S&T %GDP`>1))+
  geom_point(colour="gray48") +
  geom_text(data=subset(papers.inversion, Country=="Uruguay"), aes(`S&T %GDP`, `Researchers per 100K people`, label="UY"), colour="black", size=3, nudge_x = 0.2, nudge_y = -20)+#, size=`GDP`) +#triangle
    geom_point(data=subset(papers.inversion, Country=="Uruguay"), aes(`S&T %GDP`, `Researchers per 100K people`), colour="black", shape=2)+#, size=`GDP`) +#triangle
  geom_text(data=subset(papers.inversion, Country=="China"), aes(`S&T %GDP`, `Researchers per 100K people`, label="CN"), colour="black", size=3, nudge_x = 0.2, nudge_y = -20)+#, size=`GDP`) +#plus
    geom_point(data=subset(papers.inversion, Country=="China"), aes(`S&T %GDP`, `Researchers per 100K people`), colour="black", shape=3)+#, size=`GDP`) +#plus
  geom_text(data=subset(papers.inversion, Country=="Switzerland"), aes(`S&T %GDP`, `Researchers per 100K people`, label="CH"), colour="black", size=3, nudge_x = 0.2, nudge_y = -20)+#, size=`GDP`) +#x
    geom_point(data=subset(papers.inversion, Country=="Switzerland"), aes(`S&T %GDP`, `Researchers per 100K people`), colour="black", shape=4)+#, size=`GDP`) +#x
  geom_text(data=subset(papers.inversion, Country=="United States"), aes(`S&T %GDP`, `Researchers per 100K people`, label="US"), colour="black", size=3, nudge_x = 0.2, nudge_y = -20)+#, size=`GDP`) +#diamond
    geom_point(data=subset(papers.inversion, Country=="United States"), aes(`S&T %GDP`, `Researchers per 100K people`), colour="black", shape=5)+#, size=`GDP`) +#diamond
  geom_text(data=subset(papers.inversion, Country=="Germany"), aes(`S&T %GDP`, `Researchers per 100K people`, label="DE"), colour="black", size=3, nudge_x = 0.2, nudge_y = -20)+#, size=`GDP`) +#inverted triangle
    geom_point(data=subset(papers.inversion, Country=="Germany"), aes(`S&T %GDP`, `Researchers per 100K people`), colour="black", shape=6)+#, size=`GDP`) +#inverted triangle
  geom_text(data=subset(papers.inversion, Country=="Argentina"), aes(`S&T %GDP`, `Researchers per 100K people`, label="AR"), colour="black", size=3, nudge_x = 0.2, nudge_y = -20)+#, size=`GDP`) +#square
    geom_point(data=subset(papers.inversion, Country=="Argentina"), aes(`S&T %GDP`, `Researchers per 100K people`), colour="black", shape=7)+#, size=`GDP`) +#square
  geom_text(data=subset(papers.inversion, Country=="Brazil"), aes(`S&T %GDP`, `Researchers per 100K people`, label="BR"), colour="black", size=3, nudge_x = 0.2, nudge_y = -20)+#, size=`GDP`)+ #asterisk
    geom_point(data=subset(papers.inversion, Country=="Brazil"), aes(`S&T %GDP`, `Researchers per 100K people`), colour="black", shape=8)+#, size=`GDP`)+ #asterisk
    theme(legend.position="none")+
  stat_smooth(method = 'lm', colour = "black") +
  ggpmisc::stat_poly_eq(formula = y~x,
              eq.with.lhs = "italic(hat(y))~`=`~",
             aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~"), y=`Researchers per 100K people`, x=`S&T %GDP`), 
            parse = TRUE, inherit.aes=FALSE)+
  stat_ellipse(type = "t", linetype = 2, level = 0.75) +#75% confidence 
  scale_colour_manual(values = c("black", "black")) +
  theme_bw()  + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10, color="black"),
    legend.position="none")
#save to pdf  
pdf("/home/daniel/Figure3.pdf", height = 4, width = 4)
fig3
dev.off()
##
#EOF