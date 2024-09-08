***  excel'de wst-1 ya da wst1 gibi bir isim yerine wst gibi isim yaz
***  excel i as.data.frame e çevir

> setwd('C:/Users/emre.yoruk/Desktop/pca1/')
> dir()
[1] "ders10.rmd"          "nümerik_datalar_PCA" "PCA_1.xlsx"         
[4] "PCA_1_komut.rtf"    
> library(readxl)
> PCA_1 <- read_excel("PCA_1.xlsx")
> View(PCA_1)
> colnames(PCA_1)[1]='Group'
> PCA1=as.data.frame(PCA_1)
> View(PCA1)
> library(ggplot2)
> library(ggpubr)
> library(tidyverse)
> library(rstatix)
> library(devtools)
> devtools::install_github("vqv/ggbiplot")
> library(ggbiplot)

                                        ###VERİLERE GENEL BAKIŞ###
> summary(PCA1)
> mean(PCA1$cat)                                                                                        DÖNGÜ YAPALIM ???
> sd(PCA1$cat)                                                                                          DÖNGÜ YAPALIM ???
                                            ###NORMAL DAĞILIM###
> pdf('density.pdf')
> par(mfrow=c(3,3))
> n=colnames(PCA1)
> for(i in 2:6){
+ hist(PCA1[,i],main=n[i],probability=TRUE, col='gray',border='green')
+ x= density(PCA1[,i])
+ lines(x, col='red')
+ }
> dev.off()


                                            ###Shapiro-Wilk###
    ## ayrı ayrı shapiro.test
> shapiro.test(PCA1$`WST-1`)
    ## döngü halinde shapiro.test
> for(i in 2:6){w=shapiro.test(PCA1[,i])}
> View(w)
> print(w)
> for(i in 2:6){print(shapiro.test(PCA1[,i])$p.value)}

                                            ###Multiple Regression###
> model <- lm(wst ~. -Group ,data = PCA1)
> model
> summary(model)

                                            ###ANOVA TEST###
library(devtools)
devtools::install_github("vqv/ggbiplot")
library(ggbiplot)
library(reshape)
ggqqplot(PCA1,'cat', facet.by = 'Group')
library(rstatix)
res.aov <- PCA1 %>% welch_anova_test(cat ~ Group)
pwc <- PCA1 %>% games_howell_test(cat ~ Group)
pwc <- pwc %>% add_xy_position(x = "Group", step.increase = 1)

ggboxplot(PCA1, x = "Group", y = "cat") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
    )

summary(res.aov)
res.aov
cat=aov(cat ~ Group,data=PCA1)
cat
summary(cat)

                                           ###Tukey-LSD###      
> cat=aov(cat ~ Group,data=PCA1)
> TukeyHSD(cat, conf.level = .95)
> summary(cat)
ya da 
> caty=aov(cat ~ Group,data=PCA1) ve summary(caty)

                                            ###Dunnet### 
> library(DescTools)
> DunnettTest(PCA1$cat, PCA1$Group)
> mean(PCA1$cat)
> sd(PCA1$cat)

                                            ###Duncan_GRUPLAMA###                                          DÖNGÜDE HATA VERİYOR
> library(agricolae)
> duncan.test(cat,"Group",alpha=0.05,console=TRUE)
> for(i in 2:6){print(duncan.test(PCA1[,i]))}
        ***Error in data.frame(y, trt) : argument "trt" is missing, with no default***

                                            ###PCA###                                                   
> setwd('C:/Users/emre.yoruk/Desktop/pca_son/Cok_Deney_Normalite_den_PCA_ya/')
> PCA_1 <- read_excel("PCA_1.xlsx")
> library(readxl)
> PCA_1 <- read_excel("PCA_1.xlsx")
> View(PCA_1)
> colnames(PCA_1)[1]='Group'
> View(PCA_1)
> PCA1=as.data.frame(PCA_1)
> View(PCA1)
> library(ggbiplot)
> pcaanalizi<-prcomp(PCA1[,2:6],center = T, scale. = T)
> ggbiplot(pcaanalizi,ellipse=TRUE,   groups=PCA1[,1])
> pdf('pcax.pdf', 20,20)
> ggbiplot(pcaanalizi,ellipse=TRUE,   groups=PCA1[,1])
> dev.off()

parmar komutu ile marginleri uzatabiliriz; marginleri küçültmemiz lazım :)

