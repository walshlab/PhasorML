library(readxl)
library(ggplot2)
library(randomForest)

setwd("C:/Linghao Hu/Project/Phasor Paper/Data/Macrophages")

# Read the data from excel
M0Data = read_excel("Macrophage_Directory.xlsx",sheet = "M0")
M1Data = read_excel("Macrophage_Directory.xlsx",sheet = "M1")
M2Data = read_excel("Macrophage_Directory.xlsx",sheet = "M2")


# Remove NA and first column, add label
M0Data <- subset (M0Data, select = -1)
M0Data <- na.omit(M0Data)
M0Data$label <- "M0"


M1Data <- subset (M1Data, select = -1)
M1Data <- na.omit(M1Data)
M1Data$label <- "M1"


M2Data <- subset (M2Data, select = -1)
M2Data <- na.omit(M2Data)
M2Data$label <- "M2"




# Boxplot of each FLIM endpoint ---------------------
library(ggplot2)
library(ggsignif)
library(ggpubr)
plotData <- rbind(M0Data,M1Data,M2Data)
plotData$label <- as.factor(plotData$label)
plotData$label <- factor(plotData$label , levels=c("M0", "M1", "M2"))


my_comparisons <- list(c("M0","M1"),c("M1","M2"),c("M0","M2"))


## NADH a1
yLabName = bquote("NAD(P)H "*alpha~""[1]~~("%"))
p <- ggplot(plotData, aes(x=label, y= NADH.a1, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none') +
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 
p
  ggsave("NADH.a1.png", width = 4, height = 4)


## NADH t1
yLabName = bquote("NAD(P)H "*tau~""[1]~~("ps"))
p <- ggplot(plotData, aes(x=label, y= NADH.t1, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("NADH.t1.png", width = 4, height = 4)

## NADH t2
yLabName = bquote("NAD(P)H "*tau~""[2]~~("ps"))
p <- ggplot(plotData, aes(x=label, y= NADH.t2, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("NADH.t2.png", width = 4, height = 4)

## NADH FLIM
yLabName = bquote("NAD(P)H "*tau~""[m]~~("ps"))
p <- ggplot(plotData, aes(x=label, y= NADH.FLIM, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("NADH.tm.png", width = 4, height = 4)


## NADH Intensity
yLabName = "NAD(P)H Intensity"
p <- ggplot(plotData, aes(x=label, y= NADH.Intensity, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("NADH.Int.png", width = 4, height = 4)


## FAD a1
yLabName = bquote("FAD "*alpha~""[1]~~("%"))
p <- ggplot(plotData, aes(x=label, y= FAD.a1, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("FAD.a1.png", width = 4, height = 4)



## FAD t1
yLabName = bquote("FAD "*tau~""[1]~~("ps"))
p <- ggplot(plotData, aes(x=label, y= FAD.t1, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("FAD.t1.png", width = 4, height = 4)


## FAD t2
yLabName = bquote("FAD "*tau~""[2]~~("ps"))
p <- ggplot(plotData, aes(x=label, y= FAD.t2, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("FAD.t2.png", width = 4, height = 4)



## FAD FLIM
yLabName = bquote("FAD "*tau~""[m]~~("ps"))
p <- ggplot(plotData, aes(x=label, y= FAD.FLIM, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("FAD.tm.png", width = 4, height = 4)

## FAD Intensity
yLabName = 'FAD Intensity'
p <- ggplot(plotData, aes(x=label, y= FAD.Intensity, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("FAD.Int.png", width = 4, height = 4)



## Redox ratio
yLabName = 'Redox Ratio'
p <- ggplot(plotData, aes(x=label, y= Redox.Ratio, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("RR.png", width = 4, height = 4)


## FLIRR
yLabName = 'FLIRR'
p <- ggplot(plotData, aes(x=label, y= FLIRR, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("FLIRR.png", width = 4, height = 4)


## NADH Phasor G
yLabName = 'NAD(P)H Phasor G'
p <- ggplot(plotData, aes(x=label, y= NADH.G, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("NADH.G.png", width = 4, height = 4)



## NADH Phasor S
yLabName = 'NAD(P)H Phasor S'
p <- ggplot(plotData, aes(x=label, y= NADH.S, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("NADH.S.png", width = 4, height = 4)


## FAD Phasor G
yLabName = 'FAD Phasor G'
p <- ggplot(plotData, aes(x=label, y= FAD.G, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("FAD.G.png", width = 4, height = 4)



## FAD Phasor S
yLabName = 'FAD Phasor S'
p <- ggplot(plotData, aes(x=label, y= FAD.S, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 18,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 16, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 24),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 

p
ggsave("FAD.S.png", width = 4, height = 4)



# T test analysis for each endpoint--------------

# NADH a1
t.test(controlData$NADH.a1,tenM2DGData$NADH.a1)
t.test(tenM2DGData$NADH.a1,twentyM2DGData$NADH.a1)
t.test(twentyM2DGData$NADH.a1,fiftyM2DGData$NADH.a1)
t.test(controlData$NADH.a1,noGlucoseData$NADH.a1)
t.test(controlData$NADH.a1,noGlucose24Data$NADH.a1)
t.test(controlData$NADH.a1,cyanideData$NADH.a1)

# NADH t1
t.test(controlData$NADH.t1,tenM2DGData$NADH.t1)
t.test(controlData$NADH.t1,twentyM2DGData$NADH.t1)
t.test(controlData$NADH.t1,fiftyM2DGData$NADH.t1)
t.test(controlData$NADH.t1,noGlucoseData$NADH.t1)
t.test(noGlucoseData$NADH.t1,noGlucose24Data$NADH.t1)
t.test(controlData$NADH.t1,cyanideData$NADH.t1)

# NADH t2
t.test(controlData$NADH.t2,tenM2DGData$NADH.t2)
t.test(controlData$NADH.t2,twentyM2DGData$NADH.t2)
t.test(twentyM2DGData$NADH.t2,fiftyM2DGData$NADH.t2)
t.test(fiftyM2DGData$NADH.t2,noGlucoseData$NADH.t2)
t.test(controlData$NADH.t2,noGlucose24Data$NADH.t2)
t.test(controlData$NADH.t2,cyanideData$NADH.t2)

# NADH FLIM
t.test(controlData$NADH.FLIM,tenM2DGData$NADH.FLIM)
t.test(tenM2DGData$NADH.FLIM,twentyM2DGData$NADH.FLIM)
t.test(twentyM2DGData$NADH.FLIM,fiftyM2DGData$NADH.FLIM)
t.test(controlData$NADH.FLIM,noGlucoseData$NADH.FLIM)
t.test(controlData$NADH.FLIM,noGlucose24Data$NADH.FLIM)
t.test(controlData$NADH.FLIM,cyanideData$NADH.FLIM)

# FAD a1
t.test(controlData$FAD.a1,tenM2DGData$FAD.a1)
t.test(tenM2DGData$FAD.a1,twentyM2DGData$FAD.a1)
t.test(twentyM2DGData$FAD.a1,fiftyM2DGData$FAD.a1)
t.test(controlData$FAD.a1,noGlucoseData$FAD.a1)
t.test(controlData$FAD.a1,noGlucose24Data$FAD.a1)
t.test(controlData$FAD.a1,cyanideData$FAD.a1)

# FAD t1
t.test(controlData$FAD.t1,tenM2DGData$FAD.t1)
t.test(controlData$FAD.t1,twentyM2DGData$FAD.t1)
t.test(controlData$FAD.t1,fiftyM2DGData$FAD.t1)
t.test(controlData$FAD.t1,noGlucoseData$FAD.t1)
t.test(controlData$FAD.t1,noGlucose24Data$FAD.t1)
t.test(controlData$FAD.t1,cyanideData$FAD.t1)

# FAD t2
t.test(controlData$FAD.t2,tenM2DGData$FAD.t2)
t.test(controlData$FAD.t2,twentyM2DGData$FAD.t2)
t.test(controlData$FAD.t2,fiftyM2DGData$FAD.t2)
t.test(controlData$FAD.t2,noGlucoseData$FAD.t2)
t.test(controlData$FAD.t2,noGlucose24Data$FAD.t2)
t.test(controlData$FAD.t2,cyanideData$FAD.t2)

# FAD FLIM
t.test(controlData$FAD.FLIM,tenM2DGData$FAD.FLIM)
t.test(controlData$FAD.FLIM,twentyM2DGData$FAD.FLIM)
t.test(twentyM2DGData$FAD.FLIM,fiftyM2DGData$FAD.FLIM)
t.test(controlData$FAD.FLIM,noGlucoseData$FAD.FLIM)
t.test(controlData$FAD.FLIM,noGlucose24Data$FAD.FLIM)
t.test(controlData$FAD.FLIM,cyanideData$FAD.FLIM)


#NADH Intensity
t.test(controlData$NADH.Intensity,tenM2DGData$NADH.Intensity)
t.test(tenM2DGData$NADH.Intensity,twentyM2DGData$NADH.Intensity)
t.test(twentyM2DGData$NADH.Intensity,fiftyM2DGData$NADH.Intensity)
t.test(controlData$NADH.Intensity,noGlucoseData$NADH.Intensity)
t.test(controlData$NADH.Intensity,noGlucose24Data$NADH.Intensity)
t.test(controlData$NADH.Intensity,cyanideData$NADH.Intensity)

#FAD Intensity
t.test(controlData$FAD.Intensity,tenM2DGData$FAD.Intensity)
t.test(controlData$FAD.Intensity,twentyM2DGData$FAD.Intensity)
t.test(controlData$FAD.Intensity,fiftyM2DGData$FAD.Intensity)
t.test(controlData$FAD.Intensity,noGlucoseData$FAD.Intensity)
t.test(controlData$FAD.Intensity,cyanideData$FAD.Intensity)

# Redox Ratio
t.test(controlData$Redox.Ratio,tenM2DGData$Redox.Ratio)
t.test(controlData$Redox.Ratio,twentyM2DGData$Redox.Ratio)
t.test(controlData$Redox.Ratio,fiftyM2DGData$Redox.Ratio)
t.test(controlData$Redox.Ratio,noGlucoseData$Redox.Ratio)
t.test(controlData$Redox.Ratio,cyanideData$Redox.Ratio)

# FLIRR
t.test(controlData$FLIRR,tenM2DGData$FLIRR)
t.test(controlData$FLIRR,twentyM2DGData$FLIRR)
t.test(twentyM2DGData$FLIRR,fiftyM2DGData$FLIRR)
t.test(controlData$FLIRR,noGlucoseData$FLIRR)
t.test(controlData$FLIRR,cyanideData$FLIRR)



# Draw UMAP ---------------------------------------------------------------

library(umap)




# umap of M0 vs M1 vs M2
plotDataUmap <- plotData[,c(2:5,7:10)]
# plotDataUmap <- plotData[,c(1:12)]
plotData.umap = umap(plotDataUmap,n_neighbors = 10, min_dist = 0.1, n_epochs = 100, n_trees = 10, pca = 3)
head(plotData.umap$layout, 3)

df<- data.frame(x = plotData.umap$layout[,1],
                y = plotData.umap$layout[,2],
                label = plotData$label)

ggplot(df, aes(x, y, group = label)) +
  geom_point(aes(color = label), size = 1.5, alpha = 0.8)+
  xlab("UMAP Dimension 1") +
  ylab("UMAP Dimension 2") +
  scale_colour_manual(name="label", values = c("blue","orange2","gray"))+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position= 'bottom',
        legend.title=element_blank(),
        legend.text=element_text(size=12,face="bold"))

# PCA analysis of M0 vs M1 vs M2
plotData.PCA <- prcomp(plotData[c(1:12)], center = TRUE, scale = TRUE)
summary(plotData.PCA)
plotData.PCA.label<- factor(plotData$Metabolic, levels=c("Control","Inhibit Glycolysis", "Inhibit OXPHOS"))

ggplot(plotData, aes(plotData.PCA$x[,1], plotData.PCA$x[,2]))+ 
  geom_point(aes(colour = factor(plotData.PCA.label)), size = 2) +
  scale_colour_manual(name="label", values = c("blue","orange2","gray"))+
  xlab("PC1") +
  ylab("PC2") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position= 'bottom',
        legend.title=element_blank(),
        legend.text=element_text(size=12,face="bold"))




# Scatter phasor plot/UMAP of M0 vs M1 vs M2
plotData <- rbind(M0Data,M1Data,M2Data)
plotData.labels <- factor(plotData$label , levels=c("M0","M1", "M2"))
plotDataScatter <- plotData[c(13:16)]

plotData.umap = umap(plotDataScatter,n_neighbors = 10, min_dist = 0.1, n_epochs = 100, n_trees = 10, pca = 3)
head(plotData.umap$layout, 3)

df<- data.frame(x = plotData.umap$layout[,1],
                y = plotData.umap$layout[,2],
                label = plotData$label)

ggplot(df, aes(x, y, group = label)) +
  geom_point(aes(color = label), size = 1.5, alpha = 0.8)+
  xlab("UMAP Dimension 1") +
  ylab("UMAP Dimension 2") +
  scale_colour_manual(name="label", values = c("blue","orange2","gray"))+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position= 'bottom',
        legend.title=element_blank(),
        legend.text=element_text(size=12,face="bold"))

# Scatter plot of NADH G and NADH S
ggplot(plotData, aes(NADH.G, NADH.S))+ 
  geom_point(aes(colour = factor(plotData.labels)), size = 1.5) +
  scale_colour_manual(name="label", values = c("blue","orange2","gray"))+
  xlab("NAD(P)H G") +
  ylab("NAD(P)H S") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position= 'bottom',
        legend.title=element_blank(),
        legend.text=element_text(size=12,face="bold"))


ggplot(plotData, aes(FAD.G, FAD.S))+ 
  geom_point(aes(colour = factor(plotData.labels)), size = 1.5) +
  scale_colour_manual(name="label", values = c("blue","orange2","gray"))+
  xlab("FAD G") +
  ylab("FAD S") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position= 'bottom',
        legend.title=element_blank(),
        legend.text=element_text(size=12,face="bold"))






## classification based on NADH and FAD lifetime components---
library(caTools)
library(randomForest)
library(e1071)
# 3 classes-----
mlData <-rbind(M0Data,M1Data,M2Data)
mlData <- mlData[,c(2:5,7:10,13:15,17)]
mlData <- as.data.frame(mlData)
mlData$label <- factor(mlData$label, levels = c("M0","M1","M2"))
sample = sample.split(mlData$label, SplitRatio  = 0.75, group = NULL)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)

# random forest tree
set.seed(100)
model <- randomForest(label~., data=train, ntree = 2500, mtry = 7)
pred.rft = predict(model, newdata=test[-10])
predPro.rft = predict(model, newdata=test[-10],type="prob")
confusionMatrix = table(test[,10], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)

# classification using SVM
model <- svm(label ~ ., data=train, type = 'C-classification',kernel = 'linear', probability = TRUE)
pred.svm = predict(model, newdata=test[-10])
predPro.svm = predict(model, newdata=test[-10],probability = TRUE)
predPro.svm = attr(predPro.svm, "probabilities")
confusionMatrix = table(test[,10], pred.svm)


# K nearst neighbor
normalize <-function(x) { (x -min(x))/(max(x)-min(x))   }
mlData_norm <- as.data.frame(lapply(mlData[,c(1:9)], normalize))
head(mlData_norm)
mlData$label <- factor(mlData$label, levels = c("M0","M1","M2"))
mlData_norm$label <-mlData$label
sample = sample.split(mlData_norm$label, SplitRatio  = 0.75, group = NULL)
train = subset(mlData_norm, sample == TRUE)
test = subset(mlData_norm, sample == FALSE)


## classification based on NADH phasor components
mlData <-rbind(M0Data,M1Data,M2Data)
mlData <- mlData[,c(13:17)]
mlData <- as.data.frame(mlData)
mlData$label <- factor(mlData$label, levels = c("M0","M1","M2"))
sample = sample.split(mlData$label, SplitRatio  = 0.75, group = NULL)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)

# classification using RFT
set.seed(100)
model <- randomForest(label~., data=train, ntree = 2500, mtry = 4)
pred.rft = predict(model, newdata=test[-5])
predPro.rft = predict(model, newdata=test[-5],type="prob")
confusionMatrix = table(test[,5], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)

# classification using SVM
model <- svm(label ~ ., data=train, type = 'C-classification',kernel = 'linear', probability = TRUE)
pred.svm = predict(model, newdata=test[-5])
predPro.svm = predict(model, newdata=test[-5],probability = TRUE)
predPro.svm = attr(predPro.svm, "probabilities")
confusionMatrix = table(test[,5], pred.svm)



# 2 classes-----
## classification based on NADH and FAD FLIM
library(caTools)
library(pROC)
mlData <-rbind(M1Data,M2Data)
mlData <- mlData[,c(2:5,7:10,13:16,17)]
mlData <- as.data.frame(mlData)
mlData$label <- factor(mlData$label, levels = c("M1","M2"))
sample = sample.split(mlData$label, SplitRatio  = 0.75, group = NULL)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)

# random forest tree
set.seed(100)
model <- randomForest(label~., data=train, ntree = 2500, mtry = 3)
pred.rft = predict(model, newdata=test[-5])
predPro.rft = predict(model, newdata=test[-5],type="prob")
confusionMatrix = table(test[,5], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)
mtype = as.numeric(test$label)
ROC <- get.tpr.fpr(predPro.rft,mtype)

# classification using SVM
model <- svm(label ~ ., data=train, type = 'C-classification',kernel = 'linear', probability = TRUE)
pred.svm = predict(model, newdata=test[-10])
predPro.svm = predict(model, newdata=test[-10],probability = TRUE)
predPro.svm = attr(predPro.svm, "probabilities")
confusionMatrix = table(test[,10], pred.svm)


### 5-fold Cross validation 

library(modelr)
library(caret)

# 5-fold cross validation to get average accuracy
k = 5
accList = list()
senList = list()
speList = list()
preList = list()
recList = list()
aucList = list()

for (x in 1:k){    
  cvData  <- crossv_kfold(mlData, k = k)
  train <- cvData$train[[x]]
  train <- as.data.frame(train)
  test <- cvData$test[[x]]
  test <- as.data.frame(test)
  
  model  <- randomForest(label~., data = train, ntree = 2500, mtry = 3)
  pred.rft = predict(model, newdata=test[-13])
  predPro.rft = predict(model, newdata=test[-13],type="prob")
  performance <- confusionMatrix(test[,13],pred.rft)
  
  mtype = as.numeric(test$label)
  ROC <- get.tpr.fpr(predPro.rft,mtype)
  
  auc <- ROC[[3]]
  accuracy <- performance$overall[1]
  sen <- performance$byClass[1]
  spe <- performance$byClass[2]
  pre <- performance$byClass[5]
  rec <- performance$byClass[6]
  
  accList <- append(accList,accuracy)
  senList <- append(senList,sen)
  speList <- append(speList,spe)
  preList <- append(preList,pre)
  recList <- append(recList,rec)
  aucList <- append(aucList,auc)
  }

recList <- as.numeric(recList)
mean(recList)

# 5-fold cross validation to get the feature contribution
k = 5

for (x in 1:k){    
  cvData  <- crossv_kfold(mlData, k = k)
  train <- cvData$train[[x]]
  train <- as.data.frame(train)
  test <- cvData$test[[x]]
  test <- as.data.frame(test)
  
  model  <- randomForest(label~., data = train, ntree = 2500, mtry = 4)
  importanceList = importance(model)
  if (x == 1) {importanceLists <- importanceList }
  else{importanceLists <- cbind(importanceLists,importanceList)}
}


# Barplot with mean

colnames(importanceLists) <- c('T1','T2','T3','T4','T5')
featureSD <- apply(importanceLists, 1, sd) 
featureMean <- apply(importanceLists, 1, mean) 
data <- data.frame(
  name = letters[1:4],
  value = featureMean,
  sd = featureSD
)

m1m2DecayFitFeatures = data;
m1m0DecayFitFeatures = data;
m0m2DecayFitFeatures = data;

m1m2DecayFitFeatures$label = 'M1M2'
m1m0DecayFitFeatures$label = 'M0M1'
m0m2DecayFitFeatures$label = 'M0M2'


m1m2PhasorFeatures = data;
m1m0PhasorFeatures = data;
m0m2PhasorFeatures = data;

m1m2PhasorFeatures$label = 'M1M2'
m1m0PhasorFeatures$label = 'M0M1'
m0m2PhasorFeatures$label = 'M0M2'


data <- rbind(m1m2DecayFitFeatures,m1m0DecayFitFeatures,m0m2DecayFitFeatures,
              m1m2PhasorFeatures,m1m0PhasorFeatures,m0m2PhasorFeatures)


data <- rbind(m1m2DecayFitFeatures,m1m0DecayFitFeatures,m0m2DecayFitFeatures)
data <- rbind(m1m2PhasorFeatures,m1m0PhasorFeatures,m0m2PhasorFeatures)



ggplot(data,
       aes(x=name, 
           y=value,
           ymin=value-sd, 
           ymax=value+sd,
           fill=label)) +
  geom_bar(stat="identity",position="dodge", alpha=0.7) +
  geom_errorbar( position = "dodge", width=0.9, colour="black", alpha=0.9, size=0.8)+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 12,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 12, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 20),
        legend.position = 'right')


## classification based on NADH phasor components--
mlData <-rbind(M1Data,M2Data)
mlData <- mlData[,c(13:17)]
mlData <- as.data.frame(mlData)
mlData$label <- factor(mlData$label, levels = c("M1","M2"))
sample = sample.split(mlData$label, SplitRatio  = 0.75, group = NULL)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)

# classification using RFT
set.seed(100)
model <- randomForest(label~., data=train, ntree = 2500, mtry = 4)
pred.rft = predict(model, newdata=test[-5])
predPro.rft = predict(model, newdata=test[-5],type="prob")
confusionMatrix = table(test[,5], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)

# classification using SVM
model <- svm(label ~ ., data=train, type = 'C-classification',kernel = 'linear', probability = TRUE)
pred.svm = predict(model, newdata=test[-5])
predPro.svm = predict(model, newdata=test[-5],probability = TRUE)
predPro.svm = attr(predPro.svm, "probabilities")
confusionMatrix = table(test[,5], pred.svm)



# classification using flim and phasor together----
mlData <-rbind(M0Data,M1Data,M2Data)
# mlData <- mlData[,c(2:5,7:10,12:17)]
mlData <- as.data.frame(mlData)
mlData$label <- factor(mlData$label, levels = c("M0","M1","M2"))
sample = sample.split(mlData$label, SplitRatio  = 0.75, group = NULL)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)

# random forest tree
set.seed(100)
model <- randomForest(label~., data=train, ntree = 2500, mtry = 7)
pred.rft = predict(model, newdata=test[-17])
predPro.rft = predict(model, newdata=test[-17],type="prob")
confusionMatrix = table(test[,17], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)

# classification using SVM
model <- svm(label ~ ., data=train, type = 'C-classification',kernel = 'linear', probability = TRUE)
pred.svm = predict(model, newdata=test[-10])
predPro.svm = predict(model, newdata=test[-10],probability = TRUE)
predPro.svm = attr(predPro.svm, "probabilities")
confusionMatrix = table(test[,10], pred.svm)


# Draw ROC curves of two-class classification-----

library(ROCR)
library(plotROC)
library(pROC)

get.tpr.fpr <- function(featuresPro, readVallue ){
  if (is.null(dim(featuresPro)))
  {
    pred <- prediction(featuresPro, readVallue)
    perf <- performance(pred, "fpr", "tpr")
    tpr <- perf@x.values[[1]]
    fpr <- perf@y.values[[1]]
  }
  else
  {
    pred <- prediction(featuresPro[,2], readVallue)
    perf <- performance(pred, "fpr", "tpr")
    tpr <- perf@x.values[[1]]
    fpr <- perf@y.values[[1]]
  }
  auc = performance(pred, "auc")@y.values[[1]]
  result <- list(tpr,fpr,auc)
  return(result)
}


mtype = as.numeric(test$label)

rft.roc.m1m2 <- get.tpr.fpr(predPro.rft,mtype)
rft.roc.m0m2 <- get.tpr.fpr(predPro.rft,mtype)
rft.roc.m0m1 <- get.tpr.fpr(predPro.rft,mtype)

features <- c(rep('M1 vs M2', times=length(rft.roc.m1m2[[1]])),
              rep('M0 vs M2', times=length(rft.roc.m0m2[[1]])),
              rep('M0 vs M1', times=length(rft.roc.m0m1[[1]])))

df <- data.frame(tpr=c(rft.roc.m1m2[[1]],rft.roc.m0m2[[1]],rft.roc.m0m1[[1]]),
                 fpr=c(rft.roc.m1m2[[2]],rft.roc.m0m2[[2]],rft.roc.m0m1[[2]]),
                 label = features ,stringsAsFactors = FALSE)



basicplot <- ggplot() + geom_line(aes(x=fpr, y=tpr,color = label),data = df,size =3)+
  xlab("Specificity") +
  ylab("Sensitivity") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=18),
        axis.text.y = element_text(face="bold",size=18),
        axis.title=element_text(size=20,face="bold"),
        legend.position = "none")

basicplot


# heatmap
data <- rbind(M0Data,M1Data,M2Data)
data <- as.data.frame(data)
my_group <- as.numeric(as.factor(data$label))
data <- data[,c(2:5,7:10,13:16)]
data <- as.matrix(data)
library(RColorBrewer)
colSide <- brewer.pal(3, "Set1")[my_group]
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(data,Colv = NA,Rowv = NA,scale = "column", col = col,RowSideColors=colSide)








# classification based on PCA and LDA-------------
library("scatterplot3d")


mlData <-rbind(M0Data,M1Data,M2Data)
mlData <- mlData[,c(2:5,7:10,12,17)]
mlData <- as.data.frame(mlData)
mlData$label <- factor(mlData$label, levels = c("M0","M1","M2"))

mlData.PCA <- prcomp(mlData[c(1:9)], center = TRUE, scale = TRUE)
summary(mlData.PCA)
mlData.PCA.label<-factor(mlData[,10])

shapes = c(16, 17, 18) 
colors <- c("red", "green", "blue")
colors<- adjustcolor(colors, alpha.f = 0.7)
colors <- colors[as.numeric(mlData.PCA.label)]
shapes <- shapes[as.numeric(mlData.PCA.label)]
s3d <- scatterplot3d(x = mlData.PCA$x[,1], y=mlData.PCA$x[,2], z=mlData.PCA$x[,3],
              color = colors, pch = shapes,angle = 155,
              cex.symbols=0.8, cex.axis=0.1 * par("cex.axis"),
              box=FALSE,
              xlab = "PC1",
              ylab = "PC2",
              zlab = "PC3")
legend(s3d$xyz.convert(-10,-18,1), legend = levels(mlData.PCA.label),
       col =  c("red", "green", "blue"), pch = c(16, 17, 18))


mlData <- mlData.PCA$x
mlData <- as.data.frame(mlData)
mlData$label <- factor(mlData.PCA.label, levels = c("M0","M1","M2"))
sample = sample.split(mlData, SplitRatio  = 0.75)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)




library(MASS)
# Fit the model
model <- lda(label~., data = train)
pred.lda = predict(model, newdata=test[-10])
predPro.lda = predict(model, newdata=test[-10],probability = TRUE)
predPro.lda = attr(predPro.lda, "probabilities")
confusionMatrix = table(test[,10], pred.lda$class)
lda.data <- cbind(train, predict(model)$x)

ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Metabolic))+
  stat_ellipse(aes(LD1, LD2,color=Metabolic, group=Metabolic),type = "norm")+
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())








