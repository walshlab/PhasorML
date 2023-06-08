library(readxl)
library(ggplot2)
library(tidyverse)

# Read the data from excel
setwd("G:/My Drive/_Walsh lab/Phasor Analysis Paper/Data Results phasor")
excel_sheet_name ="mcf7cellsdata.xlsx"

controlData = read_excel(excel_sheet_name, sheet = "Control")
tenM2DGData = read_excel(excel_sheet_name,sheet = "10mM")
twentyM2DGData = read_excel(excel_sheet_name,sheet = "20mM")
fiftyM2DGData = read_excel(excel_sheet_name,sheet = "50mM")
noGlucoseData = read_excel(excel_sheet_name,sheet = "No Glucose")

cyanideData = read_excel(excel_sheet_name,sheet = "Cyanide")

# Remove NA and first column, add label
controlData <- subset (controlData, select = -1)
controlData <- na.omit(controlData)
controlData$label <- "Control"

tenM2DGData <- subset (tenM2DGData, select = -1)
tenM2DGData <- na.omit(tenM2DGData)
tenM2DGData$label <- "10mM"

twentyM2DGData <- subset (twentyM2DGData, select = -1)
twentyM2DGData <- na.omit(twentyM2DGData)
twentyM2DGData$label <- "20mM"

fiftyM2DGData <- subset (fiftyM2DGData, select = -1)
fiftyM2DGData <- na.omit(fiftyM2DGData)
fiftyM2DGData$label <- "50mM 2DG"

noGlucoseData <- subset (noGlucoseData, select = -1)
noGlucoseData <- na.omit(noGlucoseData)
noGlucoseData$label <- "No Glucose"

cyanideData <- subset (cyanideData, select = -1)
cyanideData <- na.omit(cyanideData)
cyanideData$label <- "Cyanide"


# Boxplot of each FLIM endpoint ---------------------


library(ggsignif)
library(ggpubr)
plotData <- rbind(controlData,fiftyM2DGData,cyanideData,noGlucoseData)
plotData$label <- as.factor(plotData$label)
plotData$label <- factor(plotData$label , levels=c("Control", "50mM 2DG","No Glucose","Cyanide"))


my_comparisons <- list(c("Control","50mM 2DG"), c("Control","Cyanide"),c("Control","No Glucose"))

# FLIM decay fitting
# NADH a1
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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
        stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test") 
p
ggsave("NADH_alpha1.tiff",device='tiff', width=5, height=5.25, dpi=320)

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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
        stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
      
p
ggsave("NADH_tau1.tiff",device='tiff', width=5, height=5.25, dpi=320)

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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
        stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("NADH_tau2.tiff",device='tiff', width=5, height=5.25, dpi=320)

## NADH FLIM
jpeg("NADH_FLIM.jpeg", quality = 75)
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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("NADH_FLIM.tiff",device='tiff', width=5, height=5.25, dpi=320)

mean(plotData$NADH.FLIM)
mean(plotData$FAD.FLIM)


## NADH Intensity
jpeg("NADH_Intensity.jpeg", quality = 75)
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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
dev.off()

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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("FAD_alpha1.tiff",device='tiff', width=5, height=5.25, dpi=320)

## FAD t1
yLabName = bquote("FAD "*tau~""[1]~~("ps"))
plotData <- plotData[plotData$FAD.t1 < 1500, ]
p <- ggplot(plotData, aes(x=label, y= FAD.t1, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("FAD_tau1.tiff",device='tiff', width=5, height=5.25, dpi=320)

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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("FAD_tau2.tiff",device='tiff', width=5, height=5.25, dpi=320)

## FAD FLIM

yLabName = bquote("FAD "*tau~""[m]~~("ps"))
plotData <- plotData[plotData$FAD.FLIM < 5000, ]
p <- ggplot(plotData, aes(x=label, y= FAD.FLIM, color = label)) + 
  geom_jitter(shape=16, position=position_jitter(0.15),size = 1) + 
  geom_boxplot(alpha = 0.3,outlier.shape=NA,outlier.size = 0,width = 0.5,lwd = 1)+
  stat_boxplot(geom ='errorbar',width = 0.5) +
  ylab(yLabName)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("FAD_FLIM.tiff",device='tiff', width=5, height=5.25, dpi=320)

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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  #stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p


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
        legend.position = 'none')#+
  #stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p


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
        legend.position = 'none')#+
  #stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p


## Phasor Analysis ----------------------
# NADH Phasor G

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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("NADH_PhasorG.tiff",device='tiff', width=5, height=5.25, dpi=320)

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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("NADH_PhasorS.tiff",device='tiff', width=5, height=5.25, dpi=320)

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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p
ggsave("FAD_PhasorG.tiff",device='tiff', width=5, height=5.25, dpi=320)


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
        axis.text.x = element_text(size = 20,angle = 30,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        legend.position = 'none')+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test")
p

ggsave("FAD_PhasorS.tiff",device='tiff', width=5, height=5.25, dpi=320)



# T test analysis for each endpoint--------------

# NADH a1
t.test(controlData$NADH.a1,tenM2DGData$NADH.a1)
t.test(tenM2DGData$NADH.a1,twentyM2DGData$NADH.a1)
t.test(twentyM2DGData$NADH.a1,fiftyM2DGData$NADH.a1)
t.test(controlData$NADH.a1,noGlucoseData$NADH.a1)
#t.test(controlData$NADH.a1,noGlucose24Data$NADH.a1)
t.test(controlData$NADH.a1,cyanideData$NADH.a1)

# NADH t1
t.test(controlData$NADH.t1,tenM2DGData$NADH.t1)
t.test(controlData$NADH.t1,twentyM2DGData$NADH.t1)
t.test(controlData$NADH.t1,fiftyM2DGData$NADH.t1)
t.test(controlData$NADH.t1,noGlucoseData$NADH.t1)
#t.test(noGlucoseData$NADH.t1,noGlucose24Data$NADH.t1)
t.test(controlData$NADH.t1,cyanideData$NADH.t1)

# NADH t2
t.test(controlData$NADH.t2,tenM2DGData$NADH.t2)
t.test(controlData$NADH.t2,twentyM2DGData$NADH.t2)
t.test(twentyM2DGData$NADH.t2,fiftyM2DGData$NADH.t2)
t.test(fiftyM2DGData$NADH.t2,noGlucoseData$NADH.t2)
#t.test(controlData$NADH.t2,noGlucose24Data$NADH.t2)
t.test(controlData$NADH.t2,cyanideData$NADH.t2)

# NADH FLIM
t.test(controlData$NADH.FLIM,tenM2DGData$NADH.FLIM)
t.test(tenM2DGData$NADH.FLIM,twentyM2DGData$NADH.FLIM)
t.test(twentyM2DGData$NADH.FLIM,fiftyM2DGData$NADH.FLIM)
t.test(controlData$NADH.FLIM,noGlucoseData$NADH.FLIM)
#t.test(controlData$NADH.FLIM,noGlucose24Data$NADH.FLIM)
t.test(controlData$NADH.FLIM,cyanideData$NADH.FLIM)

# FAD a1
t.test(controlData$FAD.a1,tenM2DGData$FAD.a1)
t.test(tenM2DGData$FAD.a1,twentyM2DGData$FAD.a1)
t.test(twentyM2DGData$FAD.a1,fiftyM2DGData$FAD.a1)
t.test(controlData$FAD.a1,noGlucoseData$FAD.a1)
#t.test(controlData$FAD.a1,noGlucose24Data$FAD.a1)
t.test(controlData$FAD.a1,cyanideData$FAD.a1)

# FAD t1
t.test(controlData$FAD.t1,tenM2DGData$FAD.t1)
t.test(controlData$FAD.t1,twentyM2DGData$FAD.t1)
t.test(controlData$FAD.t1,fiftyM2DGData$FAD.t1)
t.test(controlData$FAD.t1,noGlucoseData$FAD.t1)
#t.test(controlData$FAD.t1,noGlucose24Data$FAD.t1)
t.test(controlData$FAD.t1,cyanideData$FAD.t1)

# FAD t2
t.test(controlData$FAD.t2,tenM2DGData$FAD.t2)
t.test(controlData$FAD.t2,twentyM2DGData$FAD.t2)
t.test(controlData$FAD.t2,fiftyM2DGData$FAD.t2)
t.test(controlData$FAD.t2,noGlucoseData$FAD.t2)
#t.test(controlData$FAD.t2,noGlucose24Data$FAD.t2)
t.test(controlData$FAD.t2,cyanideData$FAD.t2)

# FAD FLIM
t.test(controlData$FAD.FLIM,tenM2DGData$FAD.FLIM)
t.test(controlData$FAD.FLIM,twentyM2DGData$FAD.FLIM)
t.test(twentyM2DGData$FAD.FLIM,fiftyM2DGData$FAD.FLIM)
t.test(controlData$FAD.FLIM,noGlucoseData$FAD.FLIM)
#t.test(controlData$FAD.FLIM,noGlucose24Data$FAD.FLIM)
t.test(controlData$FAD.FLIM,cyanideData$FAD.FLIM)


#NADH Intensity
t.test(controlData$NADH.Intensity,tenM2DGData$NADH.Intensity)
t.test(tenM2DGData$NADH.Intensity,twentyM2DGData$NADH.Intensity)
t.test(twentyM2DGData$NADH.Intensity,fiftyM2DGData$NADH.Intensity)
t.test(controlData$NADH.Intensity,noGlucoseData$NADH.Intensity)
#t.test(controlData$NADH.Intensity,noGlucose24Data$NADH.Intensity)
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

controlData$Metabolic <- "Control"
fiftyM2DGData$Metabolic <- "Inhibit Glycolysis"
noGlucoseData$Metabolic <- "Inhibit Glycolysis"
cyanideData$Metabolic <- "Inhibit OXPHOS"

# UMAP decay curve fitting plot: control vs inhibit glycolysis vs inhibit cyanide

plotData <- rbind(controlData,fiftyM2DGData,cyanideData,noGlucoseData)
plotData.labels <- factor(plotData$Metabolic , levels=c("Control","Inhibit Glycolysis", "Inhibit OXPHOS"))
plotDataUmap <- plotData[c(2:5,7:10)]

plotData.umap = umap(plotDataUmap,n_neighbors = 10, min_dist = 0.1, n_epochs = 100, n_trees = 10, pca = 3)
head(plotData.umap$layout, 3)

df<- data.frame(x = plotData.umap$layout[,1],
                y = plotData.umap$layout[,2],
                label = plotData$Metabolic)

p <- ggplot(df, aes(x, y, group = label)) +
  geom_point(aes(color = label), size = 1.5, alpha = 0.8)+
  xlab("UMAP Dimension 1") +
  ylab("UMAP Dimension 2") +
  scale_colour_manual(name="label", values = c("blue","orange2","gray"))+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=20,face="bold"),
        legend.position= 'bottom',
        legend.title=element_blank(),
        legend.text=element_text(size=18,face="bold"))
p
ggsave("UMAP_FLIMdecay_41523.tiff",device='tiff', width=6.75, height=6, dpi=500)





# UMAP phasor plot: control vs inhibit glycolysis vs inhibit cyanide

plotData <- rbind(controlData,fiftyM2DGData,cyanideData,noGlucoseData)
plotData.labels <- factor(plotData$Metabolic , levels=c("Control","Inhibit Glycolysis", "Inhibit OXPHOS"))
plotDataScatter <- plotData[c(13:16)]

plotData.umap = umap(plotDataScatter,n_neighbors = 10, min_dist = 0.1, n_epochs = 100, n_trees = 10, pca = 3)
head(plotData.umap$layout, 3)

df<- data.frame(x = plotData.umap$layout[,1],
                y = plotData.umap$layout[,2],
                label = plotData$Metabolic)

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
        axis.title=element_text(size=20,face="bold"),
        legend.position= 'bottom',
        legend.title=element_blank(),
        legend.text=element_text(size=18,face="bold"))

ggsave("UMAP_FLIMphasor_41523.tiff",device='tiff', width=6.75, height=6, dpi=500)







# Scatter plot of NADH G and NADH S ======
jpeg("scatterplot_NADH_G_S.jpeg", quality = 75)
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
dev.off()

jpeg("scatterplot_FAD_G_S.jpeg", quality = 75)
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
dev.off()


## Machine Learning Classification --------
## classification based on Machine Learning Method
library(caTools)
library(randomForest)
library(e1071)
library(pROC)
library(ROCR)
library(plotROC)

# classification based on NADH lifetime bi-exponential decay curve fitting components
mlData <-rbind(fiftyM2DGData,noGlucoseData,cyanideData)
mlData <-rbind(controlData,cyanideData)
mlData <-rbind(controlData,fiftyM2DGData,noGlucoseData)

mlData <- mlData[,c(2:5,7:10,18)]
mlData <- as.data.frame(mlData)

mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Inhibit OXPHOS"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Control","Inhibit OXPHOS"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Control"))

sample = sample.split(mlData$Metabolic, SplitRatio  = 0.75, group = NULL)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)



# random forest tree
set.seed(100)
model <- randomForest(Metabolic~., data=train, ntree = 2500, mtry = 7, importance=TRUE)
pred.rft = predict(model, newdata=test[-9]) # 13 is last column
predPro.rft = predict(model, newdata=test[-9],type="prob")
confusionMatrix = table(test[,9], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)


# Get the predPro for each classifier and make a new dataframe
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


mtype = as.numeric(test$Metabolic)

rft.roc.inhibitGly_v_inhibitOXPHO <- get.tpr.fpr(predPro.rft,mtype)
rft.roc.control_v_inhibitGly <- get.tpr.fpr(predPro.rft,mtype)
rft.roc.control_v_inhibitOXPHO <- get.tpr.fpr(predPro.rft,mtype)

rft.roc.inhibitGly_v_inhibitOXPHO[[3]]
rft.roc.control_v_inhibitGly[[3]]
rft.roc.control_v_inhibitOXPHO[[3]]


features <- c(rep('Inhibit Glycolysis vs. Inhibit OXPHOS AUC = X', times=length(rft.roc.inhibitGly_v_inhibitOXPHO[[1]])),
              rep('Inhibit Glycolysis vs. Control AUC = X', times=length(rft.roc.control_v_inhibitGly[[1]])),
              rep('Inhibit OXPHOS vs. Control AUC = X', times=length(rft.roc.control_v_inhibitOXPHO[[1]])))



df <- data.frame(tpr=c(rft.roc.inhibitGly_v_inhibitOXPHO[[1]],rft.roc.control_v_inhibitGly[[1]],rft.roc.control_v_inhibitOXPHO[[1]]),
                 fpr=c(rft.roc.inhibitGly_v_inhibitOXPHO[[2]],rft.roc.control_v_inhibitGly[[2]],rft.roc.control_v_inhibitOXPHO[[2]]),
                 label = features ,stringsAsFactors = FALSE)

df$label <- factor(df$label, levels = c("Inhibit Glycolysis vs. Inhibit OXPHOS AUC = 0.939",
                                          "Inhibit Glycolysis vs. Control AUC = 0.857","Inhibit OXPHOS vs. Control AUC = 0.846"))

basicplot <- ggplot() + geom_line(aes(x=fpr, y=tpr,color = label),data = df,size =3)+
  xlab("Specificity") +
  ylab("Sensitivity") +
  
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=30),
        axis.text.y = element_text(face="bold",size=30),
        axis.title=element_text(size=45,face="bold"),
        legend.position= c(0.55,0.2),
        legend.title=element_blank(),
        legend.text=element_text(size=32,face="bold"))

basicplot
ggsave("FLIMcomponents_auc.tiff",device='tiff', width=14.5, height=10, dpi=500)





## Classification based on NADH phasor components----
mlData <-rbind(fiftyM2DGData,noGlucoseData,cyanideData)
mlData <-rbind(controlData,fiftyM2DGData,noGlucoseData)
mlData <-rbind(controlData,cyanideData)

mlData <- mlData[,c(13:16,18)]
mlData <- as.data.frame(mlData)

mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Inhibit OXPHOS"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Control"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Control","Inhibit OXPHOS"))

sample = sample.split(mlData$Metabolic, SplitRatio  = 0.75)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)



# classification using RFT
set.seed(100)
model <- randomForest(Metabolic~., data=train, ntree = 2500, mtry = 4)
pred.rft = predict(model, newdata=test[-5])
predPro.rft = predict(model, newdata=test[-5],type="prob")
confusionMatrix = table(test[,5], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)

# Get the predPro for each classifier and make a new dataframe
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


mtype = as.numeric(test$Metabolic)

rft.roc.inhibitGly_v_inhibitOXPHO <- get.tpr.fpr(predPro.rft,mtype)
AUC1 = rft.roc.inhibitGly_v_inhibitOXPHO[[3]]

rft.roc.control_v_inhibitGly <- get.tpr.fpr(predPro.rft,mtype)
AUC2 = rft.roc.control_v_inhibitGly[[3]]

rft.roc.control_v_inhibitOXPHO <- get.tpr.fpr(predPro.rft,mtype)
AUC3 = rft.roc.control_v_inhibitOXPHO[[3]]


features <- c(rep('Inhibit Glycolysis vs. Inhibit OXPHOS AUC = AUC1', times=length(rft.roc.inhibitGly_v_inhibitOXPHO[[1]])),
              rep('Inhibit Glycolysis vs. Control AUC = AUC2', times=length(rft.roc.control_v_inhibitGly[[1]])),
              rep('Inhibit OXPHOS vs. Control AUC = AUC3', times=length(rft.roc.control_v_inhibitOXPHO[[1]])))



df <- data.frame(tpr=c(rft.roc.inhibitGly_v_inhibitOXPHO[[1]],rft.roc.control_v_inhibitGly[[1]],rft.roc.control_v_inhibitOXPHO[[1]]),
                 fpr=c(rft.roc.inhibitGly_v_inhibitOXPHO[[2]],rft.roc.control_v_inhibitGly[[2]],rft.roc.control_v_inhibitOXPHO[[2]]),
                 label = features ,stringsAsFactors = FALSE)


df$label <- factor(df$label, levels = c("Inhibit Glycolysis vs. Inhibit OXPHOS AUC = 0.898",
                                        "Inhibit Glycolysis vs. Control AUC = 0.841","Inhibit OXPHOS vs. Control AUC = 0.817"))

basicplot <- ggplot() + geom_line(aes(x=fpr, y=tpr,color = label),data = df,size =3)+
  xlab("Specificity") +
  ylab("Sensitivity") +
  
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=30),
        axis.text.y = element_text(face="bold",size=30),
        axis.title=element_text(size=45,face="bold"),
        legend.position= c(0.55,0.2),
        legend.title=element_blank(),
        legend.text=element_text(size=32,face="bold"))

basicplot
ggsave("phasorcomponents_auc.tiff",device='tiff', width=14.5, height=10, dpi=500)



## classification based on NADH bi-exponential curve curve fitting and phasor components------------------------------------------------
mlData <-rbind(fiftyM2DGData,cyanideData, noGlucoseData)
mlData <-rbind(controlData,fiftyM2DGData, noGlucoseData)
mlData <-rbind(controlData,cyanideData)

mlData <- mlData[,c(2:5,7:10,13:16,18)]
#mlData <- mlData[,c(1:18)]
mlData <- as.data.frame(mlData)

mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Inhibit OXPHOS"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Control"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Control","Inhibit OXPHOS"))

sample = sample.split(mlData$Metabolic, SplitRatio  = 0.75)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)



# classification using RFT
set.seed(100)
model <- randomForest(Metabolic~., data=train, ntree = 2500, mtry = 7)
pred.rft = predict(model, newdata=test[-13])
predPro.rft = predict(model, newdata=test[-13],type="prob")
confusionMatrix = table(test[,13], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)

# Get the predPro for each classifier and make a new dataframe
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


mtype = as.numeric(test$Metabolic)

rft.roc.inhibitGly_v_inhibitOXPHO <- get.tpr.fpr(predPro.rft,mtype)
rft.roc.control_v_inhibitGly <- get.tpr.fpr(predPro.rft,mtype)
rft.roc.control_v_inhibitOXPHO <- get.tpr.fpr(predPro.rft,mtype)

rft.roc.inhibitGly_v_inhibitOXPHO[[3]]
rft.roc.control_v_inhibitGly[[3]]
rft.roc.control_v_inhibitOXPHO[[3]]


features <- c(rep('Inhibit Glycolysis vs. Inhibit OXPHOS AUC = 0.936', times=length(rft.roc.inhibitGly_v_inhibitOXPHO[[1]])),
              rep('Inhibit Glycolysis vs. Control AUC = 0.917', times=length(rft.roc.control_v_inhibitGly[[1]])),
              rep('Inhibit OXPHOS vs. Control AUC = 0.907', times=length(rft.roc.control_v_inhibitOXPHO[[1]])))


df <- data.frame(tpr=c(rft.roc.inhibitGly_v_inhibitOXPHO[[1]],rft.roc.control_v_inhibitGly[[1]],rft.roc.control_v_inhibitOXPHO[[1]]),
                 fpr=c(rft.roc.inhibitGly_v_inhibitOXPHO[[2]],rft.roc.control_v_inhibitGly[[2]],rft.roc.control_v_inhibitOXPHO[[2]]),
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
        legend.position= c(0.6,0.2),
        legend.title=element_blank(),
        legend.text=element_text(size=20,face="bold"))

basicplot
ggsave("combo_auc.tiff",device='tiff', width=14.5, height=10, dpi=500)




# CROSS VALIDATION phasor/curve fitting components/combo --

library(modelr)
library(caret)

mlData <-rbind(fiftyM2DGData,cyanideData, noGlucoseData)
mlData <-rbind(controlData,fiftyM2DGData, noGlucoseData)
mlData <-rbind(controlData,cyanideData)

mlData <- mlData[,c(2:5,7:10,13:16,18)] # combo: phasor and flim decay fitting
mlData <- mlData[,c(2:5,7:10,18)] # bi-exp decay curve fitting
mlData <- mlData[,c(13:16,18)] # phasor 


mlData <- as.data.frame(mlData)

mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Inhibit OXPHOS"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Control"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Control","Inhibit OXPHOS"))


k = 5
accList = list()
aucList = list()

for (x in 1:k){    
  cvData  <- crossv_kfold(mlData, k = k)
  train <- cvData$train[[x]]
  train <- as.data.frame(train)
  test <- cvData$test[[x]]
  test <- as.data.frame(test)
  
  model  <- randomForest(Metabolic~., data = train, ntree = 2500, mtry = 4)
  pred.rft = predict(model, newdata=test[-9])
  predPro.rft = predict(model, newdata=test[-9], type = "prob")
  importanceList
  # calculate auc
  
  # calculate accuracy
  performance <- confusionMatrix(test[,9],pred.rft)
  accuracy <- performance$overall[1]
  accList <- append(accList,accuracy)

  
  
}

mean(unlist(accList))
mean(unlist(speList))
mean(unlist(senList))
mean(unlist(preList))
mean(unlist(recList))


## Cross validation for FLIM bi-exp decay curve fitting and phasor analysis ----
library(modelr)
library(caret)

controlData$Metabolic <- "Control"
fiftyM2DGData$Metabolic <- "Inhibit Glycolysis"
noGlucoseData$Metabolic <- "Inhibit Glycolysis"
cyanideData$Metabolic <- "Inhibit OXPHOS"

mlData <-rbind(cyanideData,fiftyM2DGData,noGlucoseData)
mlData <-rbind(cyanideData,controlData)
mlData <-rbind(controlData,fiftyM2DGData,noGlucoseData)

mlData <- mlData[,c(2:5,7:10,18)] # bi-exp decay curve fitting 
label_key = 9
mlData <- mlData[,c(13:16,18)] # phasor 
label_key = 5
mlData <- mlData[,c(2:5,7:10,13:16,18)] # combo
label_key = 13

mlData <- as.data.frame(mlData)

mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Inhibit OXPHOS"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Control","Inhibit OXPHOS"))
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Control"))



k = 5
accList = list()
aucList = list()

for (x in 1:k){    
  cvData  <- crossv_kfold(mlData, k = k)
  train <- cvData$train[[x]]
  train <- as.data.frame(train)
  test <- cvData$test[[x]]
  test <- as.data.frame(test)
  
  model  <- randomForest(Metabolic~., data = train, ntree = 2500, mtry = 4)
  # find AUC
  pred.rft=predict(model,newdata=test[-label_key])
  prePro.rft <- predict(model,newdata=test[-label_key],type='prob')
  mtype = as.numeric(test$Metabolic)
  auc <- get.tpr.fpr(prePro.rft,mtype)[[3]]
  aucList = append(aucList, auc)
  
  # find accuracy
  pred.rft=predict(model,newdata=test[-label_key])
  performance <- confusionMatrix(test[,label_key],pred.rft)
  accuracy <- performance$overall[1]
  accList <- append(accList,accuracy)
  
  importanceList = importance(model)
  
  if (x == 1) {importanceLists <- importanceList }
  else{importanceLists <- cbind(importanceLists,importanceList)}
}

# save fit features accuracy and auc (for t-test)
accList_IG_IO_fitfeatures <- data.frame(accList);
aucList_IG_IO_fitfeatures <- data.frame(aucList);

accList_C_IO_fitfeatures <- data.frame(accList);
aucList_C_IO_fitfeatures <- data.frame(aucList);

accList_C_IG_fitfeatures <- data.frame(accList);
aucList_C_IG_fitfeatures <- data.frame(aucList);

# save phasor accuracy and auc
accList_IG_IO_phasor <- data.frame(accList);
aucList_IG_IO_phasor <- data.frame(aucList);

accList_C_IO_phasor <- data.frame(accList);
aucList_C_IO_phasor <- data.frame(aucList);

accList_C_IG_phasor <- data.frame(accList);
aucList_C_IG_phasor <- data.frame(aucList);

# save phasor + fitting accuracy and auc
accList_IG_IO_combo <- data.frame(accList);
aucList_IG_IO_combo <- data.frame(aucList);

accList_C_IO_combo <- data.frame(accList);
aucList_C_IO_combo <- data.frame(aucList);

accList_C_IG_combo <- data.frame(accList);
aucList_C_IG_combo <- data.frame(aucList);

# paired t-test 
# Gly vs OXPHOS
# accuracy
t.test(accList_IG_IO_fitfeatures,accList_IG_IO_phasor)
t.test(accList_IG_IO_fitfeatures,accList_IG_IO_combo)
t.test(accList_IG_IO_phasor,accList_IG_IO_combo)
# AUC
t.test(aucList_IG_IO_fitfeatures,aucList_IG_IO_phasor)
t.test(aucList_IG_IO_fitfeatures,aucList_IG_IO_combo)
t.test(aucList_IG_IO_phasor,aucList_IG_IO_combo)

# Control vs Glycolysis
# accuracy
t.test(accList_C_IG_fitfeatures,accList_C_IG_phasor)
t.test(accList_C_IG_fitfeatures,accList_C_IG_combo)
t.test(accList_C_IG_phasor,accList_C_IG_combo)
# AUC
t.test(aucList_C_IG_fitfeatures,aucList_C_IG_phasor)
t.test(aucList_C_IG_fitfeatures,aucList_C_IG_combo)
t.test(aucList_C_IG_phasor,aucList_C_IG_combo)

# Control vs OXPHOS
# accuracy
t.test(accList_C_IO_fitfeatures,accList_C_IO_phasor)
t.test(accList_C_IO_fitfeatures,accList_C_IO_combo)
t.test(accList_C_IO_phasor,accList_C_IO_combo)

# AUC
t.test(aucList_C_IO_fitfeatures,aucList_C_IO_phasor)
t.test(aucList_C_IO_fitfeatures,aucList_C_IO_combo)
t.test(aucList_C_IO_phasor,aucList_C_IO_combo)





# Barplot with mean
colnames(importanceLists) <- c('T1','T2','T3','T4','T5')
featureSD <- apply(importanceLists, 1, sd) 
featureMean <- apply(importanceLists, 1, mean) 
data <- data.frame(
  name = rownames(importanceLists),
  value = featureMean,
  sd = featureSD
)

IG_IODecayFitFeatures = data;
C_IODecayFitFeatures = data;
C_IGDecayFitFeatures = data;

IG_IODecayFitFeatures$label = 'Inhibit Glycolysis vs. Inhibit OXPHOS'
C_IODecayFitFeatures$label = 'Inhibit OXPHOS vs. Control'
C_IGDecayFitFeatures$label = 'Inhibit Glycolysis vs. Control'


IG_IOPhasorFeatures = data;
C_IOPhasorFeatures = data;
C_IGPhasorFeatures = data;

IG_IOPhasorFeatures$label = 'Inhibit Glycolysis vs. Inhibit OXPHOS'
C_IGPhasorFeatures$label = 'Inhibit Glycolysis vs. Control'
C_IOPhasorFeatures$label = 'Inhibit OXPHOS vs. Control'



data_flim <- rbind(IG_IODecayFitFeatures,C_IGDecayFitFeatures,C_IODecayFitFeatures)

data_flim$label <- factor(data_flim$label, levels = c("Inhibit Glycolysis vs. Inhibit OXPHOS","Inhibit Glycolysis vs. Control","Inhibit OXPHOS vs. Control"))



data_phasorfeatures <- rbind(IG_IOPhasorFeatures,C_IGPhasorFeatures,C_IOPhasorFeatures)

data_phasorfeatures$label <- factor(data_phasorfeatures$label, levels = c("Inhibit Glycolysis vs. Inhibit OXPHOS","Inhibit Glycolysis vs. Control","Inhibit OXPHOS vs. Control"))


t.test(IG_IOPhasorFeatures,C_IGPhasorFeatures)

# plot the feature contribution
ggplot(data_flim,
       aes(x=factor(name,level=c("NADH.a1","NADH.t1","NADH.t2","NADH.FLIM","FAD.a1","FAD.t1","FAD.t2","FAD.FLIM")), 
           y=value,
           ymin=value-sd, 
           ymax=value+sd,
           fill=label))+
  geom_bar(stat="identity",position="dodge", alpha=0.7) +
  geom_errorbar( position = "dodge", width=0.9, colour="black", alpha=0.9, size=0.8)+
  scale_y_continuous(expand = c(0,0))+
  ylab("Mean Decrease Gini")+
  scale_x_discrete(labels=c('FAD.a1'=expression(paste("FAD ",alpha[1])),
                            'FAD.t1' = expression(paste("FAD ",tau[1])),
                            'FAD.FLIM' = expression(paste("FAD ",tau[m])),
                            'FAD.t2' = expression(paste('FAD ',tau[2])),
                            'NADH.a1'=expression(paste("NAD(P)H ",alpha[1])),
                            'NADH.t1' = expression(paste("NAD(P)H ",tau[1])),
                            'NADH.FLIM' = expression(paste("NAD(P)H ",tau[m])),
                            'NADH.t2' = expression(paste('NAD(P)H ',tau[2]))
                            ))+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1,color = "black"),
        axis.text.y = element_text(size = 20, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 35),
        legend.text = element_text(size=20))
        #legend.position = c(0,1))
  
ggsave("meandecreasegini_flimcomponents.tiff",device='tiff', width=30, height=15, dpi=500)



# PLOT PHASOR FEATURES

data_phasorfeatures <- rbind(IG_IOPhasorFeatures,C_IGPhasorFeatures,C_IOPhasorFeatures)
data_phasorfeatures$label <- factor(data_phasorfeatures$label, levels = c("Control vs. Inhibit Glycolysis","Inhibit Glycolysis vs. Inhibit OXPHO","Control vs. Inhibit OXPHO"))


# plot the feature contribution
ggplot(data_phasorfeatures,
       aes(x=factor(name,level=c("NADH.G","NADH.S","FAD.G","FAD.S")), 
           y=value,
           ymin=value-sd, 
           ymax=value+sd,
           fill=label))+
  geom_bar(stat="identity",position="dodge", alpha=0.7) +
  geom_errorbar( position = "dodge", width=0.9, colour="black", alpha=0.9, size=0.8)+
  scale_y_continuous(expand = c(0,0))+
  ylab("Mean Decrease Gini")+
  scale_x_discrete(labels=c('FAD.S'=expression(paste("FAD S")),
                            'FAD.G' = expression(paste("FAD G")),
                            'NADH.S' = expression(paste("NAD(P)H S")),
                            'NADH.G' = expression(paste("NAD(P)H G"))))+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1,color = "black"),
        axis.text.y = element_text(size = 20, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 35),
        legend.text = element_text(size=20))
#legend.position = c(0,1))

ggsave("meandecreasegini_phasor.tiff",device='tiff', width=30, height=15, dpi=500)
