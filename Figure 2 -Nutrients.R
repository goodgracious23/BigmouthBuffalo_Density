## Bigmouth Buffalo Trophic Cascade Project ###
# Code originally written by GM Wilkinson May 2020
# Updated:

rm(list=ls())
graphics.off()

# Read in the data
setwd("C:/Users/wilkinso/Box/Hort Farm Experiment/2019 Bigmouth Buffalo Experiment/BMB Trophic Cascades Manuscript")
nuts = read.csv("BMB_Nutrients_DOY115-149.csv")

# ========= nuts COLUMN HEADER METADATA ========= #
# doy = day of year
# pond = experimental pond A-F
# treatment = fish density treatment (high = 450 kg/ha, ambient; low= 150 kg/ha, harvested, no = 0 kg/ha, reference)
# period = experimental period (pre= prior to fish addition, add1 = after first BMB addition, add2= after second BMB addition)
# tp = total phosphorus concentration (micrograms per liter)
# tn = total nitrogen concentration (milligrams per liter)
# srp = soluble reactive phosphorus (micrograms per liter)
# no3 = nitrate concetration (milligrams per liter)

# ========= TREATMENT COLOR CODE ========= #
high1 = "aquamarine3" # Pond B
high2 = "aquamarine4" #Pond F
low1 = "royalblue" #Pond C
low2 = "royalblue4" #Pond E
no1 = "gray60" #Pond A
no2 = "gray30" #Pond D


#======================================================
# FIGURE 3 - Nutrient Bar Plots

windows(height = 2.25, width = 6.5)
par( mfrow = c(1,4), omi = c(0.1,0.1,0.1,0.1), mai = c(0.3,0.35,0.3,0))

# Total Phosphorus
tp.mean = c(
  mean(nuts[nuts$pond=="B" & nuts$period=="pre", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="F" & nuts$period=="pre", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="C" & nuts$period=="pre", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="E" & nuts$period=="pre", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="A" & nuts$period=="pre", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="D" & nuts$period=="pre", "tp"], na.rm=T),
  NA,
  mean(nuts[nuts$pond=="B" & nuts$period=="add2", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="F" & nuts$period=="add2", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="C" & nuts$period=="add2", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="E" & nuts$period=="add2", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="A" & nuts$period=="add2", "tp"], na.rm=T),
  mean(nuts[nuts$pond=="D" & nuts$period=="add2", "tp"], na.rm=T))

tp.se = c(
  sd(nuts[nuts$pond=="B" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="B" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="F" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="F" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="C" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="C" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="E" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="E" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="A" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="A" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="D" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="D" & nuts$period=="pre", "tp"])),
  0,
  sd(nuts[nuts$pond=="B" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="B" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="F" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="F" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="C" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="C" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="E" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="E" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="A" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="A" & nuts$period=="pre", "tp"])),
  sd(nuts[nuts$pond=="D" & nuts$period=="pre", "tp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="D" & nuts$period=="pre", "tp"])))

barplot(tp.mean, col=c(high1, high2, low1, low2, no1, no2, "white", high1, high2, low1, low2, no1, no2), 
  las=2, main = expression(TP~"("*mu*g~L^-1*")"), border = NA, space = 0, ylim=c(0,60))
mtext(side=1, line=0.5, "  Pre         Post", cex=0.9)

arrows(c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       tp.mean+tp.se, 
       x1=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       y1=tp.mean-tp.se,
       lwd = 2, angle = 90, code = 3, length = 0.02,
       col = c(high1, high2, low1, low2, no1, no2, "white", high1, high2, low1, low2, no1, no2))

#Soluble Reactive Phosphorus
srp.mean = 
  c(mean(nuts[nuts$pond=="B" & nuts$period=="pre", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="F" & nuts$period=="pre", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="C" & nuts$period=="pre", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="E" & nuts$period=="pre", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="A" & nuts$period=="pre", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="D" & nuts$period=="pre", "srp"], na.rm=T),
    NA,
    mean(nuts[nuts$pond=="B" & nuts$period=="add2", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="F" & nuts$period=="add2", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="C" & nuts$period=="add2", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="E" & nuts$period=="add2", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="A" & nuts$period=="add2", "srp"], na.rm=T),
    mean(nuts[nuts$pond=="D" & nuts$period=="add2", "srp"], na.rm=T))

srp.se = c(
  sd(nuts[nuts$pond=="B" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="B" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="F" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="F" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="C" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="C" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="E" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="E" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="A" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="A" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="D" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="D" & nuts$period=="pre", "srp"])),
  0,
  sd(nuts[nuts$pond=="B" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="B" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="F" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="F" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="C" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="C" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="E" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="E" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="A" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="A" & nuts$period=="pre", "srp"])),
  sd(nuts[nuts$pond=="D" & nuts$period=="pre", "srp"], na.rm=T)/sqrt(length(nuts[nuts$pond=="D" & nuts$period=="pre", "srp"])))

barplot(srp.mean, col = c(high1, high2, low1, low2, no1, no2, "white", high1, high2, low1, low2, no1, no2), 
  las=2, main = expression(SRP~"("*mu*g~L^-1*")"), border = NA, space = 0, ylim=c(0,10))
mtext(side=1, line=0.5, "  Pre         Post", cex=0.9)

arrows(c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       srp.mean+srp.se, 
       x1=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       y1=srp.mean-srp.se,
       lwd = 2, angle = 90, code = 3, length = 0.02,
       col = c(high1, high2, low1, low2, no1, no2, "white", high1, high2, low1, low2, no1, no2))

# Total Nitrogen
tn.mean = 
  c(mean(nuts[nuts$pond=="B" & nuts$period=="pre", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="F" & nuts$period=="pre", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="C" & nuts$period=="pre", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="E" & nuts$period=="pre", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="A" & nuts$period=="pre", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="D" & nuts$period=="pre", "tn"], na.rm=T),
    NA,
    mean(nuts[nuts$pond=="B" & nuts$period=="add2", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="F" & nuts$period=="add2", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="C" & nuts$period=="add2", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="E" & nuts$period=="add2", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="A" & nuts$period=="add2", "tn"], na.rm=T),
    mean(nuts[nuts$pond=="D" & nuts$period=="add2", "tn"], na.rm=T))

tn.se = c(
  sd(nuts[nuts$pond=="B" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="B" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="F" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="F" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="C" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="C" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="E" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="E" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="A" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="A" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="D" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="D" & nuts$period=="pre", "tn"])),
  0,
  sd(nuts[nuts$pond=="B" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="B" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="F" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="F" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="C" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="C" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="E" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="E" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="A" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="A" & nuts$period=="pre", "tn"])),
  sd(nuts[nuts$pond=="D" & nuts$period=="pre", "tn"], na.rm=T)/sqrt(length(nuts[nuts$pond=="D" & nuts$period=="pre", "tn"])))

barplot(tn.mean, col=c(high1, high2, low1, low2, no1, no2, "white", high1, high2, low1, low2, no1, no2), 
  las=2, main = expression(TN~"("*mg~L^-1*")"), border = NA, space = 0, ylim = c(0,16))
mtext(side=1, line=0.5, "  Pre         Post", cex=0.9)

arrows(c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       tn.mean+tn.se, 
       x1=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       y1=tn.mean-tn.se,
       lwd = 2, angle = 90, code = 3, length = 0.02,
       col = c(high1, high2, low1, low2, no1, no2, "white", high1, high2, low1, low2, no1, no2))

# Nitrate
no3.mean = 
  c(mean(nuts[nuts$pond=="B" & nuts$period=="pre", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="F" & nuts$period=="pre", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="C" & nuts$period=="pre", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="E" & nuts$period=="pre", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="A" & nuts$period=="pre", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="D" & nuts$period=="pre", "no3"], na.rm=T),
    NA,
    mean(nuts[nuts$pond=="B" & nuts$period=="add2", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="F" & nuts$period=="add2", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="C" & nuts$period=="add2", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="E" & nuts$period=="add2", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="A" & nuts$period=="add2", "no3"], na.rm=T),
    mean(nuts[nuts$pond=="D" & nuts$period=="add2", "no3"], na.rm=T))

no3.se = c(
  sd(nuts[nuts$pond=="B" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="B" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="F" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="F" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="C" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="C" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="E" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="E" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="A" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="A" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="D" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="D" & nuts$period=="pre", "no3"])),
  0,
  sd(nuts[nuts$pond=="B" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="B" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="F" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="F" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="C" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="C" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="E" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="E" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="A" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="A" & nuts$period=="pre", "no3"])),
  sd(nuts[nuts$pond=="D" & nuts$period=="pre", "no3"], na.rm=T)/sqrt(length(nuts[nuts$pond=="D" & nuts$period=="pre", "no3"])))

barplot(no3.mean, col = c(high1, high2, low1, low2, no1, no2, "white", high1, high2, low1, low2, no1, no2), 
  las=2, main = expression(NO[3]~"("*mg~L^-1*")"), border = NA, space = 0, ylim = c(0,0.08))
mtext(side=1, line=0.5, "  Pre         Post", cex=0.9)

arrows(c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       no3.mean+no3.se, 
       x1=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       y1=no3.mean-no3.se,
       lwd = 2, angle = 90, code = 3, length = 0.02,
       col = c(high1, high2, low1, low2, no1, no2, "white", high1, high2, low1, low2, no1, no2))


