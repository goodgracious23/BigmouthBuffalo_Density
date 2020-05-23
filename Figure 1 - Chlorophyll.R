## Bigmouth Buffalo Trophic Cascade Project ###
# Code originally written by GM Wilkinson May 2020
# Updated:

rm(list=ls())
graphics.off()

# Read in the data
setwd("C:/Users/wilkinso/Box/Hort Farm Experiment/2019 Bigmouth Buffalo Experiment/BMB Trophic Cascades Manuscript")
bmb = read.csv("BMB_TrophicCascade_DOY115-148_every3days.csv")

# ========= bmb COLUMN HEADER METADATA ========= #
# doy = day of year
# pond = experimental pond A-F
# treatment = fish density treatment (high = 450 kg/ha, ambient; low= 150 kg/ha, harvested, no = 0 kg/ha, reference)
# period = experimental period (pre= prior to fish addition, add1 = after first BMB addition, add2= after second BMB addition)
# time = time of day of sampling
# temp = water temperature at 0.25 m
# dosat = dissolved oxygen saturation at 0.25 m
# do = dissolved oxygen concentration (mg/L) at 0.25 m
# secchi = secchi depth
# secchi_flag = indicates if secchi depth was to the bottom of the pond
# pH = measured at 0.25 m depth
# cond = measured at 0.25 m depth (microSiemens per cm)
# chl = chlorophyll a concentration at 0.25 m depth
# pc = phycocyanin concentration at 0.25 m depth
# zoop_tow_depth = depth of zooplankton vertical tow

# ========= TREATMENT COLOR CODE ========= #
high1 = "aquamarine3" # Pond B
high2 = "aquamarine4" #Pond F
low1 = "royalblue" #Pond C
low2 = "royalblue4" #Pond E
no1 = "gray60" #Pond A
no2 = "gray30" #Pond D


# CHLOROPHYLL TIME SERIES
windows( width = 6.5, height = 4)
par(omi = c(0.75,0.9,0.2,0.2), mai = c(0,0,0,0))

plot(bmb[bmb$pond=="B", "doy"], log10(bmb[bmb$pond=="B", "chl"]), 
     col=high1, pch=19, lwd=6, type="l", ylim=c(log10(0.25), log10(100)), xlim=c(115,149), yaxt='n')
axis(side=2, at=c(log10(1), log10(2), log10(3), log10(4), log10(5), log10(6), log10(7), log10(8), log10(9),
                  log10(10), log10(20), log10(30), log10(40), log10(50), log10(60), log10(70), log10(80), log10(90), log10(100)),
     labels = c("1", "", "", "", "", "", "", "", "", "10", "", "", "", "", "", "", "", "", "100"), las=2)
polygon(c(115,123,123,115), c(log10(60), log10(60), log10(100), log10(100)), col="gray20") # Pre-Fish
lines(c(123, 123), c(log10(0.25), log10(100)), lwd=2, lty=3, col="gray20")
text(119, log10(80), "Pre Fish", col="white", font=2)
# polygon(c(124,128,128,124), c(log10(60), log10(60), log10(100), log10(100)), col="gray20") # First Fish
lines(c(129.1, 129.1), c(log10(0.25), log10(100)), lwd=2, lty=3, col="gray20")
polygon(c(129,149,149,129), c(log10(60), log10(60), log10(100), log10(100)), col="gray20") # Second Fish
text(139, log10(80), "Post Fish", col="white", font=2)

points(bmb[bmb$pond=="F", "doy"], log10(bmb[bmb$pond=="F", "chl"]), col=high2, pch=19, lwd=4, type="l")
points(bmb[bmb$pond=="C", "doy"], log10(bmb[bmb$pond=="C", "chl"]), col=low1, pch=19, lwd=6, type="l")
points(bmb[bmb$pond=="E", "doy"], log10(bmb[bmb$pond=="E", "chl"]), col=low2, pch=19, lwd=4, type="l")
points(bmb[bmb$pond=="A", "doy"], log10(bmb[bmb$pond=="A", "chl"]), col=no1, pch=19, lwd=6, type="l")
points(bmb[bmb$pond=="D", "doy"], log10(bmb[bmb$pond=="D", "chl"]), col=no2, pch=19, lwd=4, type="l")

mtext(side=2, line=2.5, expression("log"*~Chlorophyll~a~"("*mu*g~L^-1*")"), cex=1.25)
mtext(side=1, line=2.5, "Day of Year", cex=1.25)

# legend("bottomleft", legend = c("Ambient", "Ambient", "Harvested", "Harvested", "Reference", "Reference"),
       # col=c(high1, high2, low1, low2, no1, no2), pch=15, pt.cex=2.25, cex=0.9, bty = 'n')


# CHLOROPHYLL PRE BARPLOT
windows( width = 3.25, height = 3.5)
par(omi = c(0.75,0.9,0.1,0.1), mai = c(0,0,0.5,0))

chl.pre.mean = 
  c(mean(bmb[bmb$pond=="B" & bmb$period=="pre", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="F" & bmb$period=="pre", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="C" & bmb$period=="pre", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="E" & bmb$period=="pre", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="A" & bmb$period=="pre", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="D" & bmb$period=="pre", "chl"], na.rm=T))
chl.pre.se = c(
  sd(bmb[bmb$pond=="B" & bmb$period=="pre", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="B" & bmb$period=="pre", "chl"])),
  sd(bmb[bmb$pond=="F" & bmb$period=="pre", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="F" & bmb$period=="pre", "chl"])),
  sd(bmb[bmb$pond=="C" & bmb$period=="pre", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="C" & bmb$period=="pre", "chl"])),
  sd(bmb[bmb$pond=="E" & bmb$period=="pre", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="E" & bmb$period=="pre", "chl"])),
  sd(bmb[bmb$pond=="A" & bmb$period=="pre", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="A" & bmb$period=="pre", "chl"])),
  sd(bmb[bmb$pond=="D" & bmb$period=="pre", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="D" & bmb$period=="pre", "chl"])))


barplot(chl.pre.mean, col = c(high1, high2, low1, low2, no1, no2), 
        las=2, border = NA, space = 0, ylim=c(0,28), main = "Pre Fish")
arrows(c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       chl.pre.mean + chl.pre.se, 
       x1=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       y1=chl.pre.mean - chl.pre.se,
       lwd = 2, angle = 90, code = 3, length = 0.02,
       col = c(high1, high2, low1, low2, no1, no2))
mtext(side=2, line=2.5, expression(Chlorophyll~a~"("*mu*g~L^-1*")"), cex=1.25)


# CHLOROPHYLL POST BARPLOT
windows( width = 3.25, height = 3.5)
par(omi = c(0.75,0.9,0.1,0.1), mai = c(0,0,0.5,0))

chl.add2.mean = 
  c(mean(bmb[bmb$pond=="B" & bmb$period=="add2", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="F" & bmb$period=="add2", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="C" & bmb$period=="add2", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="E" & bmb$period=="add2", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="A" & bmb$period=="add2", "chl"], na.rm=T),
    mean(bmb[bmb$pond=="D" & bmb$period=="add2", "chl"], na.rm=T))
chl.add2.se = c(
  sd(bmb[bmb$pond=="B" & bmb$period=="add2", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="B" & bmb$period=="add2", "chl"])),
  sd(bmb[bmb$pond=="F" & bmb$period=="add2", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="F" & bmb$period=="add2", "chl"])),
  sd(bmb[bmb$pond=="C" & bmb$period=="add2", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="C" & bmb$period=="add2", "chl"])),
  sd(bmb[bmb$pond=="E" & bmb$period=="add2", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="E" & bmb$period=="add2", "chl"])),
  sd(bmb[bmb$pond=="A" & bmb$period=="add2", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="A" & bmb$period=="add2", "chl"])),
  sd(bmb[bmb$pond=="D" & bmb$period=="add2", "chl"], na.rm=T)/sqrt(length(bmb[bmb$pond=="D" & bmb$period=="add2", "chl"])))

barplot(chl.add2.mean, col = c(high1, high2, low1, low2, no1, no2), 
        las=2, border = NA, space = 0, ylim=c(0,4), main = "Post Fish")
arrows(c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       chl.add2.mean + chl.add2.se, 
       x1=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), 
       y1=chl.add2.mean - chl.add2.se,
       lwd = 2, angle = 90, code = 3, length = 0.02,
       col = c(high1, high2, low1, low2, no1, no2))
mtext(side=2, line=2.5, "", cex=1.25)

