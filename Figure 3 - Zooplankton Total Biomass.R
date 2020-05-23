## Bigmouth Buffalo Trophic Cascade Project ###
# Code originally written by GM Wilkinson May 2020
# Updated:

rm(list=ls())
graphics.off()

# Read in the data
setwd("C:/Users/wilkinso/Box/Hort Farm Experiment/2019 Bigmouth Buffalo Experiment/BMB Trophic Cascades Manuscript")
zoop = read.csv("zoop_115_133_140_147.csv")

# ========= zoop COLUMN HEADER METADATA ========= #
# doy = day of year
# pond = experimental pond A-F
# treatment = fish density treatment (high = 450 kg/ha, ambient; low= 150 kg/ha, harvested, no = 0 kg/ha, reference)
# period = experimental period (pre= prior to fish addition, add1 = after first BMB addition, add2= after second BMB addition)
# zoopbiomass = total zooplankton biomass (mg/L)

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

plot(zoop[zoop$pond=="B", "doy"], log10(zoop[zoop$pond=="B", "zoopbiomass"]), 
     col=high1, pch=19, lwd=6, type="l", ylim=c(log10(5), log10(3000)), xlim=c(115,149), yaxt='n')
axis(side=2, at=c(log10(10), log10(20), log10(30), log10(40), log10(50), log10(60), log10(70), log10(80), log10(90), 
                  log10(100), log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900),
                  log10(1000), log10(2000), log10(3000)),
     labels = c("10", "", "", "", "", "", "", "", "", "100", "", "", "", "", "", "", "", "", "1000","",""), las=2)
polygon(c(115,123,123,115), c(log10(2000), log10(2000), log10(3000), log10(3000)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log10(5), log10(3000)), lwd=2, lty=3, col="gray20")
text(119, log10(2500), "Pre Fish", col="white", font=2)
# polygon(c(124,128,128,124), c(log10(60), log10(60), log10(100), log10(100)), col="gray20") # First Fish
lines(c(129.1, 129.1), c(log10(5), log10(3000)), lwd=2, lty=3, col="gray20")
polygon(c(129,149,149,129), c(log10(2000), log10(2000), log10(3000), log10(3000)), col="gray20") # Second Fish
text(139, log10(2500), "Post Fish", col="white", font=2)

points(zoop[zoop$pond=="F", "doy"], log10(zoop[zoop$pond=="F", "zoopbiomass"]), col=high2, pch=19, lwd=4, type="l")
points(zoop[zoop$pond=="C", "doy"], log10(zoop[zoop$pond=="C", "zoopbiomass"]), col=low1, pch=19, lwd=6, type="l")
points(zoop[zoop$pond=="E", "doy"], log10(zoop[zoop$pond=="E", "zoopbiomass"]), col=low2, pch=19, lwd=4, type="l")
points(zoop[zoop$pond=="A", "doy"], log10(zoop[zoop$pond=="A", "zoopbiomass"]), col=no1, pch=19, lwd=6, type="l")
points(zoop[zoop$pond=="D", "doy"], log10(zoop[zoop$pond=="D", "zoopbiomass"]), col=no2, pch=19, lwd=4, type="l")

mtext(side=2, line=2.5, expression("log"*~Zooplankton~"("*mg~L^-1*")"), cex=1.25)
mtext(side=1, line=2.5, "Day of Year", cex=1.25)

legend("bottomleft", legend = c("Ambient", "Ambient", "Harvested", "Harvested", "Reference", "Reference"),
       col=c(high1, high2, low1, low2, no1, no2), pch=15, pt.cex=2.25, cex=0.9, bty = 'n')
