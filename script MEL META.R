library("metafor"); library("car"); library("dplyr"); library("ggplot2")


##------------------------------------------------------------------------------
# Reading data into R

getwd()
# should be "C:/Users/krl0022/Documents" or similar

# we can then change the working directory to a specific folder for our project
setwd("C:/Currant/Statistical Consulting/Mel Lam/")
# let's see what is in the data folder
list.files("./")

# Import the six different .csv files into R
## Each of these datasets will be used in different analyses
data_JGNG <- read.csv("./data_JGNG.csv", header = TRUE)
head(data_JGNG)
data_IGNG <- read.csv("./data_IGNG.csv", header = TRUE)
head(data_IGNG)
data_CTR <- read.csv("./data_JGNG_CTR_WO.csv", header = TRUE)
head(data_CTR)



##------------------------------------------------------------------------------
# Overall analysis of Joint Go No Go Task
m0<-rma(JA_d, JA_Vd, data=data_JGNG, method="ML")
m0
confint(m0)

#Creating a funnel plot to show potential bias in the full dataset
funnel(m0, pch=20, cex = 1.25, cex.axis=1.25, cex.lab=1.25, xlab="JGNG: IC-C (d)")
#Statistical test of symmetry
regtest(m0, model = "lm") #Applies the test of asymmetry from Eggers (1997)
#Significant result indicates bias in the distribution of results

### Dolk 2012 appears to be a significant outlier driving this effect and 
### potentially driving the signficant difference between controls and other
### groups. 
# Removing the Outlier:
data_JGNG<-subset(data_JGNG, !(First_author == "Dolk" & Year ==2012))

m0<-rma(JA_d, JA_Vd, data=data_JGNG, method="ML")
m0
confint(m0)

# Figures with outliers removed
forest(m0, slab=paste(data_JGNG$First_author, data_JGNG$Year, sep=", "), 
       cex=1)
funnel(m0, pch=20, cex = 1.25, cex.axis=1.25, cex.lab=1.25, xlab="JGNG: IC-C (d)")
regtest(m0, model = "lm") #Applies the test of asymmetry from Eggers (1997)



########################################
# Removing Small Study to Explore Bias #
# Significant publication bias in these models suggests that the observed effect
# may be postively distorted by small outlying studies. 
summary(data_JGNG$N_group)
big_JGNG<-subset(data_JGNG, N_group >24)
# Analysis of Joint Go No Go Task with Big Studies only
m0b<-rma(JA_d, JA_Vd, data=big_JGNG, method="ML")
m0b
confint(m0b)

#Creating a forest plot to show the RE model of all of the data
forest(m0b, slab=paste(big_JGNG$First_author, big_JGNG$Year, sep=", "), 
       cex=1)

#Creating a funnel plot to show potential bias in the full dataset
funnel(m0b, pch=20, cex = 1.25, cex.axis=1.25, cex.lab=1.25, xlab="N > 24; JGNG: IC-C (d)")
#Statistical test of symmetry
regtest(m0b, model = "lm") #Applies the test of asymmetry from Eggers (1997)




##------------------------------------------------------------------------------
# Overall analysis of the Individual Go No Go Task
m1<-rma(IGNG_d, IGNG_Vd, data=data_IGNG, method="ML")
m1
confint(m1)

#Creating a forest plot to show the RE model of all of the data
forest(m1, slab=paste(data_IGNG$First_author, data_IGNG$Year, sep=", "), 
       cex=1)

#Creating a funnel plot to show potential bias in the full dataset
funnel(m1, pch=20, cex = 1.25, cex.axis=1.25, cex.lab=1.25, xlab="IGNG: IC-C (d)")
#Statistical test of symmetry
regtest(m1, model = "lm") #Applies the test of asymmetry from Eggers (1997)
#Significant result indicates bias in the distribution of results




##------------------------------------------------------------------------------
# Moderator Analysis of Controls
### Dolk 2012 appears to be significant outlier driving this effect and 
### potentially driving the signficant difference between controls and other
### groups. 
# Moderator Analysis of Controls
data_CTR<-subset(data_CTR, !(First_author == "Dolk" & Year ==2012))

m6<-rma(JA_d~Control, JA_Vd, data=data_CTR, method="ML")
m6
confint(m6)

#For plotting purposes, we will also break up our effects by BA +/-
noCTR<-subset(data_CTR, Control==0)
m6b<-rma(JA_d, JA_Vd, data=noCTR, method="ML")

isCTR<-subset(data_CTR, Control==1)
m6c<-rma(JA_d, JA_Vd, data=isCTR, method="ML")

#Creating a forest plot to show the RE model of all of the data
forest(m6b, slab=paste(noCTR$First_author, noCTR$Year, sep=", "), cex=1)
forest(m6c, slab=paste(isCTR$First_author, isCTR$Year, sep=", "), cex=1)


#Creating a funnel plot to show potential bias in the full dataset
m6d<-rma(JA_d~1, JA_Vd, data=data_CTR, method="ML")
m6d
funnel(m6d, cex = 1, cex.axis=1.25, cex.lab=1.25, 
       xlab="Control: IC-C (d)", pch=as.numeric(data_CTR$Control)+1)

as.numeric(data_CTR$Control)

#Statistical test of symmetry
regtest(m6d, model = "lm") #Applies the test of asymmetry from Eggers (1997)
#Significant result indicates bias in the distribution of results



##------------------------------------------------------------------------------
# Removing NAs from the WO dataset
data_WO<-subset(data_CTR, Wipeout != "NA")

# Moderator Analysis of Wipeouts
m7<-rma(JA_d~Wipeout, JA_Vd, data=data_WO, method="ML")
m7
confint(m7)

#For plotting purposes, we will also break up our effects by BA +/-
noWipeout<-subset(data_WO, Wipeout==0)
m7b<-rma(JA_d, JA_Vd, data=noWipeout, method="ML")

isWipeout<-subset(data_WO, Wipeout==1)
m7c<-rma(JA_d, JA_Vd, data=isWipeout, method="ML")

#Creating a forest plot to show the RE model of all of the data
forest(m7b, slab=paste(noWipeout$First_author, noWipeout$Year, sep=", "), cex=1)
forest(m7c, slab=paste(isWipeout$First_author, isWipeout$Year, sep=", "), cex=1)


#Creating a funnel plot to show potential bias in the full dataset
m7d<-rma(JA_d~1, JA_Vd, data=data_WO, method="ML")
funnel(m7d, cex = 1.25, cex.axis=1.25, cex.lab=1.25, 
       xlab="Control: IC-C (d)", pch=as.numeric(data_WO$Wipeout)+1)
#Statistical test of symmetry
regtest(m7d, model = "lm") #Applies the test of asymmetry from Eggers (1997)
#Significant result indicates bias in the distribution of results


##------------------------------------------------------------------------------
# Moderator Analysis of Controls with WipeOuts removed
data_CTR_no_WO <- subset(data_CTR, Wipeout == 0)
m8<-rma(JA_d~Control, JA_Vd, data=data_CTR_no_WO, method="ML")
m8
confint(m8)

#For plotting purposes, we will also break up our effects by BA +/-
noCTR_noWO<-subset(data_CTR_no_WO, Control==0)
m8b<-rma(JA_d, JA_Vd, data=noCTR_noWO, method="ML")

isCTR_noWO<-subset(data_CTR_no_WO, Control==1)
m8c<-rma(JA_d, JA_Vd, data=isCTR_noWO, method="ML")

#Creating a forest plot to show the RE model of all of the data
forest(m8b, slab=paste(noCTR_noWO$First_author, noCTR_noWO$Year, sep=", "), cex=1)
forest(m8c, slab=paste(isCTR_noWO$First_author, isCTR_noWO$Year, sep=", "), cex=1)


#Creating a funnel plot to show potential bias in the full dataset
m8d<-rma(JA_d~1, JA_Vd, data=data_CTR_no_WO, method="ML")
funnel(m8d, pch=21, cex = 1.25, cex.axis=1.25, cex.lab=1.25, 
       xlab="Control: IC-C (d)", bg=as.numeric(data_CTR_no_WO$Control)+2)
#Statistical test of symmetry
regtest(m8d, model = "lm") #Applies the test of asymmetry from Eggers (1997)
#Significant result indicates bias in the distribution of results

