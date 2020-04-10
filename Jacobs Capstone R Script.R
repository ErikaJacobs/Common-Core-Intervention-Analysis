##########################################
#                                        #
#    ADDING UP STEM QUALIFICATIONS       #
#            ERIKA JACOBS                #
#    PENNSYLVANIA STATE UNIVERSITY       # 
#                2018                    #
#                                        #
##########################################


##########################################
#                                        #
#             DIRECTIONS                 #  
#                                        #
##########################################

#To Run Code: Open this file with R Studio. Put cursor on line 29. 
#Press CTRL+ENTER repeatedly to run each command in the file.
#Lines beginning with hashtag denote comments (Green Text).

##########################################
#                                        #
#    INTERVENTION ANALYSIS PROCESS       #  
#                                        #
##########################################

#BEFORE ANALYSIS: If you do not have the astsa package, please run the command below...

install.packages("astsa")

#STEP ONE: Delete everything from the global environment

rm(list=ls()) 

#STEP TWO: Check location of Working Directory

getwd()

#The location of the Working Directory is where files being imported have to be.

#If Working Directory location is inconvenient, click "Session"
#on menu above in R Studio, then "Set Working Directory", then "Choose Directory..." to change.

#STEP THREE: Import .dat files, Make a Time Series Object, and Plot
#Importing "NYALL.dat", "NYSTEM.dat", and "NYNONSTEM.dat"

NYALL <- scan("NYALL.dat") #Brings in data
NYALL=ts(NYALL, start=1998) #Turns data into a Time Series object
plot(NYALL, type="b", main="All Data", xlab="Year", ylab="Mean SAT Math Score") #Makes Graph
abline(h=509.0, col="purple") #Mean over all years
abline(v=2013, col="red") #Year of Common Core Implementation

NYSTEM <- scan("NYSTEM.dat") #Brings in data
NYSTEM=ts(NYSTEM, start=1998) #Turns data into a Time Series object
plot(NYSTEM, type="b", main="STEM Only", xlab="Year", ylab="Mean SAT Math Score") #Makes Graph
abline(h=522.3, col="purple") #Mean over all years
abline(v=2013, col="red") #Year of Common Core Implementation

NYNONSTEM <- scan("NYNONSTEM.dat") #Brings in data
NYNONSTEM=ts(NYNONSTEM, start=1998) #Turns data into a Time Series object
plot(NYNONSTEM, type="b", main="Non STEM Only", xlab="Year", ylab="Mean SAT Math Score") #Makes Graph
abline(h=496.6, col="purple") #Mean over all years
abline(v=2013, col="red") #Year of Common Core Implementation

#STEP FOUR: Split all 3 data sets into "Before" and "After" windows

#All 3 data sets have 19 years of data available
#15 years before Common Core Intervention
#4 years after Common Core Intervention

beforeALL = window (NYALL, 1998, 2012) #Before Common Core - All Data
afterALL = window (NYALL, 2013, 2016) #After Common Core - All Data

beforeSTEM = window (NYSTEM, 1998, 2012) #Before Common Core - STEM
afterSTEM = window (NYSTEM, 2013, 2016) #After Common Core - STEM

beforeNONSTEM = window (NYNONSTEM, 1998, 2012) #Before Common Core - NON STEM
afterNONSTEM = window (NYNONSTEM, 2013, 2016) #After Common Core - NON STEM

#STEP FIVE: Fit Time Series Structure to all "Before" windows

#BEGIN BY OBSERVING ACF/PACF PLOTS
#Plots and Interpretations of ACF/PACF Plots in Appendix G.

library(astsa)

acf2(beforeALL) #ACF/PACF Plot - All Data
acf2(beforeSTEM) #ACF/PACF Plot - STEM
acf2(beforeNONSTEM) #ACF/PACF Plot - NON STEM

#Testing Models - Testing 1 and 2 components for AR and MA.
#Testing all combinations of numbers possible below.
#0 stays in the middle because there is no differencing needed.

#Results of testing are summarized in Appendix H.

sarima(beforeALL,1,0,0)
sarima(beforeALL,0,0,1)
sarima(beforeALL,1,0,1)
sarima(beforeALL,2,0,0)
sarima(beforeALL,0,0,2)
sarima(beforeALL,2,0,2)
sarima(beforeALL,2,0,1)
sarima(beforeALL,1,0,2)

sarima(beforeSTEM,1,0,0)
sarima(beforeSTEM,0,0,1)
sarima(beforeSTEM,1,0,1)
sarima(beforeSTEM,2,0,0)
sarima(beforeSTEM,0,0,2)
sarima(beforeSTEM,2,0,2)
sarima(beforeSTEM,2,0,1)
sarima(beforeSTEM,1,0,2)

sarima(beforeNONSTEM,1,0,0)
sarima(beforeNONSTEM,0,0,1)
sarima(beforeNONSTEM,1,0,1)
sarima(beforeNONSTEM,2,0,0)
sarima(beforeNONSTEM,0,0,2)
sarima(beforeNONSTEM,2,0,2)
sarima(beforeNONSTEM,2,0,1)
sarima(beforeNONSTEM,1,0,2)

#Added for additional testing
sarima(beforeALL,1,0,3)
sarima(beforeALL,1,0,4)

#Residual Analysis For Final Models
#Residual outputs of final models selected in Appendix I.

ALLresiduals<-sarima(beforeALL,0,0,1)
summary(ALLresiduals)

STEMresiduals<-sarima(beforeSTEM,0,0,1)
summary(STEMresiduals)

NONSTEMresiduals<-sarima(beforeNONSTEM,1,0,2)
summary(NONSTEMresiduals)

#STEP 6: Forecast Predictions - Compare Differences

ALLpred = sarima.for(beforeALL, 4,0,0,1) #Predicted Scores 2013-2016 - ALL
ALLdiffs = afterALL - ALLpred$pred #Actual - Predicted - ALL
MeanALLdiffs<-mean(ALLdiffs) #Mean Difference in Score Per Year - ALL

STEMpred = sarima.for(beforeSTEM, 4,0,0,1) #Predicted Scores 2013-2016 - STEM
STEMdiffs = afterSTEM - STEMpred$pred #Actual - Predicted - STEM
MeanSTEMdiffs<-mean(STEMdiffs) #Mean Difference in Score Per Year - STEM

NONSTEMpred = sarima.for(beforeNONSTEM, 4,1,0,2) #Predicted Scores 2013-2016 - NON STEM
NONSTEMdiffs = afterNONSTEM - NONSTEMpred$pred #Actual - Predicted - NON STEM
MeanNONSTEMdiffs<-mean(NONSTEMdiffs) #Mean Difference in Score Per Year - NON STEM

#Mean Differences Table

MeanScoreDiffs<-matrix(c(MeanALLdiffs,MeanSTEMdiffs,MeanNONSTEMdiffs),ncol=3,byrow=TRUE)
colnames(MeanScoreDiffs)<-c("ALL","STEM","NONSTEM")
rownames(MeanScoreDiffs)<-c("Mean Difference")
MeanScoreDiffs<-as.table(MeanScoreDiffs)
MeanScoreDiffs

#STEP 7: Conduct Paired T Tests

#CHECK NORMAL PROBABILITY PLOTS - Check For Normality (points close or on line)

qqnorm(ALLdiffs) #Probability Plot - ALL
qqline(ALLdiffs)

qqnorm(STEMdiffs) #Probability Plot - STEM
qqline(STEMdiffs)

qqnorm(NONSTEMdiffs) #Probability Plot - NONSTEM
qqline(NONSTEMdiffs)

#Paired T Tests Below...

ALLpred$pred #Predicted Scores - ALL
afterALL #Actual Scores - ALL
ALLdiffs #Actual Minus Predicted - ALL

t.test(afterALL, ALLpred$pred, paired=TRUE) #Testing Statistical Significance - ALL

STEMpred$pred #Predicted Scores - STEM
afterSTEM #Actual Scores - STEM
STEMdiffs #Actual Minus Predicted - STEM

t.test(afterSTEM, STEMpred$pred, paired=TRUE) #Testing Statistical Significance - STEM

NONSTEMpred$pred #Predicted Scores - NON STEM
afterNONSTEM #Actual Scores - NON STEM
NONSTEMdiffs #Actual Minus Predicted - NON STEM

t.test(afterNONSTEM, NONSTEMpred$pred, paired=TRUE) #Testing Statistical Significance - NON STEM

#Output of Paired T Tests and Normality Plots in Appendix J

#STEP 8: Wilcoxon Test, Since Normality Assumption for T Test Not Met, and Sample Too Small

wilcox.test(afterALL, ALLpred$pred, paired = TRUE, alternative = "two.sided") #Wilcoxon - ALL
wilcox.test(afterSTEM, STEMpred$pred, paired = TRUE, alternative = "two.sided") #Wilcoxon - STEM
wilcox.test(afterNONSTEM, NONSTEMpred$pred, paired = TRUE, alternative = "two.sided") #Wilcoxon - NONSTEM
