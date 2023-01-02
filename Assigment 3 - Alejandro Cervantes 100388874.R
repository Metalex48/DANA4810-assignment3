#################        ASSIGNMENT 3 DANA 4820        #################

attach(NAVALBASE)
head(NAVALBASE)

x_base=ifelse(BASE == 'US',1,0)
NAVALBASE$x_base <- x_base


# A) Write a quadratic model.
E(y) = β0 + β1x + β2x**2 + β3x2

# B) Fit the quadratic model to the data.
modelquad<-lm(PERCENT ~ COST + I(COST^2) + x_base)
summary(modelquad)

# F) Determine if the coefficient β2 is larger than 0:
0.002644/0.00135 # t-statistic (Beta/StdError_Beta)
qt(p=0.05, df=(10-(3+1)), lower.tail=FALSE) # t-critical_value

# G) Determine whether the calculated s in part (d) a reasonable 
#     potential error of prediction. C.V.
(4.893/mean(PERCENT))*100

# H) Scatterplot
plot(x=COST, y=PERCENT, 
     ylab = "percentage increase in fleet effectiveness",
     xlab = "cost of modifying the fleet (in millions of USD)", 
     main = "Scatterplot of % increase in effectiveness vs. cost",
     col=ifelse(BASE == 'US',"red","black"), 
     ylim = c(0,37), xlim = c(40,170), pch=19)
legend(x="topleft", legend=c("US base", "Foreign base"), col=c("red", "black"), pch=19)

# J) Compare two fitted curves for both foreign Base and U.S. Base by plotting the two 
# prediction equations on your plot.

########################################### subsetting the data
US_base = subset(NAVALBASE, BASE=='US')
Foreign_base = subset(NAVALBASE, BASE=='FOR')
########################################### creating models for graphing
model_US = lm(US_base$PERCENT~US_base$COST+I(US_base$COST^2))
model_FOR = lm(Foreign_base$PERCENT~Foreign_base$COST+I(Foreign_base$COST^2))
########################################### displaying models to get the prediction equation
summary(model_US)
summary(model_FOR)
########################################### plotting the graphs
x<- seq(0,170,0.5) # setting the x values
y1 = 51.798761 -0.978397*x + 0.005423*x^2 # setting y values for US bases
y2 = 2.031900  -0.103639*x + 0.001889*x^2 # setting y values for foreign bases
lines(x,y1,col="red" ) 
lines(x,y2,col="black" )

# L) Fit the complete second-order model to the data
modelquad_complete<-lm(PERCENT ~ COST + I(COST^2) + x_base + COST*x_base + I(COST^2)*x_base)
summary(modelquad_complete)

# M) Is there sufficient evidence to indicate that type of base (U.S. or foreign) is a useful 
#    predictor of percentage improvement? Using α = .05 and the critical value approach.
0.001889/0.002338 # test statistic (Beta / StdError_Beta)
qt(p=0.05/2, df=(10-(5+1)), lower.tail=FALSE)

# N) What model would you recommend?
anova(modelquad, modelquad_complete) # doing partial F-test
modelquad_suggested<-lm(PERCENT ~ COST + I(COST^2)) # looking at the model with only Beta1 and Beta2
summary(modelquad_suggested)

