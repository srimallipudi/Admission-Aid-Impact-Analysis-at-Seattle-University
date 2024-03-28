#Remove objects (data) from your workspace
rm(list=ls(all=TRUE))

#Set working directory by clicking on Session --> Set Working Directory --> To Source File Location
dat <- read.csv("Unfundedaid.csv", header=TRUE)  # import data

colnames(dat)

#Estimate Logit Model
Model1 = glm(Registered.in.Colleague ~ Scholarship + Decision_time + UnfundedAid_Period + Citizenship.Status_FN + Race_AfricanAmerican + Race_AmericanIndian + Race_Asian + Race_NativeHawaiian + Sex_M + Application.Term_Spring + Application.Term_Summer + Application.Term_Winter + College_College.of.Arts.and.Sciences + College_College.of.Education + College_College.of.Science.and.Engineering, data = dat, 
                 family = "binomial")
summary(Model1)

#Generate Log-Likelihood
logLik(Model1)

#Logit model with only Intercept
Model1_Int = glm(Registered.in.Colleague ~ 1, data = dat, 
                           family = binomial(link = 'logit'))
summary(Model1_Int)

logLik(Model1_Int)

#Pseudo R2 for logit model1
1 - logLik(Model1)/logLik(Model1_Int)

#Generating Odd Ratios
exp(coef(Model1))
