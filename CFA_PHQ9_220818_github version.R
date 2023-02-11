#### PHQ-9 Quechua Bolivia: CFA, CFA Multigroups & MIMIC

###Open database
work_data<-read.csv("phq9_data_for_R.csv")
attach(work_data)

###View database
edit(work_data)

###Packages
library(lavaan)
library(semTools)
library(semPlot)

###Declare "ordinal" variables
  work_data[,c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")] <-
  lapply(work_data[,c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")], ordered)

###Fitting CFA Model (Full Sample)
  PHQ9_R.model <-' A1  =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9 '
  fit.R.full <- cfa(PHQ9_R.model, data=work_data, estimator = "WLSMV",
             ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.R.full,standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
  reliability(fit.R.full)
  semPaths(fit.R.full, "std", title = FALSE, curvePivot = TRUE)
  
###Fitting CFA Model (Variante Qechua = 1)
  fit.R.variante1 <- cfa(PHQ9_R.model, data=subset(work_data, VAR=="1"), estimator = "WLSMV",
                     ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.R.variante1,standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
  reliability(fit.R.variante1)

###Fitting CFA Model (Variante Qechua = 2)
  fit.R.variante2 <- cfa(PHQ9_R.model, data=subset(work_data, VAR=="2"), estimator = "WLSMV",
                         ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.R.variante2,standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
  reliability(fit.R.variante2)

###Fitting CFA Model (Variante Qechua = 3)
  fit.R.variante3 <- cfa(PHQ9_R.model, data=subset(work_data, VAR=="3"), estimator = "WLSMV",
                         ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.R.variante3,standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
  reliability(fit.R.variante3)

### CFA Multigroups (Measurement Invariance)

#Model across Sex
  measurementInvariance(model=PHQ9_R.model, data=work_data, group = "Sexo", strict = "TRUE", parameterization = "theta",
                      ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))

#Model across Urbano/Rural
measurementInvariance(model=PHQ9_R.model, data=work_data, group = "LugarVivienda", strict = "TRUE", parameterization = "theta",
                      ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))

###MIMIC MODELS AGE (MEASUREMENT MODEL (MM), AGE->DIMENSIONS (AD, intercept model), AGE->INDICATORS (AI, saturated model))
#### PHQ-9
  fit.full.MM <- cfa(PHQ9_R.model, data=work_data, estimator = "WLSMV",
                     ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.full.MM,standardized=TRUE, fit.measures=TRUE, modindices=TRUE)
  semPaths(fit.full.MM, title = FALSE, curvePivot = TRUE)
  reliability(fit.full.MM)
  full.AD.model <-' A1  =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9 
  A1 ~ Edad'
  fit.full.AD <- cfa(full.AD.model, data=work_data, estimator = "WLSMV",
                     ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.full.AD,standardized=TRUE, fit.measures=TRUE, modindices=TRUE)
  semPaths(fit.full.AD, title = FALSE, curvePivot = TRUE)
  full.AI.model <-' A1  =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9
  P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9  ~ Edad'
  fit.full.AI <- cfa(full.AI.model, data=work_data, estimator = "WLSMV",
                     ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.full.AI,standardized=TRUE, fit.measures=TRUE, modindices=TRUE)
  semPaths(fit.full.AI, title = FALSE, curvePivot = TRUE)

###MIMIC MODELS CIVIL STATUS (MEASUREMENT MODEL (MM), EstCiv->DIMENSIONS (ED, intercept model), EstCiv->INDICATORS (EI, saturated model))
#### PHQ-9
  full.ED.model <-' A1  =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9 
  A1 ~ EstadoCivil'
  fit.full.ED <- cfa(full.ED.model, data=work_data, estimator = "WLSMV",
                     ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.full.ED,standardized=TRUE, fit.measures=TRUE, modindices=TRUE)
  semPaths(fit.full.ED, title = FALSE, curvePivot = TRUE)
  full.EI.model <-' A1  =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9
  P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9  ~ EstadoCivil'
  fit.full.EI <- cfa(full.EI.model, data=work_data, estimator = "WLSMV",
                     ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.full.EI,standardized=TRUE, fit.measures=TRUE, modindices=TRUE)
  semPaths(fit.full.EI, title = FALSE, curvePivot = TRUE)

###MIMIC MODELS EDUCATION (MEASUREMENT MODEL (MM), GradInst->DIMENSIONS (GD, intercept model), GradInst->INDICATORS (GI, saturated model))
#### PHQ-9
  full.GD.model <-' A1  =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9 
  A1 ~ GRADO'
  fit.full.GD <- cfa(full.GD.model, data=work_data, estimator = "WLSMV",
                     ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.full.GD,standardized=TRUE, fit.measures=TRUE, modindices=TRUE)
  semPaths(fit.full.GD, title = FALSE, curvePivot = TRUE)
  full.GI.model <-' A1  =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9
  P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9  ~ GRADO'
  fit.full.GI <- cfa(full.GI.model, data=work_data, estimator = "WLSMV",
                     ordered=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9"))
  summary(fit.full.GI,standardized=TRUE, fit.measures=TRUE, modindices=TRUE)
  semPaths(fit.full.GI, title = FALSE, curvePivot = TRUE)

### END