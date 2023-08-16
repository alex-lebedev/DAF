# Load the required library
library(stringr)
library(lavaan)

# Invariance testing in response to reviewer's comments
# Date: 2023-08-16

load('/Users/alebedev/Dropbox/DAF_extended_data.rda')


#CMQ:

# Specify the measurement model
measurement_model <- '
  CONS =~ CONS_public + CONS_polit + CONS_monit + CONS_connect + CONS_org
'
# Fit the configural model (no constraints across groups)
configural_fit <- sem(measurement_model, data = ebs_data, group = "LANG")
summary(configural_fit)


# Metric invariance (constrain loadings)
metric_fit <- sem(measurement_model, data = ebs_data, group = "LANG", group.equal = "loadings")
summary(metric_fit)

# Scalar invariance (constrain loadings and intercepts)
scalar_fit <- sem(measurement_model, data = ebs_data, group = "LANG", group.equal = c("loadings", "intercepts"))
summary(scalar_fit)

# Strict invariance (constrain loadings, intercepts, and residuals)
strict_fit <- sem(measurement_model, data = ebs_data, group = "LANG", group.equal = c("loadings", "intercepts", "residuals"))
summary(strict_fit)
anova(configural_fit,metric_fit, scalar_fit, strict_fit)

#EBS:
# Specify the measurement model
measurement_model <- '
  Feel =~ EBS_feel_1 + EBS_feel_2 + EBS_feel_3 + EBS_feel_4
  Evidence =~ EBS_evid_1 + EBS_evid_2 + EBS_evid_3 + EBS_evid_4
  Politics =~ EBS_polit_1 + EBS_polit_2 + EBS_polit_3 + EBS_polit_4
'

# Fit the configural model (no constraints across groups)
configural_fit <- sem(measurement_model, data = ebs_data, group = "LANG")
summary(configural_fit)

# Metric invariance (constrain loadings)
metric_fit <- sem(measurement_model, data = ebs_data, group = "LANG", group.equal = "loadings")
summary(metric_fit)

# Scalar invariance (constrain loadings and intercepts)
scalar_fit <- sem(measurement_model, data = ebs_data, group = "LANG", group.equal = c("loadings", "intercepts"))
summary(scalar_fit)

# Strict invariance (constrain loadings, intercepts, and residuals)
strict_fit <- sem(measurement_model, data = ebs_data, group = "LANG", group.equal = c("loadings", "intercepts", "residuals"))
summary(strict_fit)
anova(configural_fit,metric_fit, scalar_fit, strict_fit)
