library(objectProperties)
# An Enum which could have behaviour associated with it.
# This is convoluted but the only way I know to represent constants and validate them.
# 
###############################################################################

MeasurementVisitEnum.gen <- setSingleEnum("MeasurementVisit",levels = c('0', '1', '2'))
par.gen <- setRefClass("Visit",
	                   properties(fields = list(visit = "MeasurementVisitSingleEnum"),
		                               prototype = list(visit = new("MeasurementVisitSingleEnum",
													   '0'))))
