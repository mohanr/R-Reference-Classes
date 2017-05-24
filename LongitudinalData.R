setwd("D:/eclipse/workspace/pollutantAnalysis")

library(dplyr)
library(plyr)

LongitudinalData <- setRefClass("LongitudinalData",
		fields = list(measurements = "list"),
		methods=list(make_LD = function(x)
				{			

					data <- read.csv("MIE - Copy.csv", header= TRUE)
					data %>% select(visit,room,id,timepoint,value) -> datum
					
					load(datum)
					
					measurements <<- list()
		
				},load = function( df ){
					by(df, 1:nrow(df), function(row) {
								visits <- par.gen$new()
								visits$visit <- as.character(row$visit)
								
								u <- CompoundUnit$new(  micrograms = 1,
														cubicmeter = 1 )
												
								q <- Quantity$new(amount = row$value,
												  units = u )
												
								t <-  TimePoint$new(time = row$timepoint)
								
								m <- Measurement$new(
										quantity = q,
										timepoint = t,
										Visit = visits)
								
								l <- Location$new( room = as.character(row$room))
								
								s <- Subject$new( id =  row$id,
										measurement = m,
										location = l)
								
								measurements <<- c( measurements, s )
							})
					
					}
				
				) 
)


ld <- LongitudinalData$new()
ld$make_LD(1)
