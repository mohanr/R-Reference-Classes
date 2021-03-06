library(plyr)
library(dplyr)
library(purrr)

CompoundUnit <- setRefClass("CompoundUnit",
		fields = list(micrograms = 'numeric',
				cubicmeter = 'numeric'))


Location <- setRefClass("Location",
		fields = list( room = 'character'),
		methods=list(getlocation = function()
				{
					room
				},
				summary = function()
				{
					paste('Room [' , room , ']')
				}))

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

TimePoint <- setRefClass("TimePoint",
		fields = list(time = 'numeric'))


Quantity <- setRefClass("Quantity",
		fields = list(amount = "numeric",
				units = CompoundUnit))


Measurement <- setRefClass("Measurement",
		fields = list(
				quantity = "Quantity",
				timepoint = "TimePoint",
				visit = "Visit"),
		methods=list(getvisit = function(){
					visit$visit
				},getquantity = function(){
					quantity
				})
)


Subject <- setRefClass("Subject",
		fields = list( id = "numeric",
				measurement = "Measurement",
				location = "Location"),
		methods=list(getmeasurement = function()
				{
					measurement
				},
				getid = function()
				{
					id
				},
				getlocation = function()
				{
					location
				},
				summary = function()#Implement other summary methods in appropriate objects as per their responsibilities
				{
					paste("Subject summary ID [",id,"] Location [",location$summary(),"]")
				},show = function(){
					cat("Subject summary ID [",id,"] Location [",location$summary(),"]\n")
				})
)

LongitudinalDatum <- setRefClass("LongitudinalDatum",
		methods=list(subject = function(sub){
						sub$getid()
					},subsummary = function(sub){
						if(is.character(sub) && sub == 'NA'){
							sub
						}else{
							sub$summary()		
						}
					},uniquelocations = function( measurements ){
						locations <- c()
						if( length( measurements ) == 0 ){
							locations <<- c('NA',locations)
						}else{
							measurements %>% map(., function(x) {
										locations <<- c(locations,x$getlocation()$getlocation())
									}
							) 
						}
						u <- unique(unlist(locations),names="FALSE")
						u
					}
				)
)

setwd("D:/eclipse/workspace/pollutantAnalysis")
library(plyr)
library(dplyr)


LongitudinalData <- setRefClass("LongitudinalData",
		contains = "LongitudinalDatum",
		fields = list(measurements = "list"),
		methods=list(
				getmeasurements = function(){
					measurements
				},
				read = function()
				{			
					data <- read.csv("MIE - Copy.csv", header= TRUE)
					data %>% select(visit,room,id,timepoint,value) -> datum
					make_LD( datum )
				},
				make_LD = function( datum )
				{			
					
					data <- read.csv("MIE - Copy.csv", header= TRUE)
					data %>% select(visit,room,id,timepoint,value) -> datum
					
					
					measurements <<- list()
					load(datum)
					
					
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
										visit = visits)
								
								l <- Location$new( room = as.character(row$room))
								
								s <- Subject$new( id =  row$id,
										measurement = m,
										location = l)
								measurements <<- c( measurements, s )
								
							})
					
				},
				getmeasurementslength = function(){
					length(measurements)
				},
				findsubject = function( id ){
					result <- 'NA'
					measurements %>% map(., function(x) {
								if( subject(x) == id){
									result <<- x # Warning message is benign  for this example. result
									#cannot be a class state. It is really local.
								}
							}
					) 
					result
					
				},
				visit = function( sub,v ){
					measurementsvisit <- c()
					if(is.character(sub) && sub == 'NA'){
						measurementsvisit
					}else{
						measurements %>% map(., function(x) {
							m <- x$getmeasurement()
							if (m$getvisit() == v && x$getid() == sub$getid() ){
								measurementsvisit <<- c(measurementsvisit,x)
							}
						}
				
					)
					
					list(visit = measurementsvisit )
				}
			},
				room = function( t, room ){
					if( length( t) == 0 ){
						c('NA')
					}else{
						measurementsvisitroom <- c()
						t$visit %>% map(., function(x) {
							if( x$getlocation()$getlocation() == room )
								measurementsvisitroom <<- c(measurementsvisitroom,x)
						})
						if( length( measurementsvisitroom ) == 0 ){
							c('NA')
						}else{
							measurementsvisitroom
						}
					}
				},
				summaries = function( subjects ){
					summaries <- c()
					if(is.character(subjects) && subjects == 'NA'){
						subjects
					}else{
						measurements %>% map(., function(x) {
									subjects %>% map(., function(y) {
												if (x$getid() == y$getid() ){
													m <<- x$getmeasurement()
													summaries <<- c(summaries,m$getquantity()$amount)
												}
											})
							})
				
					summaries %>% summary
				}
				},subjectsummary = function( subject ){
					filteredmeasurements <-
					keep(measurements, function(x){
								x$getid() == subject$getid()
								})
					groupedmeasurements <-
						filteredmeasurements %>% lapply(function(x){
								m <<- x$getmeasurement()
								as.data.frame(list('visit'=m$getvisit(),
												    'location'=x$getlocation()$getlocation(),
													'amount'=m$getquantity()$amount))
							}) %>% rbind_all() 
					dataColumns <- c('amount')
					ddply(groupedmeasurements,c('visit','location'),function(x) colSums(x[dataColumns]))
				}
					
		) 
)

