
LongitudinalData <- setRefClass("LongitudinalData",
		methods=list(make_LD = function(x)
				{			

					u <- CompoundUnit$new(  micrograms = 100,
							cubicmeter = 1 )
					
					q <- Quantity$new(amount = 100,
							units = u )
					
					t <-  TimePoint$new(time = 50)
					
					visits <- par.gen$new()
					visits$visit <- '0'
					
					m <- Measurement$new(
							quantity = q,
							timepoint = t,
							Visit = visits)
					
					l <- Location$new( room = "bedroom")
					
					s <- Subject$new( id = 10,
							measurement = m,
							location = l)
					
					print( m$quantity$amount )				} ) 
)


ld <- LongitudinalData$new()
ld$make_LD(1)
