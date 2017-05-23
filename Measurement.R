Measurement <- setRefClass("Measurement",
		fields = list(
				      quantity = "Quantity",
					  timepoint = "TimePoint"))
	  
u <- CompoundUnit$new(  micrograms = 100,
					    cubicmeter = 1 )

q <- Quantity$new(amount = 100,
				  units = u )
				  
t <-  TimePoint$new(time = 50)
		  
m <- Measurement$new(
					 quantity = q,
			 		 timepoint = t )
print( m$quantity$amount )
