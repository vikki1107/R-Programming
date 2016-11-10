############################################################################################################################
# Probability-Ticket line simulation                                                                                       #
# ------------------------------------                                                                                     #
# Suppose n people have a $5 bill, and n people have a $10 bill. They line up single file one night to buy a ticket that   # 
# costs $5. The agent selling tickets has some given number of $5 bills at the beginning of the night. No one ever changes #
# place in line. If the agent does not have change for the next customer, the ticket booth shuts down. Ignore the fact     #
# this would not happen with real human beings. Each possible ordering of the customers is equally likely. What is the     #
# probability that every customer who lined up at the beginning of the process will be able to purchase a ticket?          #
# Input arguments:                                                                                                         #
#     1. n is the number of people in line who have a $5 bill (nothing but number of people who have a $10 bill).          #
#     2. start.5 is the number of $5 bills the ticket taker has when all 2n people are in line.                            #
#     3. sim.length should also be a positive integer. It is the number of times a ticket line will be simulated.          #
# Output:                                                                                                                  #
#     1. Probability that every customer will be able to purchase the ticket.                                              #  
# The way the function works is that it first checks if n, start.5 and sim.length is numeric and is a positive integer     #
# where for start.5 it checks if its a non-negative integer. Then we will simulate the task by sim.length times using for  #
# loop. Create a random sample of 2n $5 and $10 bills in terms of 1 and -1 respectively using rep function where we have   #
# n $5 and n $10 bills, again that is n 1's and n -1's. Now it adds each element of x to the total number of 5$ bill       #
# It then checks if the total $5 bills is negative. If it is then it will increment another counter called booth.shutdown  #
# by 1 and break out of the loop. Finally we will calculate the probability by considering the booth.shutdown count with   #
# the sim.length which tell us how many of the randomly selected ticket lines allow the last customer to purchase a ticket.#
############################################################################################################################

# ticket.line function
ticket.line <- function(n,start.5,sim.length)
{ # start of ticket.line function
  # Conditions to check if n, start.5 and sim.length contains the right value
  if ( !(is.numeric(n) && length(n) == 1 && n > 0 && n%%1 == 0) ) 
  { stop("Either the 1st argument/n is not numeric or is not a vector of length 1 or is not a positive integer") }
  if ( !(is.numeric(start.5) && length(start.5) == 1 && start.5 >= 0 && start.5%%1 == 0) ) 
  { stop("Either the 2nd argument/start.5 is not numeric or is not a vector of length 1 or is not a non-negative integer") }
  if ( !(is.numeric(sim.length) && length(sim.length) == 1 && sim.length > 0 && sim.length%%1 == 0) ) 
  { stop("Either the 3rd argument/sim.length is not numeric or is not a vector of length 1 or is not a positive integer") }
  
  booth.shutdown = 0                         # Call a new variable which tells count of ticket booth shutdown
  
  for (i in 1:sim.length)
  { # start for loop for sim.length
    total.5 = start.5                       # Assign start.5 to another variable so that when the loop comples it resets. 
    x = sample(rep(c(1,-1),n),2*n)          # create a random sample of 5 and 10 2n times in terms of 1 and -1 respectively.
    
    for (j in 1:(2*n))
    { # start for loop for 2n samples
      total.5 = total.5 + x[j]              # Add the each element of x to the total number of $5 bills. 
      
      # condition to check if total.5 is negative. If it is then increment the counter booth.shutdown
      # by 1 as the booth will close if agent do not have any change left to give and break out
      if (total.5 < 0)
      { # start if condition
        booth.shutdown = booth.shutdown + 1
        break
      } # end if condition
    } # end for loop for 2n samples
  } # end for loop for sim.length
  
  return((sim.length - booth.shutdown)/sim.length)   # Return the probability of customers getting ticket. 
} # end of ticket.line function