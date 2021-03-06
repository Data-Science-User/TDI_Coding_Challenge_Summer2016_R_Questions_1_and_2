# The knight makes TT jumps to other keys according to its allowable moves 
# (so that from each key it has two or three allowable moves). 
# The knight chooses amongst the allowable moves uniformly at random and 
# keeps track of the running sum SS of keys on which it lands. 
# We are interested in SS under the modulo operator.

# Function that simulates the problem
T = list() # Counter of the number of moves
list_keys = list() # list of keys landed
List_of_Moves = list("0" = list(4,6), "1" = list(6,8), "2" = list(7,9), 
                     "3" = list(4,8), "4" = list(3,9,0), "6" = list(1,7,0), 
                     "7" = list(2,6), "8" = list(1,3), "9" = list(2,4))
sim <- function(init = "0", size) {
  if(size == 0) return(NULL)
  else {
    tmp <- sample(List_of_Moves[[as.character(init)]], 1)
    return(c(unlist(tmp), sim(tmp, size - 1)))
  }
}
Sum_keys = sum(unlist(list_keys))      # Sum of the keys landed
Sum_keys %% 10

# Q1: After T=10 moves, what is the expected value of the quantity of S mod 10?
x = replicate(1000000, {sum(sim(init = "0", size = 10)) %% 10})

prop.table(table(x))

y <- c(0,1,2,3,4,5,6,7,8,9)

p <- c(0.082710, 0.111331, 0.093211, 0.110218, 0.085769, 0.117118, 
       0.085703, 0.109429, 0.092786,0.111725)

weighted.mean(y,p) # 4.585107

After 10 moves, the expected value is 4.585107

# Q2: After T=10 moves, what is the standard deviation of the quantity of S mod 10?
sd(x) # 2.861564

After 10 moves, the standard deviation is 2.861564

# Q3: After T=1024 moves, what is the expected value of the quantity of S mod 1024?
options(expressions = 500000)
x_1024 = replicate(100000, {sum(sim(init = "0", size = 1024)) %% 1024})

tab_x_1024 = prop.table(table(x_1024))

weighted.mean(as.numeric(names(tab_x_1024)), (tab_x_1024/sum(x))) # 511.264

After 1024 moves, the expected value is 511.264

# Q4: After T=1024 moves, what is the standard deviation of the quantity of S mod 1024?
sd(x_1024) # 70.21152

After 1024 moves, the standard deviation is 70.21152

# Q5: After T=10 moves, what is the probability that the sum S is divisible by 5, 
# given that it is divisible by 7.
x_cond_prob_1 <- replicate(1000000, sum(sim(init = "0", size = 10)))

db5 <- x_cond_prob_1 %% 5 == 0
db7 <- x_cond_prob_1 %% 7 == 0

meh<- table(db5, db7) / length(x_cond_prob_1)

sum(db5 & db7) / sum(db7) # 0.1813455

After 10 moves, there is a 18% probability that the sum is divisible by 5.

# Q6:  After T=1024 moves, what is the probability that the sum S is divisible by 23, 
# given that it is divisible by 29.
x_cond_prob_2 <- replicate(100000, sum(sim(init = "0", size = 1024)))

db23 <- x_cond_prob_2 %% 23 == 0
db29 <- x_cond_prob_2 %% 29 == 0

sum(db23 & db29) / sum(db29) # 0.1207658

After 1024 moves, there is a 12% probability that the sum is divisible by 23.


