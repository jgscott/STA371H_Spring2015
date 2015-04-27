currentwealth = 1000
rounds = 10000


frac = 0.1

runningwealth = rep(0, rounds) # a placeholder for the result
for(i in 1:rounds) {
  bet = frac*currentwealth
  coin = rbinom(1,1,0.52)
  if(coin==0) {
    currentwealth = currentwealth - bet
  }
  else {
    currentwealth = currentwealth + bet
  }
  runningwealth[i] = currentwealth
}
plot(runningwealth)
