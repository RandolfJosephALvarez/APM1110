num_repetitions <- 10000

#head=1 tail=0

coin_toss <- sample(c("1", "0"), num_repetitions, replace = TRUE)

prob_head <- mean(coin_toss == "1")
cat("(a) Probability of getting a head in a fair coin toss:", prob_head, "\n")

#drawing card simulation
deck <- rep(c("Red", "Black"), times = c(26, 26))
cards_drawn <- sample(deck, num_repetitions, replace = TRUE)

prob_red_card <- mean(cards_drawn == "Red")
cat("(b) Probability of drawing a red card from a well-shuffled deck:", prob_red_card, "\n")


#fair die simulation
die_roll <- sample(1:6, num_repetitions, replace = TRUE)
prob_even_number <- mean(die_roll %% 2 == 0)
cat("(c) Probability of rolling an even number on a fair die:", prob_even_number, "\n")
