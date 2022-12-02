Day 02
================
Anirudh Govind

## Libraries

``` r
# Load the tidyverse

library(tidyverse)
```

## Part 01

— Day 2: Rock Paper Scissors —

The Elves begin to set up camp on the beach. To decide whose tent gets
to be closest to the snack storage, a giant Rock Paper Scissors
tournament is already in progress.

Rock Paper Scissors is a game between two players. Each game contains
many rounds; in each round, the players each simultaneously choose one
of Rock, Paper, or Scissors using a hand shape. Then, a winner for that
round is selected: Rock defeats Scissors, Scissors defeats Paper, and
Paper defeats Rock. If both players choose the same shape, the round
instead ends in a draw.

Appreciative of your help yesterday, one Elf gives you an encrypted
strategy guide (your puzzle input) that they say will be sure to help
you win. “The first column is what your opponent is going to play: A for
Rock, B for Paper, and C for Scissors. The second column–” Suddenly, the
Elf is called away to help with someone’s tent.

The second column, you reason, must be what you should play in response:
X for Rock, Y for Paper, and Z for Scissors. Winning every time would be
suspicious, so the responses must have been carefully chosen.

The winner of the whole tournament is the player with the highest score.
Your total score is the sum of your scores for each round. The score for
a single round is the score for the shape you selected (1 for Rock, 2
for Paper, and 3 for Scissors) plus the score for the outcome of the
round (0 if you lost, 3 if the round was a draw, and 6 if you won).

Since you can’t be sure if the Elf is trying to help you or trick you,
you should calculate the score you would get if you were to follow the
strategy guide.

For example, suppose you were given the following strategy guide:

A Y B X C Z

This strategy guide predicts and recommends the following:

    In the first round, your opponent will choose Rock (A), and you should choose Paper (Y). This ends in a win for you with a score of 8 (2 because you chose Paper + 6 because you won).
    In the second round, your opponent will choose Paper (B), and you should choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0).
    The third round is a draw with both players choosing Scissors, giving you a score of 3 + 3 = 6.

In this example, if you were to follow the strategy guide, you would get
a total score of 15 (8 + 1 + 6).

What would your total score be if everything goes exactly according to
your strategy guide?

### Data

``` r
# Load in today's input

input <- read_lines(here::here("data/rawData/02.txt"),
                    skip_empty_rows = T)
```

### Wrangle

``` r
# Convert to a data frame

df <- input %>% 
  as.data.frame()

colnames(df) <- "combinations"

# In each, I need to figure out two things:
# What my selection score is?
# Is it a win, loss, or a draw?

# I'll figure the selection score out first

df <- df %>% 
  mutate(temp = combinations) %>% 
  separate(temp,
           into = c("opponent", "me"),
           sep = " ")

# X = 1, Y = 2, Z = 3

df <- df %>% 
  mutate(myPoints = case_when(me == "X" ~ 1,
                             me == "Y" ~ 2,
                             me == "Z" ~ 3))

# And now win, loss, or a draw is dependent on the combinations. -1 is a win for me, 0 is a draw, and 1 is a loss for me

df <- df %>% 
  mutate(result = case_when(combinations == "A X" ~ "draw",
                            combinations == "A Y" ~ "win",
                            combinations == "A Z" ~ "loss",
                            combinations == "B X" ~ "loss",
                            combinations == "B Y" ~ "draw",
                            combinations == "B Z" ~ "win",
                            combinations == "C X" ~ "win",
                            combinations == "C Y" ~ "loss",
                            combinations == "C Z" ~ "draw")) %>% 
  mutate(points = case_when(result == "win" ~ 6,
                            result == "draw" ~ 3,
                            result == "loss" ~ 0)) %>% 
  mutate(totalPoints = myPoints + points)

any(is.na(df))
```

    [1] FALSE

``` r
# My total will be the sum of totalPoints

df %>% 
  summarise(myTotalPoints = sum(totalPoints, na.rm = T))
```

      myTotalPoints
    1         10718

``` r
# 10718
```

## Part 02

— Part Two —

The Elf finishes helping with the tent and sneaks back over to you.
“Anyway, the second column says how the round needs to end: X means you
need to lose, Y means you need to end the round in a draw, and Z means
you need to win. Good luck!”

The total score is still calculated in the same way, but now you need to
figure out what shape to choose so the round ends as indicated. The
example above now goes like this:

    In the first round, your opponent will choose Rock (A), and you need the round to end in a draw (Y), so you also choose Rock. This gives you a score of 1 + 3 = 4.
    In the second round, your opponent will choose Paper (B), and you choose Rock so you lose (X) with a score of 1 + 0 = 1.
    In the third round, you will defeat your opponent's Scissors with Rock for a score of 1 + 6 = 7.

Now that you’re correctly decrypting the ultra top secret strategy
guide, you would get a total score of 12.

Following the Elf’s instructions for the second column, what would your
total score be if everything goes exactly according to your strategy
guide?

``` r
# I'll start with the df and drop columns that I don't want

glimpse(df)
```

    Rows: 2,500
    Columns: 7
    $ combinations <chr> "C Y", "C Z", "C Z", "C Z", "A Y", "C Z", "C Z", "B Y", "~
    $ opponent     <chr> "C", "C", "C", "C", "A", "C", "C", "B", "C", "A", "C", "A~
    $ me           <chr> "Y", "Z", "Z", "Z", "Y", "Z", "Z", "Y", "Y", "X", "Z", "Z~
    $ myPoints     <dbl> 2, 3, 3, 3, 2, 3, 3, 2, 2, 1, 3, 3, 2, 3, 3, 1, 3, 3, 3, ~
    $ result       <chr> "loss", "draw", "draw", "draw", "win", "draw", "draw", "d~
    $ points       <dbl> 0, 3, 3, 3, 6, 3, 3, 3, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, ~
    $ totalPoints  <dbl> 2, 6, 6, 6, 8, 6, 6, 5, 2, 4, 6, 3, 2, 6, 6, 1, 3, 6, 6, ~

``` r
dfNew <- df %>% 
  select(combinations,
         opponent,
         me) %>% 
  rename(desResult = `me`) %>% 
  mutate(result = case_when(desResult == "X" ~ "loss",
                            desResult == "Y" ~ "draw",
                            desResult == "Z" ~ "win"))

# Everywhere its a draw, I need to play the same as my opponent.

dfNew <- dfNew %>% 
  mutate(me = case_when(opponent == "A" & result == "draw" ~ "A",
                        opponent == "A" & result == "win"  ~ "B",
                        opponent == "A" & result == "loss" ~ "C",
                        opponent == "B" & result == "draw" ~ "B",
                        opponent == "B" & result == "win"  ~ "C",
                        opponent == "B" & result == "loss" ~ "A",
                        opponent == "C" & result == "draw" ~ "C",
                        opponent == "C" & result == "win"  ~ "A",
                        opponent == "C" & result == "loss" ~ "B")) %>% 
  mutate(points = case_when(result == "win" ~ 6,
                            result == "draw" ~ 3,
                            result == "loss" ~ 0)) %>%
  mutate(myPoints = case_when(me == "A" ~ 1,
                             me == "B" ~ 2,
                             me == "C" ~ 3)) %>% 
  mutate(totalPoints = myPoints + points)

any(is.na(dfNew))
```

    [1] FALSE

``` r
# My total will be the sum of totalPoints

dfNew %>% 
  summarise(myTotalPoints = sum(totalPoints, na.rm = T))
```

      myTotalPoints
    1         14652

``` r
# 14652
```
