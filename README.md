# Simulation-Exercise---2
Simulate a tournament in R and produce the rankings and the points tables based on some given info.

Basketball League

1)	Write a function named rnorm_round which outputs rounded normal random numbers. More explicitly, here’s what rnorm_round should do:
-	Inputs:
-	n: number of random samples
-	mean: expected value of normal distribution
-	sd: standard deviation of the normal distribution
-	Outputs:
-	A numeric vector with n rounded values that come from a normal with expectation = mean and standard deviation = sd.

In this exercise, we will simulate the outcomes of a basketball league. There are 8 teams in total. Each team has an “attack factor” (A), “defense factor” (D), “home advantage factor” (H), and a “regularity” (R) factor. You can find a table with the teams and their factors below.

Team	A	D	H	R
Jersey Lions	7	7	3	0.5
Westchester Cats	6	8	3	0.25
Long Island Tigers	7	6	4	0.3
Staten Island Dogs	8	6	3	0.25
The Bronx Foxes	9	6	3	0.2
Queens Bears	6	9	5	0.35
Manhattan Ducks	7	6	3	0.2
Brooklyn Rats	8	7	4	0.25
<Use Basketball League.csv shared with this repository to load the above data>

Consider 2 teams, named team 1 and team 2. Let A1, D1, H1, and R1 be the attack, defense, home advantage, and regularity factors for team 1, and let A2, D2, H2, and R2 be the attack, defense, home advantage and regularity factor for team 2. When team 1 plays home against team 2, the point spread “home team - away team” follows a rounded normal distribution (see part 1) with expectation
(0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1 and standard deviation 1/R1+1/R2.
 
2)	Create a function that simulates the outcome of a game: winner, loser, and point spread. If a simulated point spread comes out to be a draw, simulate again until you get a winner and a loser.

3)	[Hint: For this part, you may find the function permutations in library(gtools) useful.] Suppose that the teams play against each other 4 times: 2 at home, 2 away (each team plays 28 games). At the end of the season, the teams will be ranked according to their “wins-losses.” If there are ties, they will be ranked according to their total point spread across the season. If there are ties in both wins-losses and point spreads, the rankings will be assigned in alphabetical order by the team names (i.e. the names Lions, Cats, Tigers, Dogs, Foxes, Bears, Ducks, and Rats). Simulate the outcomes of 100,000 seasons and answer the following questions:
a)	Graph the percentage of time that each teams win the league in your simulations. The teams should appear in descending order by that percentage.
b)	Graph the average rankings of the teams. Compare this graph to the one you created in part a).

4)	Now, suppose that the league works differently. The teams play in a best-of-three playoff (https://en.wikipedia.org/wiki/Playoff_format). [For example: In the first round, team 1 plays team 8. In the second round, whichever teams wins that series will play against the winner of the team 4 vs team 5 series.] In a series (matchup), the team with the highest ranking (number) plays home first. Then, the team with the lowest ranking (number) plays home. If there is a tie after 2 games, the team with the highest ranking gets to play home. Use the average rankings you found in part 3b) to simulate the winner of 100,000 tournaments [For example: the team with the highest average rankings in part 3b) will be Team 1 in the bracket.] Plot your results using an analogue of the plot you created in part 3a). Compare the results you found in part 3) to the results you found in this part. Which system seems fairer? If you were to recommend a system to the league organizers, which one would you recommend and why?
