# Bayesian-Poll-of-Polls
Stan code for creating a Bayesian poll of polls for multiparty systems

# How to use
The stan model expects the following data:
- T: an integer representing the total number of time steps in the model
- J: an integer representing the total number of parties in the model
- N: integer number of observations in the data (not necessarily the same as T)
- P: integer number of unique pollsters
- t: integer time step for each observation. Assumed to go from 1 to T.
- vi: N by J matrix of poll results. Set to -1 if a party is missing.
- pollster: integer id for the pollster of each observation. Should range from 1 to P
- size: vector of sample sizes for each poll
- vote0: vector used to initialize the state variable; should contain one value per party and sum to 1

Once you have the data in this format, you can compile and run the code with your preferred Stan interface. I have a couple of examples that use [cmdstanr](https://mc-stan.org/cmdstanr/) to produce graphs from [Europe Elects polling data](https://europeelects.eu/data/) in the [scripts folder](/scripts).
