Thank you very much for sharing the code! Here are my comments:

1. Although your code is well-written and did an extensive work, I could not find where your results in the paper are. This is because even though your paper's DV is voting share of incumbent parties, the main DV in the paper is turnout.
I could not have the time to read the paper of your original article or go through the detail of coding so I probably miss something, but for all the outcomes, I could not find the estimates that correspond to your main analyses.
I even tried to create the voting share of incumbent parties, but it also gave me the different estimates. Because I could not replicate your results, my comments below might be a bit off the point.

2. I think it is better to consider the research design a bit more carefully. You did a lot of modelling, but in my own experience, the most important thing is causal identification and the corresponding research design. The nature of your data is a bit hard, since it is panel at the state level for each election year. And I think the mass shooting and black arson can be endogeneous to political situations in each state at that time, so I think it is important to address them. Once potential idea is that since your data contains the date of such disastrous events (I assume "date_dis" corresponds to this), this variation might be useful to identify the causal effect as the micro-variation of disaster dates might be considered as random. At least, I recommend you to explore the sensitivity analyses of your finding, as I did in the extension code. Also, as you tried in the code, PanelMatch can be one option, but if you do it, I first recommend you to visualize the treatment histories as done in Imai, Kim, and Wang (2023). Also, you need to carefully examine the parallel trend assumption.

3. Small point, but why do you include fatalities of shootings and injuries of shootings? I assume that you are inherently interested in the effect of mass shooting, and even though the data is not fine-grained that it only has the election-year one, fatalities and injuries seem post-treatment so probably you should not include them.