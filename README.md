# simple-linear-reg
Regression analysis of Vegas travelers (Continuation of vegas-reviews)

The tourism industry is the lifeblood of my hometown of Las Vegas. As a long time Vegas resident, I am interested in examining tourism trends in the city. I am interested in the association between the types of hotel guests who travel to Las Vegas and their likelihood of giving a top (5-star) rating of the hotel at which they stayed. A dataset of 492 hotel reviews from the travel site tripadvisor.com will be used to explore this topic. 

In a preliminary analysis of the data, it appears that business travelers are less likely than other types of travelers to give a property a 5-star rating. To further test this theory, a simple regression analysis with business travelers as the explanatory variable and score as the response variable will be run. Given the dichotomous nature of the response variable (i.e.: 1 = 5-star rating & 0 = any other rating) I will perform a simple logistic regression. A summary of the beta estimates from this model can be used to predict the risk of a low score (< 5) by business travelers. Furthermore, a calculation of the odds ratio will help quantify the magnitude of any association.

This is a dataset of 492 guest reviews from the travel site tripadvisor.com. The reviews contain both numerical and categorical variables about 21 Las Vegas hotels in addition to data about the individual reviewers. Reviewers are asked to rate their overall satisfaction level on a scale of 1 to 5 (1 being the lowest, 5 being the highest). Reviewers also provided personal information about themselves such as where they are from and who they traveled with. The dataset also contains information about property amenities like pool, spa, casino, free internet, etc. For the sake of simplicity, I cleansed the dataset of all variables except the ones of interest, “Score” and “Traveler.Type”. I also created two additional categorical variables for the purpose of running my analysis. The original dataset can be found at https://www.kaggle.com/crawford/las-vegas-tripadvisor-reviews. 

Primary question of interest?

Are business travelers more likely to give lower scores (that is, scores < 5) than other types of travelers? If so, what is the odds ratio of a low score by a business traveler versus a low score by any other type of traveler?

Conclusion:

The results of the simple logistic regression show that there is, in fact, a correlation between business travelers and lower hotel ratings. More specifically, business travelers are about twice as likely to give a rating less than 5 versus other types of travelers.
