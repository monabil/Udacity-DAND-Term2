WeRateDogs is a Twitter account that rates people's dogs with a humorous comment about the dog. It was started in 2015 by college student Matt Nelson, and has received
international media coverage both for its popularity and for the attention drawn to social media copyright law when it was suspended by Twitter. As of the time of writing this report
the account contains 9000+ tweets from over 7 million users. 

In this report I am going to analyze an archive containing the basic data from 5000+ tweets. I will also use Twitter's API to gather some of the missing valuable data like "retweet counts"
and "favorite counts". Additionally, I have a table full of image predictions (the top three only) alongside each tweet ID, image URL, and the image number that corresponded to the
most confident prediction, provided by Udacity team. I will then start wrangling this data to conclude some useful insights and visualizations.
During the wrangling and analysis process, I have engineered two additional features which are, "rating" and "likability" calculated as (rating_numerator / rating_denominator)
and rating∗(retweet_count+favorite_count) respectively. These two features will be helpful in evaluating the dogs posts.

In my analysis, I was interested in finding answers to the following questions:
- What are the most common breeds?
- What are the most frequent dogs names?
- Which dog had the highest rating?
- Which dog was most reachable?
- Which dog was most likable?

Files Included in this project:
- wrangle_act.ipynb: code for gathering, assessing, cleaning, analyzing, and visualizing data
- wrangle_report.pdf or wrangle_report.html: documentation for data wrangling steps: gather, assess, and clean
- act_report.pdf or act_report.html: documentation of analysis and insights into final data
- twitter_archive_enhanced.csv: file as given
- image_predictions.tsv: file downloaded programmatically
- tweet_json.txt: file constructed via API
- twitter_archive_master.csv: combined and cleaned data
