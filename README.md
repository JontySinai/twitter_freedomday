# Freedom Day Twitter Analysis

This is an analysis of tweets related to Freedom Day (27th April) which marks the anniversary of the first democratic election in modern South Africa. The tweets were streamed over a 30 hour period beginning Wed April 26 16:17:26 GMT+2:00 (local Johannesburg time) and ending Thu April 27 22:17:26 GMT+2:00. 

The stream tracked tweets with the hasthags "#FreedomDay", "#SAUnite", "#savesa","#FreedomMovement" and "#27April". Where geo-tags were avialble, the stream was restricted to the geographic region with South-West corner -35 S 16 E and North-East corner -21 S 34 E (this rectangle full contains South Africa's geographic region).

54k tweets were streamed and stored in the original JSON file (at 152.3 mb this file is too big for Github) but unfortunately only 26k of these tweets were parsed by the `parseTweets` function from the package `streamR`. In another version of this analysis, it will be interesting to see how the results change when all of the tweets can be parsed from the original JSON. 

As a result of the limited parsing, the dataframe [tweets_freedom_day](tweets_freedom_day.feather) only contains tweets up to Thu April 27 12:10:45 GMT+2:00. 

Note: the Twitter timestamp contained in this dataframe is set to GMT, so the Johannesburg time is timestamp + 2:00 hours.

***

There is an implicit ordering to how the streaming and cleaning files should be run for the first time:

[twitter_stream](twitter_stream.rmd) -> [clean_tweets](clean_tweets.rmd) -> [clean_text](clean_text.rmd) -> [clean_sentiment](clean_sentiment.rmd) -> [clean_emojis](clean_emojis.rmd)

After which the '\*\_analysis.rmd' files can be run and knitted to produce the markdown files for display. 

***

I have included three PNG images which are needed for the visualisations. These are immutable.

***

J Sinai  
RMB FOUNDeRY  
Johannesburg  
May 2017  

