1. Twitter data parsing: 
	- Raw files obtained from GESIS are in the folder data/twitter.
	- (Recommended) Join individual raw .txt files into chunks of multiple days (01-05, 06-10, etc.) for faster processing
		- Can be done using simple bash commands "touch 01-05_07-2015.txt; cat filename >> 01-05_07-2015.txt"
	- To parse chunks of .txt files into readable .csv, use scripts/tweets_to_csv.py 
	(and change parse_file() arguments in main() function accordingly). Creates one .csv for each chunk.
	- Join .csv chunks into one .csv file (only leave the header in the first file)
	- Use scripts/filter_geo.sh to filter only geotagged tweets
	- Add header to the final .csv 
		- (touch july2015.csv; echo "id|username|timestamp|text|geo_available|coordinates" > july2015.csv; cat 07-2015_geo.csv >> july2015.csv)

2. Sentiment analysis:
	- Run scripts/vader_sentiment.py, scripts/roberta_sentiment.py, and scripts/bertweet_sentiment.py. Separate virtual environments might 
	  be necessary, as the imported libraries sometimes create dependency conflicts.
	- For topic classification, run scripts/topic_classification.py
	- The scripts should output four files to data/tweets/all/: july2015_vader_sentiment.csv, july2015_roberta_sentiment.csv, july2015_bertweet_sentiment.csv, and july2015_topics.csv.


3. Data cleaning: 
	- Run scripts/data_cleaning.R. This will create an .RData file in data/ folder that is used for the main data analysis.
	- Run scripts/bots.R. This script produces two .csv files in data/: bot_usernames.csv and day_bot_usernames.csv. These files contain
	  usernames that were evaluated as bots/automated accounts. These are necessary for the main analysis. 
	- Run scripts/hms.R. This will parse and prepare the HMS smoke data.

4. Analysis:
	- Run scripts/models.R. The script contains some further cleaning and data wrangling and all econometric models.
	- Run scripts/descriptive.R. This script contains code for all descriptive statistics, maps and graphs used in the paper. 

