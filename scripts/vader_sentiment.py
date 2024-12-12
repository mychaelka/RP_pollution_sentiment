from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import pandas as pd

data = pd.read_csv("../data/tweets/all/july2015.csv", sep='|')

# force text column to string
data["text"] = data["text"].values.astype('str')

# create columns for the specific sentiment model
data["vader_compound"] = None

def preprocess_tweet(tweet):
    """remove user tags and urls"""
    tweet_words = []

    for word in tweet.split(' '):
        word = '@user' if word.startswith('@') and len(word) > 1 else word            
        word = 'http' if word.startswith('http') else word  # careful! sometimes emojis are not separated from URLs by spaces, need to take into account
        tweet_words.append(word)
        
    return " ".join(tweet_words)


def vader_sentiment(tweet):
    preprocessed = preprocess_tweet(tweet)
    
    # Create a SentimentIntensityAnalyzer object
    sid_obj = SentimentIntensityAnalyzer()

    sentiment_dict = sid_obj.polarity_scores(tweet)
    return sentiment_dict['compound']  # compound sentiment is most widely used as a single unidimensional measure of sentiment for a given sentence


for index, row in data.iterrows():
    data.at[index, "vader_compound"] = vader_sentiment(row["text"])


data.to_csv('../data/tweets/all/july2015_vader_sentiment.csv', sep='|')
