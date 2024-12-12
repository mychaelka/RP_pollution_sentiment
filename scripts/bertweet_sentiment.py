from pysentimiento import create_analyzer
from pysentimiento.preprocessing import preprocess_tweet
import pandas as pd

data = pd.read_csv("../data/tweets/all/july2015.csv", sep='|')

# force text column to string
data["text"] = data["text"].values.astype('str')

# create columns for different the specific sentiment model
data["bertweet_negative"] = None
data["bertweet_neutral"] = None
data["bertweet_positive"] = None

def preprocess_tweet(tweet):
    """remove user tags and urls"""
    tweet_words = []

    for word in tweet.split(' '):
        word = '@user' if word.startswith('@') and len(word) > 1 else word
        word = 'http' if word.startswith(
            'http') else word  # careful! sometimes emojis are not separated from URLs by spaces, need to take into account
        tweet_words.append(word)

    return " ".join(tweet_words)


analyzer = create_analyzer(task="sentiment", lang="en")

def bertweet_sentiment(tweet, analyzer):
    processed = preprocess_tweet(tweet)
    result = analyzer.predict(tweet)

    return result


for index, row in data.iterrows():
    probas = bertweet_sentiment(row["text"], analyzer).probas
    data.at[index, "bertweet_positive"], data.at[index, "bertweet_neutral"], data.at[index, "bertweet_negative"] = probas['POS'], probas['NEU'], probas['NEG']


data.to_csv('../data/tweets/all/july2015_bertweet_sentiment.csv', sep='|')
