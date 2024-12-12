from transformers import pipeline, AutoTokenizer, AutoModelForSequenceClassification
import pandas as pd
from scipy.special import softmax

data = pd.read_csv("../data/tweets/all/july2015.csv", sep='|')

# force text column to string
data["text"] = data["text"].values.astype('str')

data["roberta_negative"] = None
data["roberta_neutral"] = None
data["roberta_positive"] = None


def preprocess_tweet(tweet):
    """remove user tags and urls"""
    tweet_words = []

    for word in tweet.split(' '):
        word = '@user' if word.startswith('@') and len(word) > 1 else word            
        word = 'http' if word.startswith('http') else word  # careful! sometimes emojis are not separated from URLs by spaces, need to take into account
        tweet_words.append(word)
        
    return " ".join(tweet_words)


def roberta_sentiment(tweet, model, tokenizer):
    preprocessed = preprocess_tweet(tweet)
    processed = tokenizer(preprocessed, return_tensors='pt')
    output = model(**processed)
    scores = output[0][0].detach().numpy()
    scores = softmax(scores)
    negative, neutral, positive = scores
    
    return negative, neutral, positive

roberta = "cardiffnlp/twitter-roberta-base-sentiment-latest"
model = AutoModelForSequenceClassification.from_pretrained(roberta)
tokenizer = AutoTokenizer.from_pretrained(roberta)


for index, row in data.iterrows():
    data.at[index, "roberta_negative"], data.at[index, "roberta_neutral"], data.at[index, "roberta_positive"] = roberta_sentiment(row["text"], model, tokenizer)
    
data.to_csv('july2015_roberta_sentiment.csv', sep='|')
