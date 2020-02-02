# -*- coding: utf-8 -*-
"""
Created on Sat Feb  1 08:48:01 2020

@author: Sandy
"""

#######################################
### RTI Aviation Accidents Exercise ###
### Text data processing ##############
### Sandy Preiss ######################
#######################################

import os, json
import pandas as pd
import nltk
nltk.download('stopwords')
import numpy as np
import re
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import Normalizer
import unicodedata
import spacy

#############
### Read JSON files to dataframe 
#############

# find json files
path_to_json = 'C:\\Users\\Sandy\\Documents\\GitHub\\data-scientist-exercise02\\data\\' # folder holding json files
json_files = [pos_json for pos_json in os.listdir(path_to_json) if pos_json.endswith('.json')] # makes list of all json files in folder

# initialize dataframe
narrative = pd.DataFrame(columns=['EventId', 'narrative', 'probable_cause'])

# read json files and append to df
for js in json_files:
    with open(os.path.join(path_to_json, js)) as json_file:
        json_int = pd.read_json(json_file, orient='records')
        json_df = pd.DataFrame([row for row in json_int.data])
        narrative = narrative.append(json_df)
        
# combining narrative and PC fields to analyze together
narrative['narrativeCombo'] = narrative.narrative + ' ' + narrative.probable_cause
narrative.drop(columns=['narrative','probable_cause'], inplace=True)

# check for dups
narrative.EventId.nunique() #none


############
### prep for text analysis
############

# load spacy english model
nlp = spacy.load("en_core_web_sm") 

# Format columns as lists
corpus = narrative.narrativeCombo.values.tolist()
labels = narrative.EventId.values.tolist()

# put together text processing function
def remove_accented_chars(text):
    '''remove any accented characters'''
    text = unicodedata.normalize('NFKD', text).encode('ascii', 'ignore').decode('utf-8', 'ignore')
    return text

def remove_special_characters(text, remove_digits=False):
    '''remove any special characters not caught by above'''
    pattern = r'[^a-zA-z0-9\s]' if not remove_digits else r'[^a-zA-z\s]'
    text = re.sub(pattern, '', text)
    return text

def lemmatize_text(text):
    '''reduce words to their lemmas'''
    text = nlp(text)
    text = ' '.join([word.lemma_ if word.lemma_ != '-PRON-' else word.text for word in text])
    return text

# prep tokenizer function
from nltk.tokenize.toktok import ToktokTokenizer
tokenizer = ToktokTokenizer()
stopword_list = nltk.corpus.stopwords.words('english')

def remove_stopwords(text, is_lower_case=False, stopwords=stopword_list):
    '''tokenize documents and remove stopwords'''
    tokens = tokenizer.tokenize(text)
    tokens = [token.strip() for token in tokens]
    if is_lower_case:
        filtered_tokens = [token for token in tokens if token not in stopwords]
    else:
        filtered_tokens = [token for token in tokens if token.lower() not in stopwords]
    filtered_text = ' '.join(filtered_tokens)    
    return filtered_text

# All the functions together now!
def normalize_corpus(corpus,
                     accented_char_removal=True, text_lower_case=True, 
                     text_lemmatization=True, special_char_removal=True, 
                     stopword_removal=True, remove_digits=True):
    '''combine above functions into main text processing function'''    
    normalized_corpus = []
    # normalize each document in the corpus
    for doc in corpus:
        # remove accented characters
        if accented_char_removal:
            doc = remove_accented_chars(doc)
        # lowercase the text    
        if text_lower_case:
            doc = doc.lower()
        # remove extra newlines
        doc = re.sub(r'[\r|\n|\r\n]+', ' ',doc)
        # lemmatize text
        if text_lemmatization:
            doc = lemmatize_text(doc)
        # remove special characters and\or digits    
        if special_char_removal:
            # insert spaces between special characters to isolate them    
            special_char_pattern = re.compile(r'([{.(-)!}])')
            doc = special_char_pattern.sub(" \\1 ", doc)
            doc = remove_special_characters(doc, remove_digits=remove_digits)  
        # remove extra whitespace
        doc = re.sub(' +', ' ', doc)
        # remove stopwords
        if stopword_removal:
            doc = remove_stopwords(doc, is_lower_case=text_lower_case)
            
        normalized_corpus.append(doc)
        
    return normalized_corpus

# Applying to the data
norm_corpus = normalize_corpus(corpus)

# TF-IDF 
tv = TfidfVectorizer(use_idf=True, min_df=10, max_df=0.8) # only terms w/ absolute freq > 10 and < 80%
tfidf_corpus = tv.fit_transform(norm_corpus)

## LSA
#svd = TruncatedSVD(n_components=1000)
#normalizer = Normalizer(copy=False)
#lsa = make_pipeline(svd, normalizer)
#
#lsa_corpus = lsa.fit_transform(tfidf_corpus)
#
#explained_variance = svd.explained_variance_ratio_.sum()
#print("Explained variance of the SVD step: {}%".format(
#    int(explained_variance * 100)))


#############
### Clustering 
#############

from sklearn.cluster import KMeans

# k means with randomization to avoid local optima
km = KMeans(n_clusters=6, 
            init='k-means++',
            max_iter=100,
            n_init=10,
            random_state=33)

km.fit(tfidf_corpus)

# for each centroid, look up top terms from TFIDF and print
print("Top terms per cluster:")

centroids = km.cluster_centers_
order_centroids = centroids.argsort()[:, ::-1]

terms = tv.get_feature_names()
for i in range(6):
    print("Cluster %d:" % i, end='')
    for ind in order_centroids[i, :10]: # adjust number of terms to pull here
        print(' %s' % terms[ind], end='')
    print()

# attach clusters to IDs
text_cluster_out = pd.DataFrame(labels, columns=['Event.Id'])
text_cluster_out['text_cluster'] = km.labels_

# export to csv for merge with structured data
text_cluster_out.to_csv('C:\\Users\\Sandy\\Documents\\GitHub\\data-scientist-exercise02\\data\\text_cluster.csv', index=False)


