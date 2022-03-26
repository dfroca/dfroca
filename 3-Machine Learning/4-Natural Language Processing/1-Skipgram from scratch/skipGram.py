from __future__ import division
import argparse
import math
from collections import Counter

import pandas as pd
import pickle

# useful stuff
import numpy as np
from scipy.special import expit
from sklearn.preprocessing import normalize
import string
from scipy import stats
from tqdm import tqdm

__authors__ = ["Prajwal Lohan", "Sarah Abdelbar", "Vidhi Mahavir Jain", "Daniel Roca"]
__emails__ = ["prajwal.lohan@student-cs.fr", "sarah.abdelbar@student-cs.fr", "vidhimahavir.jain@student-cs.fr",
              "daniel.roca@student-cs.fr"]


def text2sentences(path):
    # first we read the text, make it lower-case and remove the punctuations and numbers in the sentences
    sentences = []
    with open(path) as f:
        for l in f:
            sentence_list = l.lower().translate(str.maketrans('', '', string.punctuation)) \
                .translate(str.maketrans('', '', string.digits)).split()
            sentences.append(sentence_list)
    return sentences


def loadPairs(path):
    data = pd.read_csv(path, delimiter='\t')
    pairs = zip(data['word1'], data['word2'], data['similarity'])
    return pairs


class SkipGram:
    def __init__(self, sentences=None, nEmbed=100, negativeRate=5, winSize=5, minCount=5, epochs=15, lr=0.0001):

        self.sentences = sentences

        self.nEmbed = nEmbed    # number of embeddings
        self.negativeRate = negativeRate    # number of words to be taken for negative sampling
        self.winSize = winSize  # sliding window size
        self.minCount = minCount  # this is used to consider only those words with min amount of frequency

        self.epochs = epochs
        self.trainWords = 0

        self.lr = lr    # learning rate

    def create_unigram(self):
        # create an array of size of vocabulary, initialize with zero
        word_unigram = np.zeros(len(self.vocab))

        # word frequency 0 corresponds to those which do not come in word_counter dictionary, as they do not contribute to our unigram model
        # uses frequencies of all words
        # frequency raised to power 3/4 gives better results --> increase the probability for less frequent words and decrease the probability for more frequent words

        for eachword, frequency in self.word_counter.items():
            word_unigram[self.w2id[eachword]] = math.pow(frequency, 0.75)

        # normalize
        lollen = np.sum(word_unigram)
        for eachword, frequency in self.word_counter.items():
            word_unigram[self.w2id[eachword]] = word_unigram[self.w2id[eachword]] / lollen
        return word_unigram

    def sample(self, omit):
        """samples negative words, ommitting those in set omit"""

        # total elements we are going to mark as negative

        # negative_elems = int(round(self.negativeRate * len(self.w2id) / 100, 0))  # if negativeRate is to be taken as percentage
        negative_elems = self.negativeRate

        # negative sampling using unigram
        # pick random indexes of elements not in omit indices and also not in word_counter
        # more efficient way of doing the above by considering the probablities
        remove_omitwords = self.word_unigram.copy()
        for eachomit in omit:
            remove_omitwords[eachomit] = 0  # omit words are my context words
        remove_omitwords = remove_omitwords / np.sum(remove_omitwords)  # normalize to bring probabilities sum to 1

        list_negatives = np.random.choice(len(remove_omitwords), size=negative_elems, p=remove_omitwords)

        return list_negatives.astype(int)

    def train(self):
        sentences = self.sentences
        self.words_list = [item for sublist in sentences for item in sublist]
        self.w2id = {token: idx for idx, token in
                     enumerate(set([item for sublist in sentences for item in sublist]))}  # word to ID mapping
        self.trainset = sentences  # set of sentences
        self.vocab = list(self.w2id.keys())  # list of valid words

        # this we will later use to calculate unigram
        self.word_counter = Counter(self.words_list)  # dictionary of word frequency
        self.word_counter = {word: count for word, count in self.word_counter.items() if count >= self.minCount}

        self.word_unigram = self.create_unigram()

        # initialise our word matrix and context matrix
        self.word_mat = np.random.rand(len(self.vocab), self.nEmbed)
        self.cont_mat = np.random.rand(len(self.vocab), self.nEmbed)

        self.loss = []
        for epoch in range(self.epochs):

            self.accLoss = 0

            for counter, sentence in tqdm(enumerate(self.trainset)):  # iterate over each sentence
                sentence = list(filter(lambda word: word in self.vocab, sentence))

                for wpos, word in enumerate(sentence):                              # iterate over each word in the sentence
                    wIdx = self.w2id[word]                                          # get its word to id mapping
                    winsize = np.random.randint(self.winSize) + 1
                    start = max(0, wpos - winsize)
                    end = min(wpos + winsize + 1, len(sentence))

                    for context_word in sentence[start:end]:                        # for each context word in the window around the word
                        ctxtId = self.w2id[context_word]
                        if ctxtId == wIdx: continue
                        negativeIds = self.sample({wIdx, ctxtId})                   # take random negative context ids
                        self.accLoss += self.trainWord(wIdx, ctxtId, negativeIds)   # compute the loss and update the word and context matrix in function trainWord
                        # self.trainWords += 1

            # print("Epoch Loss : ", epoch + 1, " : ", self.accLoss)
            self.loss.append(self.accLoss)

        # print(self.loss)


    def trainWord(self, wordId, contextId, negativeIds):

        # take word to vector mappings for word id, its context word id and the set of negative ids
        x_w = self.word_mat[wordId, :]
        y_c = self.cont_mat[contextId, :]
        z_c = self.cont_mat[negativeIds, :]

        # this helps in matrix multiplication and transposing matrices later. It changes shapes of matrices from (vocab_size,) to (vocab_size,1)
        x_w = x_w[np.newaxis, :]
        y_c = y_c[np.newaxis, :]

        # compute the derivative of loss function w.r.t. the three matrices
        deriv_1 = expit(-np.dot(x_w, y_c.T))
        deriv_2 = expit(np.dot(x_w, z_c.T))

        dL_x_w = -deriv_1 * y_c + np.dot(deriv_2, z_c)
        dL_y_c = -deriv_1 * x_w
        dL_z_c = np.dot(deriv_2.T, x_w)

        # update the word to vec
        x_w = x_w - self.lr * dL_x_w
        y_c = y_c - self.lr * dL_y_c
        z_c = z_c - self.lr * dL_z_c

        # update the word and context matrices with new word to vec
        self.word_mat[wordId, :] = x_w
        self.cont_mat[contextId, :] = y_c
        self.cont_mat[negativeIds, :] = z_c

        # compute the new loss
        L = -np.log(expit(np.dot(x_w, y_c.T))) + np.sum(-np.log(expit(-np.dot(x_w, z_c.T))))

        return L.item()

    def save(self, path):
        # for computing similarity between two words, we need to save only three values: word to id mappings, word matrix, context matrix
        data = {'w2id': self.w2id,
                'word_mat': self.word_mat,
                'cont_mat': self.cont_mat}

        with open(path, 'wb') as f:
            pickle.dump(data, f)

    def similarity_spearman(self, word1, word2):

        # get word ids for the two words
        word1_id = self.w2id.get(word1)
        word2_id = self.w2id.get(word2)

        # if either of the words did not exist in training corpus, map them to first token
        if word1_id is None:
            word1_id = 0

        if word2_id is None:
            word2_id = 0

        # gtet corresponding word to vec
        w1 = self.word_mat[word1_id, :]
        w2 = self.word_mat[word2_id, :]
        spearman = stats.spearmanr(w1, w2)
        return spearman

    def similarity(self, word1, word2):
        """
            computes similiarity between the two words. unknown words are mapped to one common vector
        :param word1:
        :param word2:
        :return: a float \in [0,1] indicating the similarity (the higher the more similar)
        """

        # get word ids for the two words
        word1_id = self.w2id.get(word1)
        word2_id = self.w2id.get(word2)

        # if either of the words did not exist in training corpus, map them to first token
        if word1_id is None:
            word1_id = 0

        if word2_id is None:
            word2_id = 0

        # get corresponding word to vec
        w1 = self.word_mat[word1_id, :]
        w2 = self.word_mat[word2_id, :]

        # calculate the cosine similarity
        cos_sim = np.dot(w1, w2.T) / (np.linalg.norm(w1) * np.linalg.norm(w2))
        return cos_sim

    @staticmethod
    def load(path):
        f = open(path, 'rb')
        tmp_dict = pickle.load(f)

        skipgram = SkipGram()
        skipgram.w2id = tmp_dict["w2id"]
        skipgram.word_mat = tmp_dict["word_mat"]
        skipgram.cont_mat = tmp_dict["cont_mat"]

        return skipgram


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--text', help='path containing training data', required=True)
    parser.add_argument('--model', help='path to store/read model (when training/testing)', required=True)
    parser.add_argument('--test', help='enters test mode', action='store_true')
    opts = parser.parse_args()

    if not opts.test:
        sentences = text2sentences(opts.text)
        sg = SkipGram(sentences, nEmbed=300, negativeRate=5, winSize=5, minCount=5, epochs=10, lr=0.01)
        sg.train()
        sg.save(opts.model)

    else:
        pairs = loadPairs(opts.text)

        sg = SkipGram.load(opts.model)

        for a, b, sim in pairs:
            # make sure this does not raise any exception, even if a or b are not in sg.vocab
            print(sg.similarity(a, b))
