#
# Name: Natasia Limiardi
# email: natasia@bu.edu
#
# Partner: Beatrice Tanaga
# email: btanaga@bu.edu
#
# finalproject.py
#

import math

def clean_text(txt):
    """ removes the punctuations from the text txt
    """
    upper_text = txt.upper()
    clean_txt = ''
    for letter in range(len(upper_text)):
        if upper_text[letter] not in ['.', ',', '?', '!', ';', '-', '"']:
            clean_txt += upper_text[letter]

    return clean_txt


def stem(s):
    """ accepts a string as a parameter and return the stem of s
    """
    if s[-3:] == 'ing':
        if s[-4] == s[-5]:
            s = s[:-4]
        elif s[-4] == 'v':
            s = s[:-4] + 'e'
        else:
            s = s[:-3]
    elif s[-2:] == 'er':
        if s[-3] == s[-4]:
            s = s[:-3]
        else:
            s = s[:-2]
    elif s[-2:] == 'ed':
        if s[-3] == s[-4]:
            s = s[:-3]
        else:
            s = s[:-2]
    elif s[-3:] == 'ies':
        s = s[:-3] + 'y' 
    elif s[-2:] == 'es':
        s = s[:-2]
    elif s[-2:] == 'ly':
        s = s[:-2]
    elif s[-4:] == 'lies':
        s = s[:-3] + 'y'
    elif s[-5:] == 'liest':
        s = s[-4] + 'y'
    elif s[-4:] == 'less':
        s = s[:-4]
    elif s[-4:] == 'ness':
        s = s[:-4]
    elif s[-3:] == 'ism':
        s = s[:-3]
    elif s[-3:] == 'ion':
        s = s[:-3]
    elif s[-3:] == 'ary':
        if s[-4] == 'n' or s[-4] == 't':
            s = s[:-3]
    elif s[-1] == 's':
        singular = s[:-1]
        return stem(singular)
    else:
        s = s

    return s

def common_words(txt):
    """ finds the list of 100 most common words in English """
    lower_txt = txt.lower()
    txt_split = lower_txt.split()
    common_list = []
    common_words_list = ['the', 'be', 'to', 'of', 'and', 'a', 'in', 'that', 'have', 
                        'I', 'it', 'for', 'not', 'on', 'with', 'he', 'as', 'you', 
                        'do', 'at', 'this', 'but', 'his', 'by', 'from', 'they', 'we', 
                        'say', 'her', 'she', 'or', 'an', 'will', 'my', 'one', 'all', 
                        'would', 'there', 'their', 'what', 'so', 'up', 'out', 'if', 
                        'about', 'who', 'get', 'which', 'go', 'me', 'when', 'make', 
                        'can', 'like', 'time', 'no', 'just', 'him', 'know', 'take', 
                        'person', 'into', 'year', 'your', 'good', 'some', 'could', 
                        'them', 'see', 'other', 'than', 'then', 'now', 'look', 
                        'only', 'come', 'its', 'over', 'think', 'also', 'back', 'after',
                        'use', 'two', 'how', 'our', 'work', 'frist', 'well', 'way', 'even',
                        'new', 'want', 'because', 'any', 'these', 'give', 'day', 'most', 'us'] 
    for i in range(len(txt_split)):
        if txt_split[i] in common_words_list:
            common_list += [txt_split[i]]
        else:
            common_list = common_list
    return common_list

def compare_dictionaries(d1, d2):
    """ compare the scores of dictionaries and look at their similarities"""
    score = 0
    d1_list = list(d1)
    d2_list = list(d2)
    Total= sum(d1.values())

    for i in d2_list:
        if i in d1_list:
            score += d2[i]*math.log(d1[i]/Total)
        else:
            score += d2[i]*math.log(.5/Total)
    return score

         
#def sample_file_write(filename):
#    """ A function that demonstrates how to write a
#        Python dictionary to an easily-readable file.
#    """
#    d = {'test': 1, 'foo': 42}   # Create a sample dictionary.
#    f = open(filename, 'w')      # Open file for writing.
#    f.write(str(d))              # Writes the dictionary to the file.
#    f.close()                    # Close the file.

#def sample_file_read(filename):
#    """ A function that demonstrates how to read a
#        Python dictionary from a file.
#    """
#    f = open(filename, 'r')    # Open for reading.
#    d_str = f.read()           # Read in a string that represents a dict.
#    f.close()

#    d = dict(eval(d_str))      # Convert the string to a dictionary.

#    print("Inside the newly-read dictionary, d, we have:")
#    print(d)

def test():
    """ test the whole funtion with two test inputs"""
    source1 = TextModel('source1')
    source1.add_string('It is interesting that she is interested.')

    source2 = TextModel('source2')
    source2.add_string('I am very, very excited about this!')

    mystery = TextModel('mystery')
    mystery.add_string('Is he interested? No, but I am.')
    mystery.classify(source1, source2)

def run_tests():
    """ test similarities of the four test texts, two being the left out text form each corresponding source"""
    source1 = TextModel('The Wall Street Journal')
    source1.add_file('WSJ.txt')

    source2 = TextModel('The New York Times')
    source2.add_file('NYT.txt')

    new1 = TextModel('NYT cut out')
    new1.add_file('NYT_leftout.txt')
    new1.classify(source1, source2)
    
    print()

    new2 = TextModel('WSJ cut out')
    new2.add_file('WSJ_leftout.txt')
    new2.classify(source1, source2)

    print()

    new3 = TextModel('The Boston Globe')
    new3.add_file('TBG_trump.txt')
    new3.classify(source1, source2)

    print()

    new4 = TextModel('BBC Turkey')
    new4.add_file('BBC_turkey.txt')
    new4.classify(source1, source2)
        

    
class TextModel:

    def __init__(self, model_name):
        """ constructs a new TextModel object by accepting a string
            model_name as a parameter and initializing three attributes,
            (1) name: uses model_name as a string label for the text model
            (2) words: a dictionary that records the number of times each
                       word appeaers in the text
            (3) word_length: a dictionary that records the number of times
                             each word length appears
        """
        self.name = model_name
        self.words = {}
        self.word_lengths = {}
        self.stems = {}
        self.sentence_lengths = {}
        self.common_words = {}
        

    def __repr__(self):
        """ Return a string representation of the TextModel
        """
        s = 'text model name: ' + self.name + '\n'
        s += '  number of words: ' + str(len(self.words)) + '\n'
        s += '  number of word lengths: ' + str(len(self.word_lengths)) + '\n'
        s += '  number of stems: ' + str(len(self.stems)) + '\n'
        s += '  number of sentence lengths: ' + str(len(self.sentence_lengths)) + '\n'
        s += '  number of the most common words: ' + str(len(self.common_words)) + '\n'

        return s

    def add_string(self, s):
        """ Analyzes the string txt and adds its pieces
            to all of the dictionaries in this text model.
        """
        clean_s = clean_text(s)
        word_list = clean_s.split()

        sentence_list = s.split()
        current_sentence_len = 0
        for i in range(len(sentence_list)):
            if sentence_list[i][-1] in '.,?!':
                len_sentence = len(sentence_list[:i + 1]) - current_sentence_len
                current_sentence_len = len_sentence
                if len_sentence not in self.sentence_lengths:
                    self.sentence_lengths[len_sentence] = 1
                else:
                    self.sentence_lengths[len_sentence] += 1

        for w in word_list:
            if w not in self.words:
                self.words[w] = 1
            else:
                self.words[w] += 1
        
        current_length = len(word_list[0])
        for w in word_list:
            if len(w) not in self.word_lengths:
                self.word_lengths[len(w)] = 1
            else:
                self.word_lengths[len(w)] += 1

        for w in word_list:
            if stem(w) not in self.stems:
                self.stems[stem(w)] = 1
            else:
                self.stems[stem(w)] += 1

        
        common_list = common_words(clean_s)
        for i in common_list:
            if i not in self.common_words:
                self.common_words[i] = 1
            else:
                self.common_words[i] += 1

        for x in self.common_words:
            self.common_words[x] = 100*(self.common_words[x]/(len(word_list)))
        
    def add_file(self, filename):
        """ adds all of the text in the file filename to the model
        """
        file = open(filename, 'r', encoding='utf8', errors='ignore')
        text = file.read()
        file.close()

        self.add_string(text)

    def save_model(self):
        """ saves the TextModel object self by writing its various feature
            dictionaries to files
        """
        words_filename = self.name + '_' + 'words'

        d1 = self.words                     # Create a sample dictionary.
        f1 = open(words_filename, 'w')      # Open file for writing.
        f1.write(str(self.words))           # Writes the dictionary to the file.
        f1.close()                          # close the file.
        

        word_lengths_filename = self.name + '_' + 'word_lengths'

        d2 = self.word_lengths                    # Create a sample dictionary.
        f2 = open(word_lengths_filename, 'w')     # Open file for writing. the
                                                  # word_length_filename is a
                                                  # variable we just made to assign the
                                                  # wriiting to, not the dictionary
        f2.write(str(self.word_lengths))          # Writes the dictionary to the file.
        f2.close()                                # close the file.

        stems_filename = self.name + '_' + 'stems'

        d3 = self.stems
        f3 = open(stems_filename, 'w')
        f3.write(str(self.stems))
        f3.close

        sentence_lengths_filename = self.name + '_' + 'sentence_lengths'

        d4 = self.sentence_lengths
        f4 = open(sentence_lengths_filename, 'w')
        f4.write(str(self.sentence_lengths))
        f4.close

        common_words_filename = self.name + '_' + 'common_words'

        d5 = self.common_words
        f5 = open(sentence_length_filename, 'w')
        f5.write(str(self.common_words))
        f5.close
        
    def read_model(self):
        """ reads the stored dictionaries for the called TextModel object
            from their files and assigns them to the attributes of the
            called TextModel
        """
        words_filename = self.name + '_' + 'words'

        f1 = open(words_filename, 'r')
        d1_str = f1.read()
        f1.close()

        d1 = dict(eval(d1_str))

        self.words = d1
        
        word_lengths_filename = self.name + '_' + 'word_lengths'
        
        f2 = open(word_lengths_filename, 'r')
        d2_str = f2.read()
        f2.close()

        d2 = dict(eval(d2_str))

        self.word_lengths = d2

        stems_filename = self.name + '_' + 'stems'

        f3 = open(stems_filename, 'r')
        d3_str = f3.read()
        f3.close()

        d3 = dict(eval(d3_str))

        sentence_lengths_filename = self.name + '_' + 'sentence_lengths'

        f4 = open(sentence_lengths_filename, 'r')
        d4_str = f4.read()
        f4.close()

        d4 = dict(eval(d4_str))

        common_words_filename = self.name + "_" + "common_words"
        
        d5 = open(common_words_filename, 'r')
        d5_str = f5.read()
        f5.close()

        d5 = dict(eval(d5_str))


    def similarity_scores(self, other):
        """ computes and returns a list of log similarity scores measuring
        the similarity of self and other – one score for each type of feature
        (words, word lengths, stems, sentence lengths, and your additional feature).
        You should make repeated calls to compare_dictionaries, and put the resulting
        scores in a list that the method returns."""

        word_score = compare_dictionaries(other.words, self.words)
        word_lengths = compare_dictionaries(other.word_lengths, self.word_lengths)
        stem = compare_dictionaries(other.stems, self.stems)
        sentence_lengths = compare_dictionaries(other.sentence_lengths, self.sentence_lengths)
        common_words = compare_dictionaries(other.common_words, self.common_words)

        Total = [word_score] + [word_lengths] + [stem] + [sentence_lengths] + [common_words]
        
        return Total

    def classify(self, source1, source2):
        """compares the called TextModel object (self) to two other “source” TextModel objects (
        source1 and source2) and determines which of these other TextModels is the more likely
        source of the called TextModel."""

        scores1 = self.similarity_scores(source1)
        scores2 = self.similarity_scores(source2)

        print("scores for " + source1.name + " : " + str(scores1))
        print("scores for " + source2.name + " : " + str(scores2))

        scores1_score = 0
        scores2_score = 0

        for x in range(len(scores1)):
            if scores1[x] > scores2[x]:
                scores1_score += 1
            else:
                scores2_score += 2

        if scores1_score > scores2_score:
            print( self.name + " is more likely to have come from " + source1.name)
        else:
            print( self.name + " is more likely to have come from " + source2.name)
