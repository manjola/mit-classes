# 6.0001/6.00 Problem Set 5 - RSS Feed Filter
# Name: MM
# Collaborators:
# Time:

import feedparser
import string
import time
import threading
from project_util import translate_html
from mtTkinter import *
from datetime import datetime
import pytz


#-----------------------------------------------------------------------

#======================
# Code for retrieving and parsing
# Google and Yahoo News feeds
# Do not change this code
#======================

def process(url):
    """
    Fetches news items from the rss url and parses them.
    Returns a list of NewsStory-s.
    """
    feed = feedparser.parse(url)
    entries = feed.entries
    ret = []
    for entry in entries:
        guid = entry.guid
        title = translate_html(entry.title)
        link = entry.link
        description = translate_html(entry.description)
        pubdate = translate_html(entry.published)

        try:
            pubdate = datetime.strptime(pubdate, "%a, %d %b %Y %H:%M:%S %Z")
            pubdate.replace(tzinfo=pytz.timezone("GMT"))
          #  pubdate = pubdate.astimezone(pytz.timezone('EST'))
          #  pubdate.replace(tzinfo=None)
        except ValueError:
            pubdate = datetime.strptime(pubdate, "%a, %d %b %Y %H:%M:%S %z")

        newsStory = NewsStory(guid, title, description, link, pubdate)
        ret.append(newsStory)
    return ret

#======================
# Data structure design
#======================

# Problem 1

#NewsStory
class NewsStory(object):
    def __init__(self, guid, title, description, link, pubdate):
        self.guid = guid #string
        self.title = title #string
        self.description = description #string
        self.link = link #string
        self.pubdate = pubdate #datetime

    def get_guid(self):
        return self.guid

    def get_title(self):
        return self.title

    def get_description(self):
        return self.description

    def get_link(self):
        return self.link

    def get_pubdate(self):
        return self.pubdate


#======================
# Triggers
#======================

class Trigger(object):
    def evaluate(self, story):
        """
        Returns True if an alert should be generated
        for the given news item, or False otherwise.
        """
        # DO NOT CHANGE THIS!
        raise NotImplementedError

# PHRASE TRIGGERS

# Problem 2
# PhraseTrigger
class PhraseTrigger(Trigger):
    def __init__(self, phrase):
        self.phrase = phrase.lower()

    def is_phrase_in(self, text):
        punc = string.punctuation
        text_low = text.lower()
        L = len(self.phrase)
        
        for i in text_low:
            if i in punc:
                text_low = text_low.replace(i, ' ')
        
        text_low_single_space = ' '.join(text_low.split())
        LT = len(text_low_single_space)
        temp_truth = self.phrase in text_low_single_space
        
        if temp_truth == True:
            first_index = text_low_single_space.find(self.phrase)
            last_index = first_index + L - 1
            
            if last_index != LT-1 and text_low_single_space[last_index + 1] != ' ':
                return False
            if first_index != 0 and text_low_single_space[first_index - 1]!=' ':
                return False
            
        return temp_truth
        
# Problem 3
# TitleTrigger
class TitleTrigger(PhraseTrigger):
    def __init__(self, phrase):
        PhraseTrigger.__init__(self,phrase)
        self.phrase = phrase.lower()

    def evaluate(self, story):
        title = story.get_title()
        return self.is_phrase_in(title)
    
# Problem 4
#DescriptionTrigger
class DescriptionTrigger(PhraseTrigger):
    def __init__(self, phrase):
        PhraseTrigger.__init__(self,phrase)
        self.phrase = phrase.lower()

    def evaluate(self, story):
        desc = story.get_description()
        return self.is_phrase_in(desc)

# TIME TRIGGERS

# Problem 5
# TimeTrigger
class TimeTrigger(Trigger):
    # Constructor:
    #        Input: Time has to be in EST and in the format of "%d %b %Y %H:%M:%S".
    #        Convert time from string to a datetime before saving it as an attribute.
    def __init__(self, EST):
        self.date_time = datetime.strptime(EST, '%d %b %Y %H:%M:%S')

# Problem 6
# BeforeTrigger and AfterTrigger
class BeforeTrigger(TimeTrigger):
    def __init__(self, EST):
        TimeTrigger.__init__(self, EST)
        self.date_time = datetime.strptime(EST, '%d %b %Y %H:%M:%S')

    def evaluate(self, story):
        story_time = story.get_pubdate().replace(tzinfo=None)
        return story_time < self.date_time

class AfterTrigger(TimeTrigger):
    def __init__(self, EST):
        TimeTrigger.__init__(self, EST)
        self.date_time = datetime.strptime(EST, '%d %b %Y %H:%M:%S')

    def evaluate(self, story):
        story_time = story.get_pubdate().replace(tzinfo=None)
        return story_time > self.date_time

# COMPOSITE TRIGGERS

# Problem 7
# NotTrigger
class NotTrigger(Trigger):
    def __init__(self, T):
        self.T = T

    def evaluate(self, story):
        T_result = self.T.evaluate(story)
        return not T_result

# Problem 8
# AndTrigger
class AndTrigger(Trigger):
    def __init__(self, T1, T2):
        self.T1 = T1
        self.T2 = T2

    def evaluate(self, story):
        T1_result = self.T1.evaluate(story)
        T2_result = self.T2.evaluate(story)
        return T1_result and T2_result

# Problem 9
# OrTrigger
class OrTrigger(Trigger):
    def __init__(self, T1, T2):
        self.T1 = T1
        self.T2 = T2

    def evaluate(self, story):
        T1_result = self.T1.evaluate(story)
        T2_result = self.T2.evaluate(story)
        return T1_result or T2_result

#======================
# Filtering
#======================

# Problem 10
def filter_stories(stories, triggerlist):
    """
    Takes in a list of NewsStory instances.

    Returns: a list of only the stories for which a trigger in triggerlist fires.
    """
    trig_stories = []

    for s in stories:
        for t in triggerlist:
            if t.evaluate(s) == True:
                trig_stories.append(s)
                break

    return trig_stories


#======================
# User-Specified Triggers
#======================
# Problem 11
def read_trigger_config(filename):
    """
    filename: the name of a trigger configuration file

    Returns: a list of trigger objects specified by the trigger configuration
        file.
    """
    # We give you the code to read in the file and eliminate blank lines and
    # comments. You don't need to know how it works for now!
    trigger_file = open(filename, 'r')
    lines = []
    for line in trigger_file:
        line = line.rstrip()
        if not (len(line) == 0 or line.startswith('//')):
            lines.append(line)

    # Problem 11
    # line is the list of lines that you need to parse and for which you need
    # to build triggers
    trigger_dict = {}
    trigger_list = []
    for l in lines:
        l_list = l.split(",")
        
        if l_list[0] == "ADD":
            for t in l_list[1:]:
                trigger_list.append(trigger_dict[t])
        
        if l_list[1] == "TITLE":
            trigger_dict[l_list[0]] = TitleTrigger(l_list[2])
        if l_list[1] == "DESCRIPTION":
            trigger_dict[l_list[0]] = DescriptionTrigger(l_list[2])
        if l_list[1] == "AFTER":
            trigger_dict[l_list[0]] = AfterTrigger(l_list[2])
        if l_list[1] == "BEFORE":
            trigger_dict[l_list[0]] = BeforeTrigger(l_list[2])
            
        if l_list[1] == "NOT":
            trigger_dict[l_list[0]] = NotTrigger(trigger_dict[l_list[2]])
        if l_list[1] == "AND":
            trigger_dict[l_list[0]] = AndTrigger(trigger_dict[l_list[2]], trigger_dict[l_list[3]])
        if l_list[1] == "OR":
            trigger_dict[l_list[0]] = OrTrigger(trigger_dict[l_list[2]], trigger_dict[l_list[3]])

    return trigger_list
        

SLEEPTIME = 120 #seconds -- how often we poll

def main_thread(master):
    # A sample trigger list - you might need to change the phrases to correspond
    # to what is currently in the news
    try:
        t1 = TitleTrigger("New York")
        t2 = DescriptionTrigger("terrorist")
        t3 = DescriptionTrigger("Clinton")
        t4 = AndTrigger(t2, t3)
        triggerlist = [t1, t4]

        # Problem 11
        # TODO: After implementing read_trigger_config, uncomment this line 
        triggerlist = read_trigger_config('triggers.txt')
        
        # HELPER CODE - you don't need to understand this!
        # Draws the popup window that displays the filtered stories
        # Retrieves and filters the stories from the RSS feeds
        frame = Frame(master)
        frame.pack(side=BOTTOM)
        scrollbar = Scrollbar(master)
        scrollbar.pack(side=RIGHT,fill=Y)

        t = "Google & Yahoo Top News"
        title = StringVar()
        title.set(t)
        ttl = Label(master, textvariable=title, font=("Helvetica", 18))
        ttl.pack(side=TOP)
        cont = Text(master, font=("Helvetica",14), yscrollcommand=scrollbar.set)
        cont.pack(side=BOTTOM)
        cont.tag_config("title", justify='center')
        button = Button(frame, text="Exit", command=root.destroy)
        button.pack(side=BOTTOM)
        guidShown = []
        def get_cont(newstory):
            if newstory.get_guid() not in guidShown:
                cont.insert(END, newstory.get_title()+"\n", "title")
                cont.insert(END, "\n---------------------------------------------------------------\n", "title")
                cont.insert(END, newstory.get_description())
                cont.insert(END, "\n*********************************************************************\n", "title")
                guidShown.append(newstory.get_guid())

        while True:

            print("Polling . . .", end=' ')
            # Get stories from Google's Top Stories RSS news feed
            stories = process("http://news.google.com/news?output=rss")

            # Get stories from Yahoo's Top Stories RSS news feed
            stories.extend(process("http://news.yahoo.com/rss/topstories"))

            stories = filter_stories(stories, triggerlist)

            list(map(get_cont, stories))
            scrollbar.config(command=cont.yview)


            print("Sleeping...")
            time.sleep(SLEEPTIME)

    except Exception as e:
        print(e)


if __name__ == '__main__':
    root = Tk()
    root.title("Some RSS parser")
    t = threading.Thread(target=main_thread, args=(root,))
    t.start()
    root.mainloop()

