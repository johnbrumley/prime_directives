from gtts import gTTS
import pygame as pg
import sys
import os
import time
import random
import subprocess

import logging
from systemd.journal import JournalHandler

log = logging.getLogger(__name__)
log.addHandler(JournalHandler())
log.setLevel(logging.INFO)

# import side file things? not sure if this will work as a sysctrl
import amzntest as amzn

# twitter things
twitter = True
import tweepy
import credentials

#sounds to be loaded, sorry globals
alexaSound = None
simonSound = None

def playSoundFile(fname, sounds_for_alexa=True):
    # log.info('playing sound')
    
    if sounds_for_alexa:
        # wake and simon says
        alexaDelay = 500
        alexaSound.play()
        pg.time.wait(int(alexaSound.get_length()*1000 + alexaDelay))
        simonSound.play()
        pg.time.wait(int(simonSound.get_length()*1000) + 400)

    # play the generated text
    startTime = time.time() # store time

    pg.mixer.music.load(fname)
    pg.mixer.music.play()
    while pg.mixer.music.get_busy(): 
        pg.time.Clock().tick(10)
    
    waitTime = int(round(time.time() - startTime)) + 3
    if waitTime > 30:
        waitTime = 4
    # wait for a little while (give alexa a chance to talk )
    # log.info('waiting for %d seconds', waitTime)
    time.sleep(waitTime)

def generateSpeech(text, fileName):
    # englishVariants = ['en-in','en-ca','en','en-us','en-gb','en-ie','en-tz','en-uk','en-ng','en-gb','en-nz','en-au','en-za','en-gh','en-ph']
    # lang = random.choice(englishVariants)
    lang = 'en-us'
    # log.info('getting %s tts for %s', lang, text)
    try:
        tts = gTTS(text,lang,True)
    except AssertionError:
        print("GTTS: text is None or empty; when there’s nothing left to speak after pre-precessing, tokenizing and cleaning.")
    except ValueError:
        print("GTTS: lang_check is True and lang is not supported.")
    except RuntimeError:
        print("GTTS: lang_check is True but there’s an error loading the languages dictionnary")

    try:
        tts.save(fileName)
    except gTTSError as e:
        print("GTTS: error while trying to save file")
        print(e)

def chopTweetToLength(tweet, maxLength):
    tweetList = []
    if len(tweet) > maxLength:
        c = tweet[maxLength]
        newBreak = maxLength
        while not c.isspace():
            newBreak -= 1
            c = tweet[newBreak]

        tweetList.append(tweet[:newBreak])
        # split the remaining part of the tweet
        tweetList.extend(chopTweetToLength(tweet[newBreak:], maxLength))
        return tweetList
    else:
        tweetList.append(tweet)
        return tweetList

def tweetStory(api, story):
    # try to tweet the story
    singleStringStory = '\n'.join(story)
    tweets = chopTweetToLength(singleStringStory, 280)
    
    # note that the story shows in reverse order on the feed, but correctly in the thread
    # which one is the right one to use?
    try:
        status = None
        for tweet in tweets:
            if status is None:
                status = api.update_status(tweet)
            else: 
                status = api.update_status(tweet, in_reply_to_status_id = status.id)
    except tweepy.error.TweepError as e:
        print(e)
    except RateLimitError:
        print("TWITTER: hit the rate limit")

def loadStory(fname, line_length):
    with open(fname, errors='ignore') as f:
        content = f.readlines()
    # remove whitespace characters like `\n` at the end of each line
    content = [x.strip() for x in content]
    # fix doubled names 
    content = [x.replace("ALEXAALEXA", "ALEXA") for x in content]
    content = [x.replace("JEFFJEFF", "JEFF") for x in content]
    # make lower
    content = [x.lower() for x in content]
    # pull out the dashes
    storyLines = [x.replace("-", " ") for x in content]
    # if line is too long, split and insert in next position
    maxLength = line_length
    breakpoint = int(maxLength/2)
    for i, s in enumerate(storyLines):
        if len(s) > maxLength:
            # break it at a whitespace
            c = s[breakpoint:breakpoint+1]
            newBreak = breakpoint
            while not c.isspace():
                newBreak += 1
                c = s[newBreak:newBreak+1]
            # chop current line and insert second half
            storyLines[i] = s[:newBreak]
            storyLines.insert(i+1, s[newBreak:])

    return storyLines

def generateStory(story_number, timeout):
    # generate story (stored in 'story.txt')
    # ./ccl/armcl -l micro-talespin-amzn.lisp -e "(micro-talespin-demo *story3*)" -e "(quit)"
    storyString = "(micro-talespin-demo *s" + story_number + "*)"
    try:
        subprocess.run(["./ccl/armcl","-l","mt-amzn-items.lisp","-e",storyString,"-e","(quit)"], timeout=timeout)
    except subprocess.TimeoutExpired:
        log.info("Lisp timed out, story got stuck!")

def setupAudio(usb):
    if usb:
        # change to USB sound
        os.putenv('AUDIODEV','plughw:CARD=Set,DEV=0') 

    # init pygame mixer
    pg.mixer.pre_init(24000, -16, 1, 2048)
    pg.mixer.init()

    global alexaSound, simonSound
    alexaSound = pg.mixer.Sound('alexa.wav')
    simonSound = pg.mixer.Sound('simon.wav')

def main():
    # setup output, 
    # True == USB soundcard, 
    # False == default out (bluetooth) 
    setupAudio(True)

    api = None
    if twitter:
        # init twitter
        tw = credentials.twitter
        auth = tweepy.OAuthHandler(tw['consumer_key'], tw['consumer_secret'])
        auth.set_access_token(tw['access_token'], tw['access_secret'])
        api = tweepy.API(auth)

    while True:
        # load and trim story
        story = loadStory("story.txt", 90)
        sliceIndex = [i for i, s in enumerate(story) if 'one day' in s]
        # log.info(sliceIndex)
        story = story[sliceIndex[0]:]

        # Text Output
        if twitter and api is not None:
            # combines and forms story into a series of tweets
            tweetStory(api, story)

        # Voice Output

        for line in story:
            generateSpeech(line, "gtts.mp3")
            playSoundFile("gtts.mp3", True)

        # Story Generation

        # generate story
        storyNum = str(random.randrange(1,8))
        generateStory(storyNum, timeout=5)
        # query new items (stored in 'new-items.txt')
        amzn.queryAmazon()


        # Wait for a bit before doing another story
        time.sleep(10)
        
if __name__ == "__main__":
    main()
