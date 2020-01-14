import matplotlib.image as mpimg
import matplotlib.pyplot as plt
import numpy as np
import copy
import argparse
from math import fabs

RED = 0
GREEN = 1
BLUE = 2
GREY_LEVEL_HAPPY = 10
GREY_LEVEL_SAD = 0

def happy_changes(startup_img):
    for x in startup_img:
        for y in x:
            red_green = fabs(int(y[RED]) - int(y[GREEN]))
            red_blue = fabs(int(int(y[RED]) )- int(y[BLUE]))
            blue_green = fabs(int(int(y[BLUE])) - int(y[GREEN]))
            grey = (red_green + red_blue + blue_green) / 3
            if (grey < GREY_LEVEL_HAPPY):
                sort = sorted(range(len(y)), key=lambda k: y[k])
                add = 100 * y[sort[0]] / 255
                if add + y[sort[0]] > 255:
                    y[sort[0]] = 255
                else:
                    y[sort[0]] += add
                sub = 100 * y[sort[2]] / 255
                if  y[sort[2]] - sub < 0:
                    y[sort[2]] = 0
                else:
                    y[sort[2]] -= sub

def sad_changes(startup_img):
    for x in startup_img:
        for y in x:
            red_green = fabs(int(y[RED]) - int(y[GREEN]))
            red_blue = fabs(int(int(y[RED]) )- int(y[BLUE]))
            blue_green = fabs(int(int(y[BLUE])) - int(y[GREEN]))
            grey = (red_green + red_blue + blue_green) / 3
            if (grey > GREY_LEVEL_SAD):
                sort = sorted(range(len(y)), key=lambda k: y[k])
                avg = (int(y[RED]) + int(y[BLUE]) + int(y[GREEN])) / 3
                add = 100 * y[sort[2]] / 255
                # add = add + y[sort[2]] 
                if add + y[sort[2]] > 255:
                    y[sort[2]] = 255
                else:
                    y[sort[2]] += add
                sub = 100 * y[sort[0]] / 255
                if  y[sort[0]] - sub < 0:
                    y[sort[0]] = 0
                else:
                    y[sort[0]] -= sub

ap = argparse.ArgumentParser()
ap.add_argument('-i', '--image', required=True, help='Path to image')
ap.add_argument('-m', '--mood', required=False, default='happy', help='Mood type for image modifiy. Options: [happy, sad]')
args = ap.parse_args()



c_img = mpimg.imread(args.image)
img = copy.copy(c_img)
if args.mood == 'happy':
    happy_changes(img)
elif args.mood == 'sad':
    sad_changes(img)

f, axarr = plt.subplots(2)
axarr[0].imshow(c_img, interpolation='kaiser')
axarr[1].imshow(img, interpolation='kaiser')
plt.show()