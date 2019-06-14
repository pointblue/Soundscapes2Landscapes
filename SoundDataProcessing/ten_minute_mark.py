"""
This script extracts files of recordings made at every 10 minute mark.
Author: Sara Schmunk
"""

import os
from shutil import copy2


def main():

    from_dir = input('Directory to search: ')
    os.mkdir(from_dir + '_extracted')
    to_dir = from_dir + '_extracted'
    print('Files will be saved here:', to_dir)
    original_dir = os.listdir(from_dir)

    for file in original_dir:
        absolute_path = from_dir + '\\' + file
        filename = file[:-4]
        minutes = filename[-2:]
        if int(minutes) % 10 == 0 or minutes == 00:
            copy2(absolute_path, to_dir)
        else:
            continue


main()
