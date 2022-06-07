# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pickle
import joblib
import pandas
def read_pickle_file(file):
    pickle_data = joblib.load(file)
    return pickle_data