#!/scratch/cq73/.conda/envs/tfgpu/bin/ python3.7
# coding: utf-8

import matplotlib
matplotlib.use('Agg') # output plots on headless cluster
matplotlib.rcParams['figure.dpi'] = 72 #dpi manually set to match all S2L machines

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import IPython.display as ipd
import os
import joblib
import glob
import time
from datetime import date
import tensorflow as tf
import IPython.display as display
from PIL import Image
import pathlib
import sys

AUTOTUNE = tf.data.experimental.AUTOTUNE
tf.__version__

# --- USER ARGUMENTS ---
todo_csv = '' # the csv with pathless folder names which contain melspecs for incomplete recording. e.g.: recording1/, recordingN/; where each recordingN contains mel spectrograms
mel_storage = '' # where parent melspec directory is. Contains directories from todo_csv var.
out_dir = '' # where to save predictions
model_path = '' # where current CNN model is saved

print("Data [csv] dir = ", todo_csv)
print('Mel storage:', mel_storage)
print("Results dir = ", out_dir)
print("CNN checkpoint =",model_path)
print("Error logs will print to :", logs_path)

# input melspecs
mel_dirs = pd.read_csv(todo_csv, names = ['toDo'])

print("\nLoaded audio files...")
print("Length of audio file list:", len(mel_dirs))
print("First file = 0 :",mel_dirs[0])
print("Last file = ",len(mel_dirs),":",mel_dirs[-1])


# FUNCTIONS
# function that interpretes image after being provided a path in process_path
def decode_img(img, IMG_HEIGHT, IMG_WIDTH):
  # convert the compressed string to a 3D uint8 tensor
  img = tf.image.decode_jpeg(img, channels=3)
  # Use `convert_image_dtype` to convert to floats in the [0,1] range.
  img = tf.image.convert_image_dtype(img, tf.float32)
  # resize the image to the desired size.
  return tf.image.resize(img, [IMG_HEIGHT, IMG_WIDTH])

# function to read in a file path
def process_path(file_path, IMG_HEIGHT, IMG_WIDTH):
    # load the raw data from the file as a string
    img = tf.io.read_file(file_path)
    img = decode_img(img, IMG_HEIGHT, IMG_WIDTH)
    return img #, label


# Load in model (not checkpoint)
model = tf.keras.models.load_model(model_path)
IMG_HEIGHT = 224
IMG_WIDTH = 224

model.summary()

print('Number of folders in the parent Mel-Spec directory:',len(melspec_dirs))

start_time = time.time()

# iterate through each folder/wav file which contains mels
for i in range(len(mel_dirs)):
  try:
      temp_wav = os.path.splitext(mel_dirs[i])[0]
      out_path_temp = os.path.join(out_dir, 'sigmoid', temp_wav + '.pkl')
      print("Wav name:",temp_wav)
      
      # does the prediction exist already? Skip and move to next.
      if os.path.isfile(out_path_temp):
        print("WAV ALREADY PREDICTED") 
        continue
                          
      else:
        # if not, what is the mel spec directory          
        mel_store = os.path.join(mel_storage, "melspecs_pre2020", temp_wav)
        
        # mels are saved in a pre2020 or 2021 directory - check both
        if os.path.isdir(mel_store): 
          # pre2020
          print("Melspec dir:",mel_store)
        else: 
          # 2021
          mel_store = os.path.join(mel_storage, "melspecs_2020-2021", temp_wav)
          print("Melspec dir:",mel_store)
  
        mel_store_lst = os.listdir(mel_store) # temp dir with the PNGs
        loop_run = len(mel_store_lst)
        print("Number of melspecs in current dir =", loop_run)
        
        sigmoid_pred_lst = []
        mel_names = []
        # iterate through each melspec in current file directory
        for j in range(loop_run):
            fname = str(j)+'.png' # files are named 0-59 so simple loop works
            mel_names.append(fname)
            
            # read in png using process_path fx and tf
            img_ = process_path(os.path.join(mel_store, fname), IMG_HEIGHT = 224, IMG_WIDTH = 224)
            img_ = tf.reshape(img_, shape= (1, IMG_HEIGHT, IMG_WIDTH, 3))
  
            # get predictions
            pred = model.predict(img_, verbose=0, steps=1, callbacks=None, max_queue_size=10, workers=1, use_multiprocessing=False)
  
            # CNN does not have sigmoid - run pred through sigmoid final activation
            sigmoid_pred = tf.math.sigmoid(pred).numpy()           
            sigmoid_pred_lst.append(sigmoid_pred)
            
        # save predictions in a small pkl for each recording where row = melspec and col = bird spp 1:54 
        joblib.dump(sigmoid_pred_lst, out_path_temp)
        print('Saved this file at:', out_path_temp, "Length was:",len(sigmoid_pred_lst))
  
  except Exception as e:
      print(e)
      f = logs_path + temp_wav + '.txt'
      f = open(f, 'w')
      f.write('This wav file did not complete -%s' % e)
      f.close()
      
  print('-'*80)
  print()
print("--- %s seconds ---" % (time.time() - start_time))

today = date.today()
print("Today's date:", today)
