#!/scratch/cq73/.conda/envs/mfcc/bin/ python3.7
# coding: utf-8

# import libraries
import matplotlib
matplotlib.use('Agg') 
matplotlib.rcParams['figure.dpi'] = 72 

import matplotlib.pyplot as plt
import numpy as np
import librosa
import IPython.display as ipd
import librosa.display
import os
import joblib
import pandas as pd

import tensorflow as tf
import IPython.display as display
from PIL import Image
import pathlib
print("libraries loaded...")

import sys
import glob

# retrieve slurm commnads
slurm_id=int(sys.argv[1])
chunk_size=int(sys.argv[2])
todo_csv=sys.argv[3]
out_dir=sys.argv[4]
audio_dir=sys.argv[5]
wav_ext=sys.argv[6]

print("Slurm ID =", slurm_id)
print("Chunk size =", chunk_size)
print("Data file = ", todo_csv)
print("Reuslts dir = ", out_dir)
print('Audio dir = ', audio_dir)

# start and stop files based on slurm ID and chunk size from bash script
start_index = (slurm_id - 1) * chunk_size + 1
end_index = (slurm_id * chunk_size)
print("Start pos = ",start_index)
print("End pos =",end_index)

# input dirs
# input melspecs, dependent on slurm array ID
todo_csv = pd.read_csv(todo_csv, names = ['wavs'])
audio_files = todo_csv.loc[start_index:end_index]['wavs'].tolist()

print("\nLoaded audio files...")
print("Length of audio file list:", len(audio_files))
print("First file = ",start_index,":",audio_files[0])
print("Last file = ",len(audio_files),":",audio_files[-1])

# -----------------------
# FUNCTION 1
# define a function to clip a minute long clip
# output should a array of shape: (n_clips, sr)
def clip_wav(wav_path = None, clip_duration = None, sample_rate= 22050, offset = None):
    
    x, sr = librosa.load(wav_path, sr = sample_rate)
    print(np.shape(x))
    no_clips = int(np.shape(x)[0] / (clip_duration*sample_rate))
    print('The number of clips here are:', no_clips, "; including offsets =", no_clips * 2 - 1)
    
    if(offset == None):
        x = x[:int(no_clips*clip_duration * sample_rate)]
    else:
        x = x[offset:int(no_clips*clip_duration*sample_rate)]
        no_clips = int(np.shape(x)[0] / (clip_duration*sample_rate))
        x = x[:int(no_clips*clip_duration * sample_rate)]
        
    #print(np.shape(x))
    no_clips = int(np.shape(x)[0] / (clip_duration*sample_rate))
    #print('The number of clips here are:', no_clips)
    
    o_p = np.reshape(x, (no_clips, int(clip_duration * sample_rate)))
    
    print('Final Shape:', np.shape(o_p))
    
    return o_p


# FUNCTION 2
def create_melSpecs(audio_samples = None, FMIN = 0, FMAX = 11025, HOP_SIZE = 128, N_MELS = 128, N_FFT = 1024+512, 
                    sample_rate = 22050, save_pth = None, mel_fname = None, offset = False):

    SR = sample_rate
    
    for i in range(len(audio_samples)): 
        
        y_21k = audio_samples[i]
    
        fig = plt.figure(1, frameon=False)
        fig.set_size_inches(6, 6)
        ax = plt.Axes(fig, [0., 0., 1., 1.])
        ax.set_axis_off()
        fig.add_axes(ax)

  
        S = librosa.feature.melspectrogram(y=y_21k, sr=22050, n_mels=128,fmin = 0,
                                     fmax=11025, n_fft=728, hop_length=32, win_length = None, htk = True) # hop length lesser the better

        librosa.display.specshow(librosa.power_to_db(S ** 1, ref=np.max), fmin=0, y_axis='linear')# , cmap = 'gray')
        
        if offset == True:
          directory = os.path.join(save_pth, mel_fname+str(i*2+1)+'.png')
        else:
          directory = os.path.join(save_pth, mel_fname+str((i)*2)+'.png')
        #print(directory)
        fig.savefig(directory)
        #plt.show()
        fig.clear()
        ax.cla()
        plt.clf()
        plt.close('all')

    print('No. of melSpecs created:', i+1)


# FUNCTION 3
def decode_img(img, IMG_HEIGHT, IMG_WIDTH):
  # convert the compressed string to a 3D uint8 tensor
  img = tf.image.decode_jpeg(img, channels=3)
  # Use `convert_image_dtype` to convert to floats in the [0,1] range.
  img = tf.image.convert_image_dtype(img, tf.float32)
  # resize the image to the desired size.
  return tf.image.resize(img, [IMG_HEIGHT, IMG_WIDTH])

# FUNCITON 4
def process_path(file_path, IMG_HEIGHT, IMG_WIDTH):
    # load the raw data from the file as a string
    img = tf.io.read_file(file_path)
    img = decode_img(img, IMG_HEIGHT, IMG_WIDTH)
    return img #, label

       
       
# PROCESSING: MELSPEC GENERATION
# TRY AND SAVE THE PNG FOLDER AND THE CORRESPONDING LABEL HERE
excep = 0

for no in range(len(audio_files)):
    
    w_pth = audio_files[no]
    dir_nm = os.path.basename(w_pth) # remove .wav or .WAV extension
    mel_store = os.path.join(out_dir, dir_nm)
    w_pth = os.path.join(audio_dir, w_pth + wav_ext)
    
    print('The dir name/mel_store here will be:', mel_store)
    print('wav file = ', w_pth)
    
    try:
        os.mkdir(mel_store)
    
        # non-offset/ even seconds (e.g. 0,2,...58) toggled using offset command in clip_wav() and create_melSpecs
        clipped_wav = clip_wav(wav_path = w_pth, clip_duration = 2.000, sample_rate= 22050, offset = None)
        fmin = 0 
        create_melSpecs(audio_samples = clipped_wav, FMIN = fmin, FMAX = 11025, HOP_SIZE = 128, N_MELS = 128, N_FFT = 1024+512, 
                            sample_rate = 22050, save_pth = mel_store, mel_fname = "", offset = False)
                            
        # offset/ odd seconds (e.g. 1,3,...57) toggled using offset command in clip_wav() and create_melSpecs
        clipped_wav = clip_wav(wav_path = w_pth, clip_duration = 2.000, sample_rate= 22050, offset = 22050)
        fmin = 0 
        create_melSpecs(audio_samples = clipped_wav, FMIN = fmin, FMAX = 11025, HOP_SIZE = 128, N_MELS = 128, N_FFT = 1024+512, 
                            sample_rate = 22050, save_pth = mel_store, mel_fname = "", offset = True)

    except:
        print(mel_store,'Already exists!')
        
        # check if there are 59 images (30 + 29)
        mel_files = glob.glob(mel_store + "/*.png")
        print("There are ",len(mel_files), "melspecs in the directory")
        
        # if there are not 59 images, build them
        if len(mel_files) != 59 :
          print("calculating melspecs")
          # non-offset/ even seconds (e.g. 0,2,...58)
          clipped_wav = clip_wav(wav_path = w_pth, clip_duration = 2.000, sample_rate= 22050, offset = None)
          fmin = 0 #250
          create_melSpecs(audio_samples = clipped_wav, FMIN = fmin, FMAX = 11025, HOP_SIZE = 128, N_MELS = 128, N_FFT = 1024+512, 
                              sample_rate = 22050, save_pth = mel_store, mel_fname = "", offset = False)
                              
          # offset/ odd seconds (e.g. 1,3,...57)
          clipped_wav = clip_wav(wav_path = w_pth, clip_duration = 2.000, sample_rate= 22050, offset = 22050)
          fmin = 0 #250
          create_melSpecs(audio_samples = clipped_wav, FMIN = fmin, FMAX = 11025, HOP_SIZE = 128, N_MELS = 128, N_FFT = 1024+512, 
                              sample_rate = 22050, save_pth = mel_store, mel_fname = "", offset = True)
        else:
          print("The directory is up to date")
        
        excep += 1
        print('Excep no:', excep)

    print('-'*100, no)
