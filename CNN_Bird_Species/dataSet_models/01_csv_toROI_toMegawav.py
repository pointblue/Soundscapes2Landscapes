# read_csv.py

# read csv 

import pandas as pd
import numpy as np
import csv
import joblib
import os 
import joblib
import numpy as np
import csv
import os
import json
import librosa
import IPython.display as ipd
import numpy as np
from matplotlib import pyplot as plt
import scipy.io.wavfile as wav
from numpy.lib import stride_tricks
from sklearn.preprocessing import normalize
import joblib
import pandas as pd
from itertools import chain


""" short time fourier transform of audio signal """
def stft(sig, frameSize, overlapFac=0.9, window=np.hanning): # was 0.5
    win = window(frameSize)
    hopSize = int(frameSize - np.floor(overlapFac * frameSize))

    # zeros at beginning (thus center of 1st window should be for sample nr. 0)   
    samples = np.append(np.zeros(int(np.floor(frameSize/2.0))), sig)    
    # cols for windowing
    cols = np.ceil( (len(samples) - frameSize) / float(hopSize)) + 1
    # zeros at end (thus samples can be fully covered by frames)
    samples = np.append(samples, np.zeros(frameSize))

    frames = stride_tricks.as_strided(samples, shape=(int(cols), frameSize), strides=(samples.strides[0]*hopSize, samples.strides[0])).copy()
    frames *= win

    return np.fft.rfft(frames)    

""" scale frequency axis logarithmically """    
def logscale_spec(spec, sr=22050, factor=20.):
    timebins, freqbins = np.shape(spec)

    scale = np.linspace(0, 1, freqbins) ** factor
    scale *= (freqbins-1)/max(scale)
    scale = np.unique(np.round(scale))

    # create spectrogram with new freq bins
    newspec = np.complex128(np.zeros([timebins, len(scale)]))
    for i in range(0, len(scale)):        
        if i == len(scale)-1:
            newspec[:,i] = np.sum(spec[:,int(scale[i]):], axis=1)
        else:        
            newspec[:,i] = np.sum(spec[:,int(scale[i]):int(scale[i+1])], axis=1)

    # list center freq of bins
    allfreqs = np.abs(np.fft.fftfreq(freqbins*2, 1./sr)[:freqbins+1])
    freqs = []
    for i in range(0, len(scale)):
        if i == len(scale)-1:
            freqs += [np.mean(allfreqs[int(scale[i]):])]
        else:
            freqs += [np.mean(allfreqs[int(scale[i]):int(scale[i+1])])]

    return newspec, freqs

""" plot spectrogram"""
def plotstft(binsize=2**10, plotpath=None, colormap="jet", srr = 22050, sam = None):
    samplerate, samples = srr, sam

    s = stft(samples, binsize)

    sshow, freq = logscale_spec(s, factor=1.0, sr=samplerate)

    ims = 20.*np.log10(np.abs(sshow)/10e-6) # amplitude to decibel

    timebins, freqbins = np.shape(ims)

    print("timebins: ", timebins)
    print("freqbins: ", freqbins)

    plt.figure(figsize=(15, 7.5))
    plt.imshow(np.transpose(ims), origin="lower", aspect="auto", cmap=colormap, interpolation="none")
    plt.colorbar()

    plt.xlabel("time (s)")
    plt.ylabel("frequency (hz)")
    plt.xlim([0, timebins-1])
    plt.ylim([0, freqbins])

    
    xlocs = np.float32(np.linspace(0, timebins-1, 5))
    plt.xticks(xlocs, ["%.02f" % l for l in ((xlocs*len(samples)/timebins)+(0.5*binsize))/samplerate])
    ylocs = np.int16(np.round(np.linspace(0, freqbins-1, 10)))
    plt.yticks(ylocs, ["%.02f" % freq[i] for i in ylocs])

    if plotpath:
        plt.savefig(plotpath, bbox_inches="tight")
    else:
        plt.show()

    plt.clf()

    return ims

csv_path = 'csv/1June_pattern_matching_ROIs_200601.csv'

pd.read_csv(csv_path,index_col=None)

import csv
import librosa
import matplotlib.pyplot as plt
import IPython.display as ipd

sp_names = []
fir_app = "birdcode"

with open(csv_path,'rt')as f:
    data = csv.reader(f)
    for row in data:
        #print(row[10])
        if(row[10] != fir_app):
            sp_names.append(row[10])
            fir_app = row[10]

#print(sp_names)

# becasue there were duplicates!!
mylist = list(dict.fromkeys(sp_names))
print(mylist)
print(len(mylist))
sp_names = mylist

roi_list = []
for i in range(len(sp_names)):
    roi_cnt = 0
    bird = sp_names[i]
    highest_dur = 0
    dur =0 
    with open(csv_path,'rt')as f:
        data = csv.reader(f)
        for row in data:
            #print(row[10])
            if(row[10]==bird and row[18] =='present' and row[0] != 'NA'):
                #print(row[18])
                roi_cnt = roi_cnt+1
                dur = dur + (np.float16(row[14])-np.float16(row[13]))
                if ((np.float16(row[14]) - np.float16(row[13])) > highest_dur):
                    highest_dur = np.float16(row[14]) - np.float16(row[13])
    if(roi_cnt!=0)            :
        print(roi_cnt,'ROIs for',sp_names[i])
        print("Avg. duration: dur:",dur/roi_cnt)
        print("Highest duration:",highest_dur)
        print('-'*50)
        roi_list.append([roi_cnt,sp_names[i]])
		

(roi_list)
roi_sum = 0
indx = 0
total_roi_w_cutoff = 0
bird_name_lst = []
cutoff_sp_name = []
cutoff = 200# th nuo. or ROIs should at least be these many


for i in range(len(roi_list)):
    roi_sum = roi_sum + roi_list[i][0]
    
    if(roi_list[i][0] >=cutoff):
        indx+=1
        print(indx, roi_list[i])
        cutoff_sp_name.append(roi_list[i])
        total_roi_w_cutoff += roi_list[i][0]
        bird_name_lst.append(roi_list[i])
print("ROI_SUM with cutoff",total_roi_w_cutoff)
print("ROI_SUM for all birds",roi_sum)

joblib.dump(cutoff_sp_name, 'handy_pkls/cutoff_sp_name.pkl')
print("Dumped")


# Pad with surr birds    
def pad_rois_w_noise(sr = 22050, target_time = 2.00, roi_npy_path = None):
    
    desired_sams = target_time * sr
    print("Desired Sams:",desired_sams)
    jl = joblib.load(roi_npy_path)
    print(np.shape(jl))

    roi_noise_pad = []
    lab_lst = []
    for i in range(len(jl)):
        if(i%100 == 0):
            print("Done with, ",i)

        no_segments = (np.shape(jl[i][0])[0])/(target_time * sr)
        #print("No. of Segments: ",no_segments)
        #print("Shape of the X:", np.shape(jl[i][0]))

        if (no_segments >= 1):
            roi_noise_pad.append(jl[i][0])

        else:
            x, sr = librosa.load(jl[i][1], sr= sr) # librosa loading whole one min clip is a killer

            _5_sec_sam = int(sr*target_time) # the roi should not start before this
            _60_sec_sam = sr*60
            last_5_sec = _60_sec_sam - _5_sec_sam # the roi should not FINISH after this

            #print("Len of samples:",len(jl[i][0]))
            #print("Diff between samples:",(jl[i][3]) - jl[i][2])

            if(jl[i][2] > _5_sec_sam and jl[i][3] < last_5_sec):  # regular scenario
                len_roi = len(jl[i][0][:int(sr*target_time)-1])
                play = _5_sec_sam - len_roi
                #print("The roi can  displace these maany samples:",play)
                #print(">>>--1111--->>>>>>>In first scenario...")
                st_roi = jl[i][2] - np.random.randint(play)
                end_roi = st_roi + _5_sec_sam

            elif(jl[i][2] < _5_sec_sam):
                #print(">>>--222222--->>>>>>>In second scenario...")
                st_roi = jl[i][2]
                end_roi = st_roi + _5_sec_sam

            elif(jl[i][3] > last_5_sec):
                #print(">>>--333333--->>>>>>>In Third scenario...")
                end_roi = jl[i][3]
                st_roi = end_roi - _5_sec_sam

            #print('Print the x[st:end]:',len(x[st_roi:end_roi]))
            roi_noise_pad.append(x[st_roi:end_roi])
            #print(np.shape(x[st_roi:end_roi]))
    return roi_noise_pad

# CHAIN AND CONVERT TO WAVs

wav_dir = '/media/third/2020/June_1/wav_6June/'

for i in range(len(cutoff_sp_name)):
    print('>>>',cutoff_sp_name[i],'<<<')
    roi_npy_path = os.path.join('/media/first/June_s2l/June_01_37_class/ext_pkl/',cutoff_sp_name[i][1] + '_1June2020.pkl')
    
    roi_lst_ = pad_rois_w_noise(sr = 22050, target_time = 2.00, roi_npy_path = roi_npy_path)
    print("DONE..padding!")
    print(np.shape(roi_lst_))
    
    newlist = list(chain(*roi_lst_))
    print(len(newlist))
    wav_pth = os.path.join(wav_dir,cutoff_sp_name[i][1]+'_2000' + '.wav')
    
    librosa.output.write_wav(wav_pth, np.array(newlist), sr  = 22050)
    
    print('a .wav file for',cutoff_sp_name[i][1],'saved at:',wav_pth)		