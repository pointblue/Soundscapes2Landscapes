# Generate augmented samples

import os
import matplotlib
matplotlib.use('Agg') # No pictures displayed 
import pylab
import librosa
import librosa.display
import numpy as np
import matplotlib.pyplot as plt
import IPython.display as ipd
import joblib
import cv2

%matplotlib inline

mel_spec_dir = '/media/third/2020/mel_meth/birds_37/2_sec_work/raw_mel_specs/tr/'
pkl_dir = '/media/third/2020/mel_meth/birds_37/2_sec_work/raw_pkl/tr/'

print('No of folders here, in mel_spec_dir:',os.listdir(mel_spec_dir))

cutoff_sp_name = joblib.load('../../data_management/handy_pkls/cutoff_sp_name.pkl')
print(len(cutoff_sp_name))

roi_len = 2.000 # IMPORTANT !
roi_len_str = 'tr' # training set!

print('Generating ROI PNG images with',roi_len_str)

N_MELS = 128
HOP_SIZE = 128
N_FFT = 1024+512  # Number of frequency bins for Fast Fourier Transform
FMIN = 0 #250 # 1000
SR = 22050

wav_dir = '/media/third/2020/mel_meth/birds_37/split_wav_2000/'

n_sam = int(22050 * roi_len)  #21168

for wav_nm in (os.listdir(wav_dir)):

    cnt = 0

    if roi_len_str in wav_nm:
        print(wav_nm)
        
        y, sr = librosa.load(os.path.join(wav_dir, wav_nm), sr = 22050)
        
        # data aug from here
        jl = y
        no_sam = np.shape(jl)[0]

        if (no_sam%2 != 0): #if odd
            no_sam = no_sam -1  # make it even

        jl = jl[:no_sam]
        first_half = np.asarray(jl[:int(no_sam/2)])
        second_half = np.asarray(jl[int(no_sam/2):])
        print('Shape of full clip:', np.shape(jl))
        print('Shape of first half:', np.shape(first_half))
        print('Shape of second half:', np.shape(second_half))

        aug_sam = first_half + second_half

        print("New data augmented samples:",np.shape(aug_sam))
        # data aug ends here
        
        tot_sam = int(np.shape(aug_sam)[0]/n_sam)
        print('Total number of possible ROIs:', tot_sam)
        
        for n in range(tot_sam):
            
            y_21k = aug_sam[n*n_sam:(n+1)*n_sam]
            pkl_directory = os.path.join(pkl_dir, wav_nm.split('_')[0] +'/' + (wav_nm.split('.')[0] + '_pkl_' +str(cnt) +'aug.pkl'))
            
            #print('Converting these samples to mel-spec:', np.shape(y_21k))
            joblib.dump(y_21k, pkl_directory)
            
            
            fig = plt.figure(1, frameon=False)
            fig.set_size_inches(6, 6)
            ax = plt.Axes(fig, [0., 0., 1., 1.])
            ax.set_axis_off()
            fig.add_axes(ax)

            S = librosa.feature.melspectrogram(y=y_21k, sr=22050, n_mels=128,fmin = 0,
                                     fmax=11025, n_fft=728, hop_length=32, win_length = None, htk = True) # hop length lesser the better
            
            
            librosa.display.specshow(librosa.power_to_db(S ** 1.5, ref=np.max), fmin=FMIN, y_axis='linear')#, cmap = 'gray')
            
            directory = os.path.join(mel_spec_dir, wav_nm.split('_')[0] +'/' + (wav_nm.split('.')[0] + '_' +str(cnt) +'aug.png'))  # 'test.png'
            #print(directory)
            fig.savefig(directory)
            #plt.show()
            fig.clear()
            ax.cla()
            plt.clf()
            plt.close('all')
            
            cnt +=1
            
        print('Number of ROIs converted here:', cnt)
        print('-'*70)





