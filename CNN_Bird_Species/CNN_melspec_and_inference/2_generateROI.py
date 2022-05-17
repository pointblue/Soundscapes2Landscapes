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

mel_spec_dir = '/media/third/2020/mel_meth/birds_37/vanilla_mel/raw_mel_spec/tr/'
pkl_dir = '/media/third/2020/mel_meth/birds_37/vanilla_mel/raw_pkl/tr/'

print('No of folders here, in mel_spec_dir:',os.listdir(mel_spec_dir), os.listdir(pkl_dir))
cutoff_sp_name = joblib.load('handy_pkls/cutoff_sp_name.pkl')


#print(cutoff_sp_name)

for i in range(len(cutoff_sp_name)):
    #print(cutoff_sp_name[i][1])
    os.mkdir(os.path.join(mel_spec_dir, cutoff_sp_name[i][1]))
    
print(os.listdir(mel_spec_dir))


for i in range(len(cutoff_sp_name)):
    #print(cutoff_sp_name[i][1])
    os.mkdir(os.path.join(pkl_dir, cutoff_sp_name[i][1]))
    
print(os.listdir(pkl_dir))
print(len(os.listdir(pkl_dir)))

# split the data in training and validation

wav_dir = '/media/third/2020/June_1/wav_6June/'
roi_len = 2.000 # IMPORTANT !
roi_len_str = str(2000)

n_sam = int(22050 * roi_len)  #21168

for wav_nm in (os.listdir(wav_dir)):
    cnt = 0
    if roi_len_str in wav_nm:
        print(wav_nm)
        
        y, sr = librosa.load(os.path.join(wav_dir, wav_nm), sr = 22050)
        print(y.shape)
        
        tot_sam = int(np.shape(y)[0]/n_sam)
        print("Tot-Sam:",tot_sam)
        tot_sam = tot_sam - 100
        spl = 100 * n_sam # total training samples
        
        wav_pth = os.path.join('/media/third/2020/mel_meth/birds_37/split_wav_2000',wav_nm.split('.')[0]+'_tr_2000.wav')
        librosa.output.write_wav(wav_pth, y[spl:], sr  = 22050)
        print('SAVED THESE MANY SAMPLES IN TRAINING', y[spl:].shape)
        print(y[spl:].shape)
        
        wav_pth = os.path.join('/media/third/2020/mel_meth/birds_37/split_wav_2000',wav_nm.split('.')[0]+'_val_2000.wav')
        librosa.output.write_wav(wav_pth, y[:spl], sr  = 22050)
        print('SAVED THESE MANY SAMPLES IN VALIDATION', y[:spl].shape)
        
        print('-'*75)

SR = 22050  # Sampling frequency
N_MELS = 128  # Mel band parameters
WIN_SIZE = 1024  # number of samples in each STFT window
WINDOW_TYPE = 'hann'  # the windowin function
FEATURE = 'mel'  # feature representation


roi_len = 2.000 # IMPORTANT !
roi_len_str = 'val' # training set!

print('Generating ROI PNG images with',roi_len_str)

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
        
        
        tot_sam = int(np.shape(y)[0]/n_sam)
        
        print('Total number of possible ROIs:', tot_sam)
        for n in range(tot_sam):
            
            y_21k = y[n*n_sam:(n+1)*n_sam]
            pkl_directory = os.path.join(pkl_dir, wav_nm.split('_')[0] +'/' + (wav_nm.split('.')[0] + '_pkl_' +str(cnt) +'.pkl'))
            
            #print('Converting these samples to mel-spec:', np.shape(y_21k))
            joblib.dump(y_21k, pkl_directory)
            
            
            fig = plt.figure(1, frameon=False)
            fig.set_size_inches(6, 6)
            ax = plt.Axes(fig, [0., 0., 1., 1.])
            ax.set_axis_off()
            fig.add_axes(ax)


            S = librosa.feature.melspectrogram(y=y_21k, sr=22050, n_mels=128,fmin = 0,
                                     fmax=11025, n_fft=728, hop_length=32, win_length = None, htk = True) # hop length lesser the better
            
            
            librosa.display.specshow(librosa.power_to_db(S ** 1, ref=np.max), fmin=0, y_axis='linear')# , cmap = 'gray')
            
            directory = os.path.join(mel_spec_dir, wav_nm.split('_')[0] +'/' + (wav_nm.split('.')[0] + '_' +str(cnt) +'.png'))  # 'test.png'
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
