# Soundscapes2Landscapes (S2L) Project
## Supporting code, metadata, dataset and models used for analyses contained in DOI TBD.  Data can be found at ZENODO DOI.

P-I: Dr. Matthew L. Clark (<matthew.clark@sonoma.edu>)

***
### [CNN_Bird_Species](CNN_Bird_Species/) contains code to perform the following tasks locally or in an HPC environment:
#### [dataSet_models](CNN_Bird_Species/dataSet_models/)
- Generate mel spectrogram representations of (1) ROIs and (2) entire wav recordings
- Calculate predicted bird species CNN probabilities (inference)
- If you want to use the pre-built S2L_54 Dataset, you can download it from Zenodo. Details are mentioned in the Dataset section below and the Zenodo DOI, above. 

#### [CNN_pretraining](CNN_Bird_Species/CNN_pretraining/)
- Data used were from the BirdCLEF dataset, https://www.kaggle.com/datasets/imoore/xenocanto-bird-recordings-dataset
- Identify ROIs from xeno-canto data using R’s warbleR
- Pretrain ImageNet CNNs using xeno-canto ROIs

#### [Conda_envs](CNN_Bird_Species/Conda_envs/)
- Install python conda environments

***
### Further information on scripts in this repository:
#### 1. Inference
The following files will help running inference using the saved models based on MobileNetV2, ResNet101 and ResNet50.
- [saved_models.zip](CNN_Bird_Species/dataSet_models/)  
  - Load the above saved_models in Tensorflow 2.0 and run inference using the inference code.
  - tfgpu.yml can be used to install the python environment. 
- [5c_inference_local_with_csv.py](CNN_Bird_Species/dataSet_models/) - To run this script on a local machine:
  - Create a csv or dataframe of folder names containing mel spectrograms
  - Update variables in lines 27-30 to reflect local setup of data

#### 2. Build S2L dataset
To build the Dataset from scratch, use the following three notebooks/python code. You will also need the raw S2L Corpus to extract the ROIs. You can get that *(from NASA Archive/Database COMING SOON)* 
- [1_fromCSV_toROIs_toMegaWavs.py](CNN_Bird_Species/dataSet_models/): Use the CSVs provided in the ROI_CSV.zip to extract ROIs from Raw S2L Data (?from NASA Archive/Database?) Later, you string together these ROIs to form Megawavs as explained in the paper.
- [2_generateROI-Melspecs.py](CNN_Bird_Species/dataSet_models/): Use the Megawavs created above and chop them into desirable duration ROIs. We convert them into 2-sec ROIs sampled at 22050 Hz. We convert them into .png Mel Spectrograms here.
- [3_generateAugmentedTrSamples.py](CNN_Bird_Species/dataSet_models/): To increase the amount of training data and make models more robust, we perform class-wise overlaid data augmentation techniques that is briefly explained in the paper

#### 3. CNN xeno-canto Pre-training and HPC processing
Other scripts in repository demonstrate xeno-canto acoustic pre-training, warbleR acoustic event detection, and data processing implementation on a local HPC. These scripts use a bash .sh script to submit a python or R script on the HPC cluster using a SLURM scheduler:
- [4a_melspec_generation_csv-HPC.sh](CNN_Bird_Species/dataSet_models/) / [4b_melspec_generation_csv.py](CNN_Bird_Species/dataSet_models/): generate all S2L mel spectrograms on HPC setup.
- [5a_inference_with_csv-HPC.sh](CNN_Bird_Species/dataSet_models/) / [5b_inference_HPC_model_with_csv.sh](CNN_Bird_Species/dataSet_models/): run prediction on S2L mel spectrogram dataset using HPC setup.
- [cnn_pretrain_gpus.sh](CNN_Bird_Species/CNN_pretraining/) / [mobnet_pretrain.py](CNN_Bird_Species/CNN_pretraining/): pre-train, here, an ImageNetv2 weighted MobileNetv2 architecture using Xeno-Canto data on an HPC.
- [warbleR_XC.sh](CNN_Bird_Species/CNN_pretraining/) / [warbleR_XC.R](CNN_Bird_Species/CNN_pretraining/): acoustic event detection in the Xeno-Canto dataset on an HPC. The result is a csv for each recording that is then used in the melspec generation set of scripts (1_fromCSV_toROIs_toMegaWavs.py, 2_generateROI-Melspecs.py, 3_generateAugmentedTrSamples.py).
- CNN_Pretraining.zip: Pre-train the three networks on the other open-source Datasets mentioned in the Paper.

***
## TO DO:
- [ ] Add Zenodo DOIs to lines 2 and 51
- [ ] Add Paper DOI to line 2
- [ ] Confirm what is in CNN_pretraining.zip (line 43)
- [ ] Confirm dataset section reflects published Zenodo Repo 









