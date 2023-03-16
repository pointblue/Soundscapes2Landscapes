### CNN_Bird_Species contains code to perform the following tasks associated with the associated peer-reviewed paper: 
Clark, M.L., Salas, L., Baligar, S., Quinn, C., Snyder, R.L., Leland, D., Schackwitz, W., Goetz, S.J., Newsam, S. (2023). The effect of soundscape composition on bird vocalization classification in a citizen science biodiversity monitoring project. Ecological Informatics. https://doi.org/10.1016/j.ecoinf.2023.102065

Data can be found at Zenodo https://zenodo.org/record/6512218

Sound recordings are available upon request: [Dr. Matthew Clark](matthew.clark@sonoma.edu)

### Citation
Please cite the above paper if using these data, code, or models.

These data, code and models in this repository are from the Soundscapes to Landscapes project (soundscapes2landscapes.org), funded by NASA’s Citizen Science for Earth Systems Program (CSESP) 16-CSESP 2016-0009 under cooperative agreement 80NSSC18M0107.

***
#### [CNN_melspec_and_inference](CNN_melspec_and_inference/)
- Generate mel spectrogram representations of (1) ROIs and (2) entire wav recordings.
- Calculate predicted bird species CNN probabilities (inference).
- If you want to use the pre-built S2L_54 Dataset, you can download it from the Zenodo link, above. 

#### [CNN_pretraining](CNN_pretraining/)
- Data used were from the xeno-canto dataset at https://www.kaggle.com/datasets/imoore/xenocanto-bird-recordings-dataset. Dataset found in Zenodo depository.
- Identify ROIs from xeno-canto data using R’s warbleR.
- Pretrain ImageNet CNNs using xeno-canto ROIs.

#### [CNN_post-processing](CNN_post-processing/)
- Full code stack used to generate all figures and tables shown in the paper.
- All the datasets used, including data from ARBIMON (.csv files), AirTable (queried into .RData files), and predictions (read from pickle files into .RData files).
- The entire database of predictions into a mysqldump file. Data can be found at the associated Zenodo code repository.
- A README file with the logic of the full code stack.

#### [Conda_envs](Conda_envs/)
- Install python conda environments

***
### Further information on scripts in this repository:
#### 1. Inference
The following files will help running inference using the saved models based on MobileNetV2, ResNet101 and ResNet50.
- [saved_models.zip](CNN_melspec_and_inference/saved_models.zip)  
  - Load the above saved_models in Tensorflow 2.0 and run inference using the inference code.
  - tfgpu.yml can be used to install the python environment. 
- [5c_inference_local_with_csv.py](CNN_melspec_and_inference/5c_inference_local_with_csv.py) - To run this script on a local machine:
  - Create a csv or dataframe of folder names containing mel spectrograms
  - Update variables in lines 27-30 to reflect local setup of data

#### 2. Build S2L dataset
To build the Dataset from scratch, use the following three notebooks/python code. You will also need the raw S2L Corpus to extract the ROIs. You can get that *(from NASA Archive/Database COMING SOON)* 
- [1_csv_toROI_toMegawav.py](CNN_melspec_and_inference/1_csv_toROI_toMegawav.py): Use the CSVs provided in the ROI_CSV.zip to extract ROIs from Raw S2L Data (?from NASA Archive/Database?) Later, you string together these ROIs to form Megawavs as explained in the paper.
- [2_generateROI.py](CNN_melspec_and_inference/2_generateROI.py): Use the Megawavs created above and chop them into desirable duration ROIs. We convert them into 2-sec ROIs sampled at 22050 Hz. We convert them into .png Mel Spectrograms here.
- [3_generateAugmentedSamples.py](CNN_melspec_and_inference/3_generateAugmentedSamples.py): To increase the amount of training data and make models more robust, we perform class-wise overlaid data augmentation techniques that is briefly explained in the paper

#### 3. CNN xeno-canto Pre-training and HPC processing
Other scripts in repository demonstrate xeno-canto acoustic pre-training, warbleR acoustic event detection, and data processing implementation on a local HPC. These scripts use a bash .sh script to submit a python or R script on the HPC cluster using a SLURM scheduler:
- [4a_melspec_generation_csv-HPC.sh](CNN_melspec_and_inference/4a_melspec_generation_csv-HPC.sh) / [4b_melspec_generation_csv.py](CNN_melspec_and_inference/4b_melspec_generation_csv.py): generate all S2L mel spectrograms on HPC setup.
- [5a_inference_with_csv-HPC.sh](CNN_melspec_and_inference/5a_inference_with_csv-HPC.sh) / [5b_inference_HPC_model_with_csv.py](CNN_melspec_and_inference/5b_inference_HPC_model_with_csv.py): run prediction on S2L mel spectrogram dataset using HPC setup.
- [cnn_pretrain_gpus.sh](CNN_pretraining/cnn_pretrain_gpus.sh) / [mobnet_pretrain.py](CNN_pretraining/mobnet_pretrain.py): pre-train, here, an ImageNetv2 weighted MobileNetv2 architecture using Xeno-Canto data on an HPC.
- [warbleR_XC.sh](CNN_pretraining/warbleR_XC.sh) / [warbleR_XC.R](CNN_pretraining/warbleR_XC.R): acoustic event detection in the Xeno-Canto dataset on an HPC. The result is a csv for each recording that is then used in the melspec generation set of scripts (1_fromCSV_toROIs_toMegaWavs.py, 2_generateROI-Melspecs.py, 3_generateAugmentedTrSamples.py).
