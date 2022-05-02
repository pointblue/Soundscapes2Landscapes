#!/scratch/cq73/.conda/envs/tfgpu/bin/ python3.7
# coding: utf-8

import tensorflow as tf
import IPython.display as display
from PIL import Image
import numpy as np
import matplotlib.pyplot as plt
import os
import pathlib

# visualizations
import matplotlib
matplotlib.use('Agg') 
matplotlib.rcParams['figure.dpi'] = 72 #dpi manually set to match S2L machines

# aux libs
import random
import joblib

AUTOTUNE = tf.data.experimental.AUTOTUNE
tf.__version__

print("Entering data prep....")

###################################
##### DATA INPUTS AND OUTPUTS #####
# Set up training data with pre-split melspec partitions
# validation = 150 if class has >= 750 pngs or 20% if <750
tr_pth = '/projects/tropics/users/cquinn/s2l/XC_cnn_pretrain/warbleR_rand_val150/training/'
data_dir = pathlib.Path(tr_pth)
image_count = len(list(data_dir.glob('*/*.png')))
print("Training image count:",image_count)

# Set up the validation data
val_pth = '/projects/tropics/users/cquinn/s2l/XC_cnn_pretrain/warbleR_rand_val150/validation/'
val_data_dir = pathlib.Path(val_pth)
val_image_count = len(list(val_data_dir.glob('*/*.png')))
print("Val image count:",val_image_count)

# results directory
out_dir = '/scratch/cq73/projects/S2L/cnn_pretrain/results/'

# model version
modName = 'mobnetv2_saved_model_xc2_warbleR_rand_val150'

# modeling params
initial_epochs = 10
fine_tune_epochs = 10

###########################
##### DEFS FOR SCRIPT #####
def get_label(file_path):
  # convert the path to a list of path components
  parts = tf.strings.split(file_path, os.path.sep)
  # The second to last is the class-directory
  return parts[-2] == CLASS_NAMES

def decode_img(img):
  # convert the compressed string to a 3D uint8 tensor
  img = tf.image.decode_jpeg(img, channels=3)
  # Use `convert_image_dtype` to convert to floats in the [0,1] range.
  img = tf.image.convert_image_dtype(img, tf.float32)
  # resize the image to the desired size.
  return tf.image.resize(img, [IMG_HEIGHT, IMG_WIDTH])

def process_path(file_path):
    label = get_label(file_path)
    # load the raw data from the file as a string
    img = tf.io.read_file(file_path)
    img = decode_img(img)
    return img, label

def prepare_for_training(ds, cache=True, shuffle_buffer_size=1000, repeat=1):
  if cache:
    if isinstance(cache, str):
        ds = ds.cache(cache)
    else:
        ds = ds.cache()

  ds = ds.shuffle(buffer_size=shuffle_buffer_size)

  # Repeat forever
  ds = ds.repeat(repeat) 
  ds = ds.batch(BATCH_SIZE)
  ds = ds.prefetch(buffer_size=AUTOTUNE)

  return ds

def show_batch(image_batch, label_batch):
  plt.figure(figsize=(10,10))
  for n in range(25):
      ax = plt.subplot(5,5,n+1)
      plt.imshow(image_batch[n])
      plt.title(CLASS_NAMES[label_batch[n]==1][0].title())
      plt.axis('off')


##############################
##### PREP TRAINING DATA #####
CLASS_NAMES = np.array([item.name for item in data_dir.glob('*') if item.name != "LICENSE.txt"])
print(CLASS_NAMES)

# The 1./255 is to convert from uint8 to float32 in range [0,1].
image_generator = tf.keras.preprocessing.image.ImageDataGenerator(rescale=1./255)

BATCH_SIZE = 64
IMG_HEIGHT = 224
IMG_WIDTH = 224
STEPS_PER_EPOCH = np.ceil(image_count/BATCH_SIZE)
list_ds = tf.data.Dataset.list_files(str(data_dir/'*/*'))

for f in list_ds.take(5):
    print(f.numpy())

# Set `num_parallel_calls` so multiple images are loaded/processed in parallel.
labeled_ds = list_ds.map(process_path, num_parallel_calls=AUTOTUNE)

for image, label in labeled_ds.take(1):
    print("Image shape: ", image.numpy().shape)
    print("Label: ", label.numpy())

train_ds = prepare_for_training(labeled_ds, repeat = None)
image_batch, label_batch = next(iter(train_ds))

################################
##### PREP VALIDATION DATA #####
CLASS_NAMES = np.array([item.name for item in val_data_dir.glob('*') if item.name != "LICENSE.txt"])
CLASS_NAMES

# The 1./255 is to convert from uint8 to float32 in range [0,1].
image_generator = tf.keras.preprocessing.image.ImageDataGenerator(rescale=1./255)

STEPS_PER_EPOCH = np.ceil(val_image_count/BATCH_SIZE)

list_ds = tf.data.Dataset.list_files(str(val_data_dir/'*/*'))

for f in list_ds.take(5):
    print(f.numpy())

# Set `num_parallel_calls` so multiple images are loaded/processed in parallel.
labeled_ds = list_ds.map(process_path, num_parallel_calls=AUTOTUNE)

for image, label in labeled_ds.take(1):
    print("Image shape: ", image.numpy().shape)
    print("Label: ", label.numpy())

validation_ds = prepare_for_training(labeled_ds)
image_batch, label_batch = next(iter(validation_ds))

# NOW WE HAVE:
print(validation_ds)
print(train_ds)

###########################
##### START THE MODEL #####
print("Starting initial train....")
IMG_SIZE = 224
IMG_SHAPE = (IMG_SIZE, IMG_SIZE, 3)

# Create the base model from the pre-trained model ResNet-50 V2
base_model = tf.keras.applications.MobileNetV2(input_shape=IMG_SHAPE,
                                               include_top=False, # Not including the FC layer when "False"
                                               weights='imagenet',
                                               alpha= 1.4)

feature_batch = base_model(image_batch)
print(feature_batch.shape)

base_model.trainable = False

global_average_layer = tf.keras.layers.GlobalAveragePooling2D()
feature_batch_average = global_average_layer(feature_batch)
print(feature_batch_average.shape)

n_classes = len(CLASS_NAMES) # based on the no. of classes
prediction_layer = tf.keras.layers.Dense(n_classes, activation = None) # no activation now. Will be applying sigmoid later suring inference
prediction_batch = prediction_layer(feature_batch_average)
print(prediction_batch.shape)

model = tf.keras.Sequential([
  base_model,
  global_average_layer,
  prediction_layer
])

base_learning_rate = 0.0001 #the initial learning rate. This will be reduced by a factor of 10 in the Finetuning stage

model.compile(optimizer = tf.keras.optimizers.Adam(lr=base_learning_rate),
              metrics=tf.keras.metrics.CategoricalAccuracy(),
              loss=tf.keras.losses.BinaryCrossentropy(from_logits=True))         #Whether to interpret y_pred as a tensor of logit values. By default, we assume that y_pred contains probabilities (i.e., values in [0, 1]). **Note - Using from_logits=True may be more numerically stable.           
        
model.summary()

len(model.trainable_variables)

# NOW USE THE validation_ds and train_ds THAT WE BUILT BEFORE
loss0,accuracy0 = model.evaluate(validation_ds, steps= val_image_count // BATCH_SIZE)
print("initial loss: {:.2f}".format(loss0))
print("initial accuracy: {:.2f}".format(accuracy0))

history = model.fit(train_ds,
                    epochs=initial_epochs,
                    validation_data=validation_ds, 
                    steps_per_epoch = np.ceil(image_count/BATCH_SIZE))

acc = history.history['categorical_accuracy']
val_acc = history.history['val_categorical_accuracy']

loss = history.history['loss']
val_loss = history.history['val_loss']

plt.figure(figsize=(8, 8))
plt.subplot(2, 1, 1)
plt.plot(acc, label='Training Accuracy')
plt.plot(val_acc, label='Validation Accuracy')
plt.legend(loc='lower right')
plt.ylabel('Accuracy')
plt.ylim([min(plt.ylim()),1])
plt.title('Training and Validation Accuracy')

plt.subplot(2, 1, 2)
plt.plot(loss, label='Training Loss')
plt.plot(val_loss, label='Validation Loss')
plt.legend(loc='upper right')
plt.ylabel('Cross Entropy')
plt.ylim([0,1.0])
plt.title('Training and Validation Loss')
plt.xlabel('epoch')

# save output plot for first training 
plt.savefig(os.path.join(out_dir + modName + '_initial_tr.png'))

model.save(out_dir + modName) 
print("saved model at :", out_dir, modName)

print("Starting final train....")


# This will make the whole network trainable
base_model.trainable = True

# Let's take a look to see how many layers are in the base model
print("Number of layers in the base model: ", len(base_model.layers))

# Fine-tune from this layer onwards
fine_tune_at = 0

# Freeze all the layers before the `fine_tune_at` layer
for layer in base_model.layers[:fine_tune_at]:
    layer.trainable =  False

model.compile(optimizer = tf.keras.optimizers.Adam(lr=base_learning_rate),
              metrics=tf.keras.metrics.CategoricalAccuracy(),
              loss=tf.keras.losses.BinaryCrossentropy(from_logits=True))         #Whether to interpret y_pred as a tensor of logit values. By default, we assume that y_pred contains probabilities (i.e., values in [0, 1]). **Note - Using from_logits=True may be more numerically stable.            

model.summary()

len(model.trainable_variables)

total_epochs =  initial_epochs + fine_tune_epochs
history_fine = model.fit(train_ds,
                         epochs=total_epochs,
                         initial_epoch =  history.epoch[-1],
                         validation_data = validation_ds,
                         steps_per_epoch = np.ceil(image_count/BATCH_SIZE))

# Save 
model.save(out_dir + modName) 
print("saved model at :", out_dir, modName)
