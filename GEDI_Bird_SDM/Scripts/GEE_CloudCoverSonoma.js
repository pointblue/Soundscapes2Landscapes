// Calculate Mean Annual Cloud Cover for All Sonoma County
var sonoma_mean_cld = meanAnn.reduceRegion(ee.Reducer.mean(), geom, 1000)
var sonoma_stdev_cld = meanAnn.reduceRegion(ee.Reducer.stdDev(), geom, 1000)
var sonoma_95p_cld = meanAnn.reduceRegion(ee.Reducer.percentile([95]), geom, 1000)
Map.addLayer(meanAnn.multiply(0.01))
Map.addLayer(geom)
print('The mean annual cloud cover for Sonoma County is ', ee.Number(sonoma_mean_cld.get("b1")).multiply(0.01))
print('The mean annual cloud cover standard deviation for Sonoma County is ', ee.Number(sonoma_stdev_cld.get("b1")).multiply(0.01))
print('The mean annual cloud cover 95th percentile for Sonoma County is ', ee.Number(sonoma_95p_cld.get("b1")).multiply(0.01))


// Display a histogram of mean annual cloud cover
var hist = ui.Chart.image.histogram({
  image: meanAnn.multiply(0.01),
  region: geom,
  scale: 1000,
  maxBuckets: 20
})

print(hist)


// Create an image collection of monthly mean cloud cover
var sonoma_cc_mon = ee.ImageCollection.fromImages([
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_01').multiply(0.01).set({'month': 1}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_02').multiply(0.01).set({'month': 2}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_03').multiply(0.01).set({'month': 3}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_04').multiply(0.01).set({'month': 4}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_05').multiply(0.01).set({'month': 5}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_06').multiply(0.01).set({'month': 6}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_07').multiply(0.01).set({'month': 7}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_08').multiply(0.01).set({'month': 8}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_09').multiply(0.01).set({'month': 9}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_10').multiply(0.01).set({'month': 10}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_11').multiply(0.01).set({'month': 11}),
  ee.Image('users/pb463/Clouds/Global1KM_CC_EarthEnv/Acc_201803/MODCF_monthlymean_12').multiply(0.01).set({'month': 12}),
  ])
  
print(sonoma_cc_mon)

var cc_mean_mon_chart = ui.Chart.image.series({
  imageCollection: sonoma_cc_mon,
  region: geom,
  reducer: ee.Reducer.mean(),
  scale: 1000,
  xProperty: 'month'
})

print(cc_mean_mon_chart)