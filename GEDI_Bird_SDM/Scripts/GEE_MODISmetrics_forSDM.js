/* GEDI + S2L - Sonoma County
 Compute MODIS metrics:
    ndvi minimum (DHI)
    ndvi 5th percentile
    ndvi median
    ndvi 95th percentile
    ndvi seasonal difference
    ndvi variance (DHI)
    ndvi cumulative (DHI)

 by Patrick Burns
 last updated: 2018-06-22

 DHI methodology from 
   Hobi, M.L., Dubinin, M., Graham, C.H., Coops, N.C., Clayton, M.K., Pidgeon, A.M., & Radeloff, V.C. (2017). 
   A comparison of Dynamic Habitat Indices derived from different MODIS products as predictors of avian species richness. 
   Remote Sensing of Environment, 195, 142-152.
   Link: http://silvis.forest.wisc.edu/data/DHIs
*/


/*-----------------------/
/ ----- DATA IMPORT ---- /
/-----------------------*/

// Load in and create necessary feature classes
// Sonoma county shapefile
var sonomafc = ee.FeatureCollection('users/pb463/S2L/Sonoma_cty_v2_PBcleaned')
var sonoma = ee.Feature(sonomafc.first()).buffer(1000)
Map.addLayer(sonoma, {color:'red'}, 'Sonoma Cty Boundary')

// Bounding rectangle in projected coordinates
var rect = ee.Geometry.Rectangle({
  coords: [450000, 4210000+1, 560000-1, 4320000],
  proj: 'EPSG:32610',
  geodesic: false,
  evenOdd: true
})
//Map.addLayer(rect, {color:'green'}, 'Processing Extent')

// A bigger shape for processing and export
var bigClip = /* color: #d6cc2e */ee.Geometry.Polygon(
        [[[-123.9532470703125, 39.16835008003937],
          [-123.28857421875, 37.93114968513442],
          [-121.9537353515625, 37.9094828276104],
          [-122.530517578125, 39.155572476393786]]]);
Map.addLayer(bigClip, {'color':'#d6cc2e'}, 'Clipping Shape')

// Load MODIS collections
var modvi_16d_250m = ee.ImageCollection('MODIS/006/MOD13Q1')
var modvi_16d_500m = ee.ImageCollection('MODIS/006/MOD13A1')
var modvi_16d_1000m = ee.ImageCollection('MODIS/006/MOD13A2')


// Check on pixel size and projection info
var mod_ndvi_250m_check = ee.Image('MODIS/006/MOD13Q1/2012_10_15')
var mod_ndvi_500m_check = ee.Image('MODIS/006/MOD13A1/2012_10_15')
var mod_ndvi_1000m_check = ee.Image('MODIS/006/MOD13A2/2012_10_15')

print('The nominal 250m pixel size is ', mod_ndvi_250m_check.projection().nominalScale())
print('The nominal 500m pixel size is ', mod_ndvi_500m_check.projection().nominalScale())
print('The nominal 1000m pixel size is ', mod_ndvi_1000m_check.projection().nominalScale())

print('The projection info is ', mod_ndvi_250m_check.projection().wkt())

print('The start date for the first image is', ee.Date(mod_ndvi_250m_check.get('system:time_start')))
print('The end date for the first image is ', ee.Date(mod_ndvi_250m_check.get('system:time_end')))

// Do a visual check of the various resolutions
Map.addLayer(mod_ndvi_250m_check.clip(bigClip), {min:0, max: 10000}, 'MOD 250m NDVI Check')
Map.addLayer(mod_ndvi_500m_check.clip(bigClip), {min:0, max: 10000}, 'MOD 500m NDVI Check')
Map.addLayer(mod_ndvi_1000m_check.clip(bigClip), {min:0, max: 10000}, 'MOD 1000m NDVI Check')


/*-----------------------/
/ ----- PROCESSING ---- /
/-----------------------*/

// Filter each image collection spatially, temporally, and using QA bands (see DHI methods).
var vi_250m_filt = modvi_16d_250m.filterBounds(bigClip)
                                 .filterDate('2012-10-01', '2015-09-30')
                                 .map(function(img){
                                      var vi = ee.Image(img.select(['NDVI', 'EVI']).multiply(0.0001)
                                                               .addBands(img, ['DetailedQA'])
                                                               .copyProperties(img,['system:footprint', 'system:index', 'system:time_end', 'system:time_start']))
                                                               
                                      var mask = vi.select('DetailedQA').lt(5411)
                                                                        .or(vi.select('DetailedQA').gte(18433).and(vi.select('DetailedQA').lte(21798)))
                                                                        .or(vi.select('DetailedQA').gte(34817).and(vi.select('DetailedQA').lte(38378)))
                                                                        .or(vi.select('DetailedQA').gte(51201).and(vi.select('DetailedQA').lte(54574)))
                                      
                                      var vi_masked = vi.select(['NDVI', 'EVI'])
                                                        .updateMask(mask)  
                                                        .addBands(vi, ['DetailedQA'])
                                                        .copyProperties(img,['system:footprint', 'system:index', 'system:time_end', 'system:time_start'])
                                      
                                      return vi_masked
                                })
                                
var vi_500m_filt = modvi_16d_500m.filterBounds(bigClip)
                                 .filterDate('2012-10-01', '2015-09-30')
                                 .map(function(img){
                                      var vi = ee.Image(img.select(['NDVI', 'EVI']).multiply(0.0001)
                                                               .addBands(img, ['DetailedQA'])
                                                               .copyProperties(img,['system:footprint', 'system:index', 'system:time_end', 'system:time_start']))
                                                               
                                      var mask = vi.select('DetailedQA').lt(5411)
                                                                        .or(vi.select('DetailedQA').gte(18433).and(vi.select('DetailedQA').lte(21798)))
                                                                        .or(vi.select('DetailedQA').gte(34817).and(vi.select('DetailedQA').lte(38378)))
                                                                        .or(vi.select('DetailedQA').gte(51201).and(vi.select('DetailedQA').lte(54574)))
                                      
                                      var vi_masked = vi.select(['NDVI', 'EVI'])
                                                        .updateMask(mask)  
                                                        .addBands(vi, ['DetailedQA'])
                                                        .copyProperties(img,['system:footprint', 'system:index', 'system:time_end', 'system:time_start'])
                                      
                                      return vi_masked
                                })
                                
var vi_1000m_filt = modvi_16d_1000m.filterBounds(bigClip)
                                   .filterDate('2012-10-01', '2015-09-30')
                                   .map(function(img){
                                      var vi = ee.Image(img.select(['NDVI', 'EVI']).multiply(0.0001)
                                                               .addBands(img, ['DetailedQA'])
                                                               .copyProperties(img,['system:footprint', 'system:index', 'system:time_end', 'system:time_start']))
                                                               
                                      var mask = vi.select('DetailedQA').lt(5411)
                                                                        .or(vi.select('DetailedQA').gte(18433).and(vi.select('DetailedQA').lte(21798)))
                                                                        .or(vi.select('DetailedQA').gte(34817).and(vi.select('DetailedQA').lte(38378)))
                                                                        .or(vi.select('DetailedQA').gte(51201).and(vi.select('DetailedQA').lte(54574)))
                                      
                                      var vi_masked = vi.select(['NDVI', 'EVI'])
                                                        .updateMask(mask)  
                                                        .addBands(vi, ['DetailedQA'])
                                                        .copyProperties(img,['system:footprint', 'system:index', 'system:time_end', 'system:time_start'])
                                      
                                      return vi_masked
                                })
                                

print('The filtered 250m VI image collection list ', vi_250m_filt)
print('The filtered 500m VI image collection list ', vi_500m_filt)
print('The filtered 1000m VI image collection list ', vi_1000m_filt)

Map.addLayer(ee.Image(vi_250m_filt.first()).clip(bigClip), null, '250m filtered VI check')


// Make a list of MODIS 16 day DOYs                                
var mod_16d_doys = ee.List.sequence(289,365,16).cat(ee.List.sequence(1,273,16))
print('MODIS 16 day DOYs', mod_16d_doys)

// Compute the median at each DOY for the 3 year period
var ndvi_250m_WY13to15_DOYmed = ee.ImageCollection(mod_16d_doys.map(function(day){
  return vi_250m_filt.filter(ee.Filter.calendarRange(day, day, 'day_of_year'))
                                          .select('NDVI').median().set({'DOY': day})
}))

var ndvi_500m_WY13to15_DOYmed = ee.ImageCollection(mod_16d_doys.map(function(day){
  return vi_500m_filt.filter(ee.Filter.calendarRange(day, day, 'day_of_year'))
                                          .select('NDVI').median().set({'DOY': day})
}))

var ndvi_1000m_WY13to15_DOYmed = ee.ImageCollection(mod_16d_doys.map(function(day){
  return vi_1000m_filt.filter(ee.Filter.calendarRange(day, day, 'day_of_year'))
                                          .select('NDVI').median().set({'DOY': day})
}))

print('Median NDVI at 250m every 16 days ', ndvi_250m_WY13to15_DOYmed)
print('Median NDVI at 500m every 16 days ', ndvi_500m_WY13to15_DOYmed)
print('Median NDVI at 1000m every 16 days ', ndvi_1000m_WY13to15_DOYmed)


// Compute Min NDVI
var ndvi_250m_ann_min = ndvi_250m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.min())
var ndvi_500m_ann_min = ndvi_500m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.min())
var ndvi_1000m_ann_min = ndvi_1000m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.min())

// Compute the 5th percentile NDVI
var ndvi_250m_ann_05p = ndvi_250m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.percentile([5]))
var ndvi_500m_ann_05p = ndvi_500m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.percentile([5]))
var ndvi_1000m_ann_05p = ndvi_1000m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.percentile([5]))

// Compute the median NDVI
var ndvi_250m_ann_med = ndvi_250m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.median())
var ndvi_500m_ann_med = ndvi_500m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.median())
var ndvi_1000m_ann_med = ndvi_1000m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.median())

// Compute the 95th percentile NDVI
var ndvi_250m_ann_95p = ndvi_250m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.percentile([95]))
var ndvi_500m_ann_95p = ndvi_500m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.percentile([95]))
var ndvi_1000m_ann_95p = ndvi_1000m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.percentile([95]))

// Compute the seasonal difference of NDVI
var ndvi_250m_June = ndvi_250m_WY13to15_DOYmed.select('NDVI').filterMetadata('DOY','greater_than',152).filterMetadata('DOY','less_than',181)
                                        .mean()
var ndvi_500m_June = ndvi_500m_WY13to15_DOYmed.select('NDVI').filterMetadata('DOY','greater_than',152).filterMetadata('DOY','less_than',181)
                                        .mean()
var ndvi_1000m_June = ndvi_1000m_WY13to15_DOYmed.select('NDVI').filterMetadata('DOY','greater_than',152).filterMetadata('DOY','less_than',181)
                                        .mean()
                                        
var ndvi_250m_Nov = ndvi_250m_WY13to15_DOYmed.select('NDVI').filterMetadata('DOY','greater_than',305).filterMetadata('DOY','less_than',334)
                                       .mean()
var ndvi_500m_Nov = ndvi_500m_WY13to15_DOYmed.select('NDVI').filterMetadata('DOY','greater_than',305).filterMetadata('DOY','less_than',334)
                                       .mean()
var ndvi_1000m_Nov = ndvi_1000m_WY13to15_DOYmed.select('NDVI').filterMetadata('DOY','greater_than',305).filterMetadata('DOY','less_than',334)
                                       .mean()

var ndvi_250m_seas_diff = ndvi_250m_June.subtract(ndvi_250m_Nov)
var ndvi_500m_seas_diff = ndvi_500m_June.subtract(ndvi_500m_Nov)
var ndvi_1000m_seas_diff = ndvi_1000m_June.subtract(ndvi_1000m_Nov)

// Compute the annual variance of NDVI
var ndvi_250m_var = ndvi_250m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.variance())
var ndvi_500m_var = ndvi_500m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.variance())
var ndvi_1000m_var = ndvi_1000m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.variance())

// Compute the annual sum of NDVI
var ndvi_250m_sum = ndvi_250m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.sum())
var ndvi_500m_sum = ndvi_500m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.sum())
var ndvi_1000m_sum = ndvi_1000m_WY13to15_DOYmed.select('NDVI').reduce(ee.Reducer.sum())

// Check the metrics visually
Map.addLayer(ndvi_250m_ann_min.clip(bigClip), {min:-1, max: 1}, 'NDVI minimum')
Map.addLayer(ndvi_250m_ann_05p.clip(bigClip), {min:-1, max: 1}, 'NDVI 5th percentile')
Map.addLayer(ndvi_250m_ann_med.clip(bigClip), {min:-1, max: 1}, 'NDVI median')
Map.addLayer(ndvi_250m_ann_95p.clip(bigClip), {min:-1, max: 1}, 'NDVI 95th percentile')
Map.addLayer(ndvi_250m_seas_diff.clip(bigClip), {min:-0.5, max: 0.5}, 'NDVI seasonal difference')
Map.addLayer(ndvi_250m_var.clip(bigClip), {min:0, max: 0.1}, 'NDVI Variance')
Map.addLayer(ndvi_250m_sum.clip(bigClip), {min:-20, max: 20}, 'NDVI Cumulative')


/*-----------------------/
/ ----- PLOTS ---- /
/-----------------------*/

// Plot DOY NDVI for Grass
var ndvi_grass_ts_byYear = ui.Chart.image.doySeriesByYear({
  imageCollection: vi_250m_filt, 
  region: grass, 
  regionReducer: ee.Reducer.mean(), 
  bandName: 'NDVI', 
  scale: 250
}).setOptions({
  title: 'NDVI of Grass by Year',
  vAxis: {
  title: 'NDVI',
  ticks: [0.00, 0.25, 0.50, 0.75, 1.00]}
  })
print(ndvi_grass_ts_byYear)

// Plot DOY NDVI for Conifers
var ndvi_con_ts_byYear = ui.Chart.image.doySeriesByYear({
  imageCollection: vi_250m_filt, 
  region: conifer, 
  regionReducer: ee.Reducer.mean(), 
  bandName: 'NDVI', 
  scale: 250
}).setOptions({
  title: 'NDVI of Conifer by Year',
  vAxis: {
  title: 'NDVI',
  ticks: [0.00, 0.25, 0.50, 0.75, 1.00]}
})
print(ndvi_con_ts_byYear)

// Plot DOY NDVI for Urban
var ndvi_urb_ts_byYear = ui.Chart.image.doySeriesByYear({
  imageCollection: vi_250m_filt, 
  region: urban, 
  regionReducer: ee.Reducer.mean(), 
  bandName: 'NDVI', 
  scale: 250
}).setOptions({
  title: 'NDVI of Urban by Year',
  vAxis: {
  title: 'NDVI',
  ticks: [0.00, 0.25, 0.50, 0.75, 1.00]}
})
print(ndvi_urb_ts_byYear)

// Plot DOY NDVI for Oak
var ndvi_oak_ts_byYear = ui.Chart.image.doySeriesByYear({
  imageCollection: vi_250m_filt, 
  region: oak, 
  regionReducer: ee.Reducer.mean(), 
  bandName: 'NDVI', 
  scale: 250
}).setOptions({
  title: 'NDVI of Oak by Year',
  vAxis: {
  title: 'NDVI',
  ticks: [0.00, 0.25, 0.50, 0.75, 1.00]}
})
print(ndvi_oak_ts_byYear)

// Plot the median for different landcovers
var ndvi_med_ts = ui.Chart.image.seriesByRegion({
  imageCollection: ndvi_250m_WY13to15_DOYmed, 
  regions: [conifer, urban, grass, oak], 
  reducer: ee.Reducer.mean(),
  scale: 250,
  xProperty: 'DOY'
}).setOptions({
  title: 'NDVI Median of Different Landcover',
  vAxis: {
  title: 'NDVI',
  ticks: [0.00, 0.25, 0.50, 0.75, 1.00]}
})
print(ndvi_med_ts)



/*-----------------------/
/ ----- DATA EXPORT ---- /
/-----------------------*/
var export_crs = 'EPSG:32610'
var export_crs_suffix = '-EPSG32610'

//NDVI min
Export.image.toDrive({
  image: ndvi_250m_ann_min, 
  description: 'ndvi_232m_ann_min',
  fileNamePrefix: 'ndvi_232m_ann_min' + export_crs_suffix, 
  region: bigClip, 
  scale: 231.65635826395828,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_500m_ann_min, 
  description: 'ndvi_463m_ann_min',
  fileNamePrefix: 'ndvi_463m_ann_min' + export_crs_suffix, 
  region: bigClip, 
  scale: 463.3127165275,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_1000m_ann_min, 
  description: 'ndvi_927m_ann_min',
  fileNamePrefix: 'ndvi_927m_ann_min' + export_crs_suffix, 
  region: bigClip, 
  scale: 926.6254330555,  
  crs: export_crs,
  maxPixels: 10000000000})

//NDVI 5th percentile
Export.image.toDrive({
  image: ndvi_250m_ann_05p, 
  description: 'ndvi_232m_ann_05p',
  fileNamePrefix: 'ndvi_232m_ann_05p' + export_crs_suffix, 
  region: bigClip, 
  scale: 231.65635826395828,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_500m_ann_05p, 
  description: 'ndvi_463m_ann_05p',
  fileNamePrefix: 'ndvi_463m_ann_05p' + export_crs_suffix, 
  region: bigClip, 
  scale: 463.3127165275,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_1000m_ann_05p, 
  description: 'ndvi_927m_ann_05p',
  fileNamePrefix: 'ndvi_927m_ann_05p' + export_crs_suffix, 
  region: bigClip, 
  scale: 926.6254330555,  
  crs: export_crs,
  maxPixels: 10000000000})

//NDVI 50th percentile (median)
Export.image.toDrive({
  image: ndvi_250m_ann_med, 
  description: 'ndvi_232m_ann_med',
  fileNamePrefix: 'ndvi_232m_ann_med' + export_crs_suffix, 
  region: bigClip, 
  scale: 231.65635826395828,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_500m_ann_med, 
  description: 'ndvi_463m_ann_med',
  fileNamePrefix: 'ndvi_463m_ann_med' + export_crs_suffix, 
  region: bigClip, 
  scale: 463.3127165275,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_1000m_ann_med, 
  description: 'ndvi_927m_ann_med',
  fileNamePrefix: 'ndvi_927m_ann_med' + export_crs_suffix, 
  region: bigClip, 
  scale: 926.6254330555,  
  crs: export_crs,
  maxPixels: 10000000000})

//NDVI 95th percentile
Export.image.toDrive({
  image: ndvi_250m_ann_95p, 
  description: 'ndvi_232m_ann_95p',
  fileNamePrefix: 'ndvi_232m_ann_95p' + export_crs_suffix, 
  region: bigClip, 
  scale: 231.65635826395828,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_500m_ann_95p, 
  description: 'ndvi_463m_ann_95p',
  fileNamePrefix: 'ndvi_463m_ann_95p' + export_crs_suffix, 
  region: bigClip, 
  scale: 463.3127165275,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_1000m_ann_95p, 
  description: 'ndvi_927m_ann_95p',
  fileNamePrefix: 'ndvi_927m_ann_95p' + export_crs_suffix, 
  region: bigClip, 
  scale: 926.6254330555,  
  crs: export_crs,
  maxPixels: 10000000000})
  
//NDVI Seasonal Difference
Export.image.toDrive({
  image: ndvi_250m_seas_diff, 
  description: 'ndvi_232m_seas_diff',
  fileNamePrefix: 'ndvi_232m_seas_diff' + export_crs_suffix, 
  region: bigClip, 
  scale: 231.65635826395828,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_500m_seas_diff, 
  description: 'ndvi_463m_seas_diff',
  fileNamePrefix: 'ndvi_463m_seas_diff' + export_crs_suffix, 
  region: bigClip, 
  scale: 463.3127165275,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_1000m_seas_diff, 
  description: 'ndvi_927m_seas_diff',
  fileNamePrefix: 'ndvi_927m_seas_diff' + export_crs_suffix, 
  region: bigClip, 
  scale: 926.6254330555,  
  crs: export_crs,
  maxPixels: 10000000000})
  
//NDVI Variance
Export.image.toDrive({
  image: ndvi_250m_var, 
  description: 'ndvi_232m_var',
  fileNamePrefix: 'ndvi_232m_var' + export_crs_suffix, 
  region: bigClip, 
  scale: 231.65635826395828,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_500m_var, 
  description: 'ndvi_463m_var',
  fileNamePrefix: 'ndvi_463m_var' + export_crs_suffix, 
  region: bigClip, 
  scale: 463.3127165275,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_1000m_var, 
  description: 'ndvi_927m_var',
  fileNamePrefix: 'ndvi_927m_var' + export_crs_suffix, 
  region: bigClip, 
  scale: 926.6254330555,  
  crs: export_crs,
  maxPixels: 10000000000})
  
//NDVI Cumulative
Export.image.toDrive({
  image: ndvi_250m_sum, 
  description: 'ndvi_232m_sum',
  fileNamePrefix: 'ndvi_232m_sum' + export_crs_suffix, 
  region: bigClip, 
  scale: 231.65635826395828,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_500m_sum, 
  description: 'ndvi_463m_sum',
  fileNamePrefix: 'ndvi_463m_sum' + export_crs_suffix, 
  region: bigClip, 
  scale: 463.3127165275,  
  crs: export_crs,
  maxPixels: 10000000000})
  
Export.image.toDrive({
  image: ndvi_1000m_sum, 
  description: 'ndvi_927m_sum',
  fileNamePrefix: 'ndvi_927m_sum' + export_crs_suffix, 
  region: bigClip, 
  scale: 926.6254330555,  
  crs: export_crs,
  maxPixels: 10000000000})