/* 
Title: GEDI Sonoma MODIS Landcover 

By: Patrick Burns, Northern Arizona University

About: Purpose is to reclassify MODIS LC map into two classes: veg. with structure and everything else

Refs.: 
  > ref: https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mcd12q1_v006

Last Updated: 15 Mar. 2019
*/

// ----- DATA IMPORT -----
// MODIS Landcover 500m
var modLC = ee.ImageCollection("MODIS/006/MCD12Q1");

// Get Landcover during period of interest (2013 to 2015)
var modLC1_2013to15 = modLC.filterDate('2013-01-01', '2015-12-31')
                           .select("LC_Type1")
print(modLC1_2013to15)

// Add individual years to map
Map.addLayer(modLC1_2013to15, {}, "MODIS LC1")
Map.addLayer(ee.Image("MODIS/006/MCD12Q1/2013_01_01"), {min: 1, max: 17}, "2013")
Map.addLayer(ee.Image("MODIS/006/MCD12Q1/2014_01_01"), {min: 1, max: 17}, "2014")
Map.addLayer(ee.Image("MODIS/006/MCD12Q1/2015_01_01"), {min: 1, max: 17}, "2015")

// Function to reclass Landcover (veg. with structure and everything else) 
var modLC1_remap = function(img){
  var yr = ee.String(img.date().get('year'))
  var recl = img.remap({
  from: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17],
  to:   [1,1,1,1,1,1,1,1,1, 1, 1, 0, 0, 1, 1, 0, 0]
})
return recl
}

// Apply the remap function over the years of interest
var modLC1_2013to15_remap = modLC1_2013to15.map(modLC1_remap)
print("Remapped: ", modLC1_2013to15_remap)

// Take the minimum value (0) over the three year period to be conservative
var modLC1_2013to15_remap_min = ee.Image(modLC1_2013to15_remap.min())
Map.addLayer(modLC1_2013to15_remap_min, {}, "MODIS LC1 Remap 2013-2015 Min")

// Add Sonoma county boundaries to map
var sonomaSHP = ee.FeatureCollection('users/pb463/S2L/Sonoma_cty_v2_PBcleaned')
Map.addLayer(sonomaSHP , 
            {color: "green"}, "Sonoma County")
Map.centerObject(sonomaSHP, 9)


// Calculate percent of "natural" pixels in Sonoma
var sonomaCount = modLC1_2013to15_remap_min.reduceRegion({
  reducer: ee.Reducer.count(), 
  geometry: sonomaSHP, 
  scale: 463.3127165275, 
  })
  
  var natFrac = modLC1_2013to15_remap_min.reduceRegion({
  reducer: ee.Reducer.sum(), 
  geometry: sonomaSHP, 
  scale: 463.3127165275, 
  })
  
print("Fraction of natural pixels", ee.Number(natFrac.get("remapped")).divide(ee.Number(sonomaCount.get("remapped"))))

//throw('stop')


// ----- EXPORT -----
// Add a geometry for processing and export
var bigClip = /* color: #d6cc2e */ee.Geometry.Polygon(
        [[[-123.9532470703125, 39.16835008003937],
          [-123.28857421875, 37.93114968513442],
          [-121.9537353515625, 37.9094828276104],
          [-122.530517578125, 39.155572476393786]]]);
//Map.addLayer(bigClip, {'color':'#d6cc2e'}, 'Clipping Shape')

// Specify CRS for export
var export_crs = 'EPSG:32610'
var export_crs_suffix = '-EPSG32610'

// // Export Landcover type 1 as is
// Export.image.toDrive({
//   image: modLC1_2014, 
//   description: 'modLC1_2014_463m',
//   fileNamePrefix: 'modLC1_2014_463m' + export_crs_suffix, 
//   region: bigClip, 
//   scale: 463.3127165275,  
//   crs: export_crs,
//   maxPixels: 10000000000})
  
// Export Landcover re-classed where urban = 0 and everything else = 1
Export.image.toDrive({
  image: modLC1_2013to15_remap_min, 
  description: 'modLC1_2013to15_remap_463m',
  fileNamePrefix: 'modLC1_2013to15_remap_463m' + export_crs_suffix, 
  region: bigClip, 
  scale: 463.3127165275,  
  crs: export_crs,
  maxPixels: 10000000000})