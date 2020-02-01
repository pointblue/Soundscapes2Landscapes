// What - Define Met dataset of interest
var PRISM = ee.ImageCollection('OREGONSTATE/PRISM/AN81m')

// WHERE - Define polygon area of interest
var AOI = ee.FeatureCollection('users/pb463/S2L/Sonoma_cty_v2_PBcleaned')
var AOI_name = 'Sonoma'
print(AOI)

// WHEN - Define start and end dates
var date1 = '1895-01-01'
var date2 = '2017-12-31'

// Filter the datasets
var PRISM_filt = PRISM.filterBounds(AOI)
                      .filterDate(date1,date2)

print(PRISM_filt)

//Plot average monthly precip for the county
var monthlyPre = ui.Chart.image.series({
  imageCollection: PRISM_filt.select('ppt').filterDate(date1, date2),
  region: AOI,
  reducer: ee.Reducer.mean(),
  scale: 4000,
  xProperty: null
  })
  .setChartType('ColumnChart')
  .setOptions({
  title: 'Average Monthly PPT - Sonoma County',
  vAxis: {
    title: 'Precip (mm)',
    ticks: [0, 100, 200, 300, 400]
  },
  hAxis: {
    title: 'Year and Month'
  },
  lineWidth: 0,
  })

print(monthlyPre)

//Plot average monthly precip for the Santa Rosa airport
var monthlyPre_KSTS = ui.Chart.image.series({
  imageCollection: PRISM_filt.select('ppt').filterDate(date1, date2),
  region: KSTS,
  reducer: ee.Reducer.mean(),
  scale: 4000,
  xProperty: null
  })
  .setChartType('ColumnChart')
  .setOptions({
  title: 'Average Monthly PPT - KSTS Airport',
  vAxis: {
    title: 'Precip (mm)',
    ticks: [0, 100, 200, 300, 400]
  },
  hAxis: {
    title: 'Year and Month'
  },
  lineWidth: 0,
  })

print(monthlyPre_KSTS)

//Plot stdDev of monthly precip for the county
var monthlyPreVar = ui.Chart.image.series({
  imageCollection: PRISM_filt.select('ppt').filterDate(date1, date2),
  region: AOI,
  reducer: ee.Reducer.stdDev(),
  scale: 4000,
  xProperty: null,
  })
  .setOptions({
  title: 'StdDev Monthly PPT',
  vAxis: {
    title: 'Precip StdDev (mm)',
    ticks: [0, 100, 200]
  },
  hAxis: {
    title: 'Year and Month'
  },
  lineWidth: 0,
  pointSize: 4
  })

print(monthlyPreVar)

//Plot average monthly temp for the county
var monthlyTemp = ui.Chart.image.series({
  imageCollection: PRISM_filt.select(['tmean']).filterDate(date1, date2),
  region: AOI,
  reducer: ee.Reducer.mean(),
  scale: 4000,
  xProperty: null
  })
  .setOptions({
  title: 'Average Monthly Temperature - Sonoma County',
  vAxis: {
    title: 'Temp (deg. C)',
    ticks: [0, 5, 10, 15, 20, 25, 30]
  },
  hAxis: {
    title: 'Year and Month'
  },
  lineWidth: 2,
  })

print(monthlyTemp)

//Plot average monthly temp for Santa Rosa airport
var monthlyTemp_KSTS = ui.Chart.image.series({
  imageCollection: PRISM_filt.select(['tmean']).filterDate(date1, date2),
  region: KSTS,
  reducer: ee.Reducer.mean(),
  scale: 4000,
  xProperty: null
  })
  .setOptions({
  title: 'Average Monthly Temperature - KSTS',
  vAxis: {
    title: 'Temp (deg. C)',
    ticks: [0, 5, 10, 15, 20, 25, 30]
  },
  hAxis: {
    title: 'Year and Month'
  },
  lineWidth: 2,
  })

print(monthlyTemp_KSTS)


//Plot average monthly temp variation for the county
var monthlyTempVar = ui.Chart.image.series({
  imageCollection: PRISM_filt.select(['tmin', 'tmax']).filterDate(date1, date2),
  region: AOI,
  reducer: ee.Reducer.stdDev(),
  scale: 4000,
  xProperty: null
  })
  .setOptions({
  title: 'StdDev Monthly Temps',
  vAxis: {
    title: 'Temp StdDev (deg. C)',
    ticks: [0, 1, 2, 3, 4, 5]
  },
  hAxis: {
    title: 'Year and Month'
  },
  lineWidth: 2,
  })

print(monthlyTempVar)
// PRISM Maps
// PRISM variable category - Precipitation
// PRISM Variable
var PRISMvarP = 'ppt'
var pptVisOn = 0 // Should these layers be on or off? on the Map below

// Specify visualization Min and Max (Red to blue rainbow color palette)
var PminVis = 0
var PmaxVis = 250

Map.centerObject(AOI)

// Mean
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(1,1,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Jan Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(2,2,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Feb Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(3,3,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Mar Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(4,4,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Apr Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(5,5,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM May Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(6,6,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Jun Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(7,7,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Jul Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(8,8,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Aug Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(9,9,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Sep Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(10,10,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Oct Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(11,11,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Nov Mean ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(12,12,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_mean"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Dec Mean ' + PRISMvarP, pptVisOn)

//StdDev
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(1,1,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Jan stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(2,2,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Feb stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(3,3,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Mar stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(4,4,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Apr stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(5,5,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM May stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(6,6,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Jun stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(7,7,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Jul stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(8,8,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Aug stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(9,9,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Sep stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(10,10,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Oct stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(11,11,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Nov stdDev ' + PRISMvarP, pptVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(12,12,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarP + "_stdDev"],"min": PminVis, "max":PmaxVis,"palette":["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"]}, 'PRISM Dec stdDev ' + PRISMvarP, pptVisOn)

// PRISM variable category - Temperature
// PRISM Variable
var PRISMvarT = 'tmax'
var tempVisOn = 0 // Should these layers be on or off on the Map below?

// Specify visualization Min and Max (Blue to red rainbow color palette)
var TminVis = 0
var TmaxVis = 30
var stdevVisFac = 10

// Mean
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(1,1,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Jan Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(2,2,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Feb Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(3,3,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Mar Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(4,4,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Apr Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(5,5,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM May Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(6,6,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Jun Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(7,7,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Jul Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(8,8,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Aug Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(9,9,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Sep Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(10,10,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Oct Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(11,11,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Nov Mean ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(12,12,'month')).reduce(ee.Reducer.mean())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_mean"],"min": TminVis, "max":TmaxVis,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Dec Mean ' + PRISMvarT, tempVisOn)

//StdDev
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(1,1,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Jan stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(2,2,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Feb stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(3,3,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Mar stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(4,4,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Apr stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(5,5,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM May stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(6,6,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Jun stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(7,7,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Jul stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(8,8,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Aug stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(9,9,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Sep stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(10,10,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Oct stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(11,11,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Nov stdDev ' + PRISMvarT, tempVisOn)
Map.addLayer(ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(12,12,'month')).reduce(ee.Reducer.stdDev())).clip(AOI), {"opacity":1,"bands":[PRISMvarT + "_stdDev"],"min": TminVis/stdevVisFac, "max":TmaxVis/stdevVisFac,"palette":["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"]}, 'PRISM Dec stdDev ' + PRISMvarT, tempVisOn)


// Make an image collection of Mean and StdDev
// First define dictionaries for visualizing single band images as RGB
var visDict_ppt_mean = {
  bands: 'ppt_mean', min: PminVis, max: PmaxVis, palette: ["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"], forceRgbOutput: true
}

var visDict_ppt_stdDev = {
  bands: 'ppt_stdDev', min: PminVis, max: PmaxVis, palette: ["000000","ff0000","ff9400","f5ff1e","22ff00","2afcff","0315ff","a800ff","ffffff"], forceRgbOutput: true
}

var visDict_tmin_mean = {
  bands: 'tmin_mean', min: TminVis, max: TmaxVis, palette: ["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"], forceRgbOutput: true
}

var visDict_tmax_mean = {
  bands: 'tmax_mean', min: TminVis, max: TmaxVis, palette: ["000000","032cff","09e5ff","22ff0d","e9ff0f","ffad03","ff0000","ffffff"], forceRgbOutput: true
}

// Make the image collections
var PRISM_filt_ppt_mean = ee.ImageCollection.fromImages([
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(1,1,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(2,2,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(3,3,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(4,4,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(5,5,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(6,6,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(7,7,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(8,8,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(9,9,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(10,10,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(11,11,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(12,12,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('ppt_mean').visualize(visDict_ppt_mean)
  ])

var PRISM_filt_ppt_stdDev = ee.ImageCollection.fromImages([
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(1,1,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(2,2,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(3,3,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(4,4,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(5,5,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(6,6,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(7,7,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(8,8,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(9,9,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(10,10,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(11,11,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(12,12,'month')).reduce(ee.Reducer.stdDev())).clip(AOI).select('ppt_stdDev').visualize(visDict_ppt_stdDev)
])

var PRISM_filt_tmin_mean = ee.ImageCollection.fromImages([
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(1,1,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(2,2,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(3,3,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(4,4,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(5,5,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(6,6,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(7,7,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(8,8,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(9,9,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(10,10,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(11,11,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(12,12,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmin_mean').visualize(visDict_tmin_mean)
  ])

var PRISM_filt_tmax_mean = ee.ImageCollection.fromImages([
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(1,1,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(2,2,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(3,3,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(4,4,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(5,5,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(6,6,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(7,7,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(8,8,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(9,9,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(10,10,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(11,11,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean),
  ee.Image(PRISM_filt.filter(ee.Filter.calendarRange(12,12,'month')).reduce(ee.Reducer.mean())).clip(AOI).select('tmax_mean').visualize(visDict_tmax_mean)
  ])




// Export Visualizations as Videos to Google Drive
// Export ppt_mean video
Export.video.toDrive({
  collection: PRISM_filt_ppt_mean, 
  description: 'PRISM_Mean_Monthly_Precip_WY2006to2015', 
  folder: 'GEE_Exports', 
  fileNamePrefix: 'PRISM_Sonoma_ppt_mean_WY2006to2015', 
  framesPerSecond: 1, 
  dimensions: "1920x1080", 
  region: AOI, 
  scale: 500, 
  crs: 'EPSG:4326', 
  maxPixels: 10000000
})

// Export ppt_stdDev video
Export.video.toDrive({
  collection: PRISM_filt_ppt_stdDev, 
  description: 'PRISM_stdDev_Monthly_Precip_WY2006to2015', 
  folder: 'GEE_Exports', 
  fileNamePrefix: 'PRISM_Sonoma_ppt_stdDev_WY2006to2015', 
  framesPerSecond: 1, 
  dimensions: "1920x1080", 
  region: AOI, 
  scale: 500, 
  crs: 'EPSG:4326', 
  maxPixels: 10000000
})

// Export tmin_mean video
Export.video.toDrive({
  collection: PRISM_filt_tmin_mean, 
  description: 'PRISM_Mean_Monthly_Tmin_WY2006to2015', 
  folder: 'GEE_Exports', 
  fileNamePrefix: 'PRISM_Sonoma_tmin_mean_WY2006to2015', 
  framesPerSecond: 1, 
  dimensions: "1920x1080", 
  region: AOI, 
  scale: 500, 
  crs: 'EPSG:4326', 
  maxPixels: 10000000
})

// Export tmax_mean video
Export.video.toDrive({
  collection: PRISM_filt_tmax_mean, 
  description: 'PRISM_Mean_Monthly_Tmax_WY2006to2015', 
  folder: 'GEE_Exports', 
  fileNamePrefix: 'PRISM_Sonoma_tmax_mean_WY2006to2015', 
  framesPerSecond: 1, 
  dimensions: "1920x1080", 
  region: AOI, 
  scale: 500, 
  crs: 'EPSG:4326', 
  maxPixels: 10000000
})