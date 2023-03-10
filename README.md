# Barrier Interaction Behaviour Analysis

MoveApps 

Github repository: https://github.com/nilanjanchatterjee/Barrier_Interaction_Bahaviour_Analysis

## Description
The app assesses animal encounters with linear features (roads, rail tracks, barriers, fences, etc.) by identifying and classifying different movement behaviours that occur near a set of input features. It requires a user-specified buffer distance and time intervals to specify how different behaviours are calculated. Linear features for the assessment can be provided as a shapefile, otherwise a roads dataset for the Yellowstone-to-Yukon region is used. The app is based on the [BaBA package](https://github.com/wx-ecology/BaBA/) described in [Xu et al. 2021](https://doi.org/10.1111/1365-2664.13806). 

## Documentation
Animals behave in a variety of ways when they encounter linear features and barriers. The app classifies behaviours near linear features based on the changes in movement. The identified behaviours can be classified into three broad classes: **Usual movement**, **Altered movement** and **Trapped**: 
* **Usual movement** consists of 'Average movement' and 'Quick cross'; 
* **Altered movement** consists of 'Bounce', 'Back and Forth' and 'Trace'; and
* **Trapped** is signified when the animal movement is restricted within a close vicinity of the feature for a significant time. 

For more details about the behaviour classes, please review the paper describing the BaBA app (Xu et al. 2021)[https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.13806].

### Input data
*move/moveStack* in Movebank format
*Linear feature layer* in shapefile (.shp) format

### Output data
*MoveStack* in Movebank format    

### Artefacts
`Encounter_event_data.csv`: Details of the identified behaviours, including the following attributes:  

Individual_ID: the animal ID

burstID: an identifier for the burst of events associated with the encounter

long and lat: the coordinates of the first location in the encounter (WGS84)

start_time and end_time: the timestamps associated with the beginning and end of the encounter (format `yyyy-MM-dd HH:mm:ss.SSS` in UTC)

duration: the duration of the encounter (in hours)

cross: the number of feature crossings during the encounter

straightness: The straightness of travel over a period around the encounter. This is an index (value range 0-1) calculated as D/L, where D is the straightline distance between the first and last location fixes, and L is the distance between all location fixes, over this period (Batschelet 1981, *Circular statistics in biology*).

eventTYPE: the type of encounter behaviour (e.g., Bounce, TBD, Trapped, unknown, Quick_Cross)


`Encounter_data.csv`: Details of the road encounters, including the following attributes:  

burstID: an identifier for the burst of events associated with the encounter

trackId: the animal ID

tmestamp: the timestamp associated with the start of the encounter (format `yyyy-MM-dd HH:mm:ss.SSS` in UTC)

eventTYPE: the type of encounter behaviour (e.g., Bounce, TBD, Trapped, unknown, Quick_Cross)

geometry: the coordinate geometry of the first location in the encounter (format `c(-long, lat)` in WGS84)


`Event_plot_output.pdf`: Document with plots of each identified encounter. Plots include a label with the burstID and the identified behaviour for each encounter, the features (red line), buffer area (grey), the animal locations (blue dot), and lines between consecutive animal locations (black line).  

### Settings
**Distance buffer (in meters) (`buffer`):** Buffer distance between the animal and the feature to use for evaluating the effect of the linear feature/barrier. Unit: `metres`. Default: 500.    
**Maximum time for short encounter events (in hours) (`b_time`):** Maximum duration, that an encounter event would be considered as a short *bounce* or *quick cross* event. Unit: `hours`. Default: 4.   
**Minimum time for 'trapped' encounter events (in hours) (`p_time`):** Minimum duration, that an encounter event would be considered as a *trapped* condition. Unit: `hours`. Default: 36.   
**Buffer time around encounter events (in hours) (`w`):** The length of time, to include around the encounter event to calculate average movement straightness using a moving window. Locations included are all locations within `w/2` before the first location of the encounter event and `w/2` after the last location of the event. Unit: `hours`. Default: 72.
**Feature shapefile (`barrier_files`):** Optionally, upload a shapefile containing linear features to use for the barrier intersection behaviour analysis. Fallback road files are provided, but they span only the Yukon to Yellowstone Region (Y2Y) (extracted from the [GRIP global roads database](https://www.globio.info/download-grip-dataset)). Requirements for user-provided shapefiles are the following: 
* The linear feature data must overlap with the tracking data. 
* The shapefile must be in WGS84 projection (lat-long coordinates using [ESPG 4326](https://spatialreference.org/ref/epsg/wgs-84/))! 
* The App requires the following files, named exactly as given: 1. `roads.cpg`, 2. `roads.dbf`, 3. `roads.prj`, 4. `roads.shp`, 5. `roads.shx`.  

### Null or error handling
*The app contains a road shapefile from the Y2Y region but users can upload their own shapefiles also. Please be careful that the projection of the barrier feature shapefile should be lat-long (epsg 4326). Moreover, the identified behaviours are function of the user specified input (buffer and time), please be careful and use time intervals with respect to the fix-intervals.*

*Example* : **Parameter `b_time`**:  should not be smaller than the fix-intervals. If your data set has very different fix-intervals please create multiple workflows of individuals with similar fix-intervals.