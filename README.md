# Barrier Interaction Behaviour Analysis

MoveApps 

Github repository: https://github.com/nilanjanchatterjee/Barrier_Interaction_Bahaviour_Analysis

## Description
The app identifies and classifies different behaviors of animal encounters with linear features (road, rail tracks, barriers, fences etc.). It requires a user specified buffer distance, minimum and maximum time interval for identification of different behaviors. The app is based on the BaBA package (Xu et al. 2021) https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.13806 

## Documentation
Animals do not behave in the usual manner when they encounter linear features and barriers. The app identifies these different behavior classes based on the changes in movement. The identified behaviors can be classified into three broad classes **Usual movement**, **Altered movement** and **Trapped**. **Usual movement** consists of Average movement and Quick cross, **Altered movement** consists of Bounce, Back and Forth and Trace while **Trapped** is signified when the animal movement is restricted within a close vicinity of the feature for a significant time. For more details about the behavior classes please go through the manuscript on BaBA app by Xu et al. 2021 (https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.13806).

### Input data
*move/moveStack* in Movebank format
*Linear feature layers* in Shapefile(.shp) format

### Output data
*MoveStack* in Movebank format   
*Road encounter* in .csv format   

### Artefacts
- Encounter_event_data.csv: Details of the identified behaviours
- Encounter_data.csv : Details of the road encounters 
- Event_plot_output.pdf: Document of plot of all identified behaviors 


### Parameters 
buffer: Distance to evaluate the effect of the linear feature/Barrier. Unit: `metres`.    
b_time: Maximum duration, that an encounter event would be considered as a short event *bounce* or *quick cross*. Unit: `hours`   
p_time: Minimum duration, that an encounter event would be considered as a *trapped* condition. Unit: `hours`   
w: The length of time, to include around the encounter event to calculate average movement straightness using a moving window. Locations included are all locations within `w/2` before the first location of the encounter event and `w/2` after the last location of the event.

*Example:* `radius`: Defined radius the animal has to stay in for a given duration of time for it to be considered resting site. Unit: `metres`.

### Null or error handling
*Please indicate for each parameter as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if parameters are improperly set and any other important information that you find the user should be aware of.*

*Example:* **Parameter `radius`:** If no radius AND no duration are given, the input data set is returned with a warning. If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set. 
