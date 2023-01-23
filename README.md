# Barrier Interaction Behaviour Analysis*

MoveApps

Github repository: https://github.com/nilanjanchatterjee/Barrier_Interaction_Bahaviour_Analysis

## Description
*Enter here the short description of the App that might also be used when filling out the description when submitting the App to Moveapps. This text is directly presented to Users that look through the list of Apps when compiling Workflows.*

## Documentation
*Enter here a detailed description of your App. What is it intended to be used for. Which steps of analyses are performed and how. Please be explicit about any detail that is important for use and understanding of the App and its outcomes.*

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
w:

*Example:* `radius`: Defined radius the animal has to stay in for a given duration of time for it to be considered resting site. Unit: `metres`.

### Null or error handling
*Please indicate for each parameter as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if parameters are improperly set and any other important information that you find the user should be aware of.*

*Example:* **Parameter `radius`:** If no radius AND no duration are given, the input data set is returned with a warning. If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set. 
