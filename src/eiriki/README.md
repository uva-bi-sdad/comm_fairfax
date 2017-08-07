eirik source folder

01_GIS_youth_survey.R 
  This script is THE FOUNDATION for all further work in this folder. Takes GIS information about zip code from virginia and converts 
  it to the FCPS High School Pyramid level. This is how we can make heat maps on FCPS youth survey data. This code is copied and pasted into
  most of the other scripts
  
02_GIS_map_overlays.R
  Overlays parks in Fairfax county with the depression heatmap

03_Multiple_Heatmaps.R 
  Ended up not using these. Did heatmaps on youth survey variables thought to correlate with depression. Put four heatmaps on one page which       looked really bad

05_more_overlays.R 
  Ended up not using these. Same as script 03, but had overlays of mental health service providers.

07_fix_heatmap.R 
  Use this for reproducing figures. We chose different variables to compare to depression and attached a bar graph to the side of the heatmap
  to achieve better visualization. 
  
08_region_bars.R
  Aggregated depression on the Region level (high school pyramids grouped by geographic location: determined by FCPS). Did this to get an idea of   what fcps might want to do on geographic group level
  
09_heat_to_heat.R 
  Ended up not using these. Did side by side comparisons of the heatmaps.
