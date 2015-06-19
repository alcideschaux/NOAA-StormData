Data$EVTYPE <- Data$EVTYPE %>%
  str_to_title() %>% # Converting string to title case
  str_trim() %>% # Trim whitespace from start and end of string
  str_replace("Summary.*", "NA") %>%
  str_replace("Avalanch?e", "Avalanche") %>%
  str_replace("Astronomical.*", "Astronomical Low Tide") %>%
  str_replace(".*Blizzard.*", "Blizzard") %>%
  str_replace(".*Snow.*", "Heavy Snow") %>%
  str_replace("Co[lo][ld].*", "Cold/Wind Chill") %>%
  str_replace(".*Coastal ?[Ff]lood.*", "Coastal Flood") %>%
  str_replace(".*Coastal ?[Ss].*", "Storm Surge/Tidal") %>% 
  str_replace("^Co?a?sta?l.*", "Coastal Flood") %>% 
  str_replace(".*Beach.*", "Coastal Flood") %>% 
  str_replace(".*Drought.*", "Drought") %>%
  str_replace(".*Dry.*", "Drought") %>%
  str_replace(".*Hail.*", "Hail") %>%
  str_replace(".*Heat.*", "Heat") %>%
  str_replace(".*Warm.*", "Heat") %>%
  str_replace(".*Dust Dev.*", "Dust Devil") %>%
  str_replace(".*Dust ?[Ss]t.*", "Dust Storm") %>%
  str_replace(".*Freez.*", "Frost/Freeze") %>%
  str_replace(".*Chil.*", "Cold/Wind Chill") %>%
  str_replace(".*Extreme Wind.*", "Extreme Cold/Wind Chill") %>%
  str_replace(".*Flash.*", "Flash Flood") %>%
  str_replace("^Funnel.*", "Funnel Cloud") %>%
  str_replace(".*Glaze.*", "Ice Storm") %>%
  str_replace(".*Heavy Rain.*", "Heavy Rain") %>%
  str_replace(".*Heavy Precip.*", "Heavy Rain") %>%
  str_replace(".*Heavy Show.*", "Heavy Rain") %>%
  str_replace(".*Rain.*", "Heavy Rain") %>% 
  str_replace(".*High  ?S[uw].*", "High Surf") %>%
  str_replace("High  ?Wind.*", "High Wind") %>%
  str_replace(".*Hurricane.*", "Hurricane (Typhoon)") %>%
  str_replace(".*Ice ?.?St.*", "Ice Storm") %>%
  str_replace(".*Ic[ey].*", "Frost/Freeze") %>%
  str_replace(".*Lands.*", "Debris Flow") %>%
  str_replace(".*Mud.*", "Debris Flow") %>%
  str_replace(".*Lig[hn]tn?ing.*", "Lightning") %>%
  str_replace(".*Thun?de.*", "Lightning") %>%
  str_replace("Th?ud?n?.ee?.*", "Thunderstorm Wind") %>%
  str_replace("Tstm.*", "Thunderstorm Wind") %>%
  str_replace("Torna?d.*", "Tornado") %>%
  str_replace("Tropical S.*", "Tropical Storm") %>%
  str_replace("Wild.*", "Wildfire") %>%
  str_replace("Winter S.*", "Winter Storm") %>%
  str_replace("Wint.*", "Winter Weather") %>%
  str_replace("^Wi?nd.*", "Strong Wind") %>%
  str_replace(".*Way?ter.*", "Watersprout") %>%
  str_replace("Volcanic.*", "Volcanic Ash") %>%
  str_replace("Urban.*", "Flash Flood") %>%
  str_replace("Marine.*", "Marine High/Strong/Thunderstorm Wind") %>%
  str_replace("^Storm.*", "Storm Surge/Tide") %>%
  str_replace("Record High.*", "Excessive Heat") %>%
  str_replace("Record Low.*", "Extreme Cold/Wind Chill") %>%
  str_replace("^Rip Current.*", "Rip Current") %>%
  str_replace("^Sma?l?l Stream.*", "Flash Flood") %>%
  str_replace("Saharan Dust", "Dust Storm") %>%
  str_replace("Blowing Dust", "Dust Storm") %>%
  str_replace("Blow-Out Tides?", "Frost/Freeze") %>%
  str_replace("Breakup.*", "Flood") %>%
  str_replace("Brush Fires?", "Wildfire") %>%
  str_replace("Downburst.*", "Thunderstorm Wind") %>%
  str_replace("Driest Month", "Excessive Heat") %>%
  str_replace("Dam .*", "Flash Flood") %>%
  str_replace("Drowning", "Flood") %>%
  str_replace("Erosion.*", "Coastal Flood") %>%
  str_replace("Excessive [PW].*", "Heavy Rain") %>%
  str_replace("^Flood.*", "Flood") %>%
  str_replace("Forest F.*", "Wildfire") %>%
  str_replace("Gradient Winds?", "High Winds") %>%
  str_replace("Grass F.*", "Wildfire") %>%
  str_replace("^Gust.*", "High Wind") %>%
  str_replace("Hazardous.*", "High Surf") %>%
  str_replace("Heavy Seas", "High Surf") %>%
  str_replace("Heavy Surf.*", "High Surf") %>%
  str_replace("Heavy Swe.*", "High Surf") %>%
  str_replace("High Seas", "High Surf") %>%
  str_replace("High Temp.*", "Excessive Heat") %>%
  str_replace("High [TW][ia][dv].*", "High Surf") %>%
  str_replace("Highway F.*", "Flood") %>%
  str_replace("Hot [PSW][ape].*", "Excessive Heat") %>%
  str_replace("^Typhoon", "Hurricane (Typhoon)") %>%
  str_replace("Hyperth.*", "Excessive Heat") %>%
  str_replace("Hypoth.*", "Extreme Cold/Wind Chill") %>%
  str_replace("^Extremely Wet", "Heavy Rain") %>%
  str_replace("\\?", "NA") %>%
  str_replace("Abnormally Wet", "NA") %>%
  str_replace("Apache County", "NA") %>%
  str_replace("Below Normal Precipitation", "NA") %>%
  str_replace("Heavy M.*", "NA") %>%
  str_replace("^Fog", "Dense Fog") %>%
  str_replace("^Vog", "Dense Fog") %>%
  str_replace("^Frost$", "Frost/Freeze") %>%
  str_replace("^Early.*", "Frost/Freeze") %>%
  str_replace("^First.*", "Frost/Freeze") %>%
  str_replace("^Excessive$", "NA") %>%
  str_replace("^High$", "NA") %>%
  str_replace("^High Winds$", "High Wind") %>%
  str_replace("^Lake Flood$", "Lakeshore Flood") %>%
  str_replace("^Large W.*", "Funnel Cloud") %>%
  str_replace("^Local F.*", "Flood") %>%
  str_replace("^Low Temp.*", "Extreme Cold/Wind Chill") %>%
  str_replace("^Major F.*", "Flood") %>%
  str_replace("^Metro S.*", "NA") %>%
  str_replace("^Microb.*", "Thunderstorm Wind") %>%
  str_replace("^Mild P.*", "NA") %>%
  str_replace("^Minor F.*", "Flood") %>%
  str_replace("^Mixed P.*", "NA") %>%
  str_replace("^Month.*", "NA") %>%
  str_replace("^None$", "NA") %>%
  str_replace("^Non ?-?.*", "Flood") %>%
  str_replace("^No.*", "NA") %>%
  str_replace("^Ot.*", "NA") %>%
  str_replace("^Pat.*", "Dense Fog") %>%
  str_replace("^Record P.*", "Heavy Rain") %>%
  str_replace("^Record T.*", "NA") %>%
  str_replace("^Re[dm].*", "NA") %>%
  str_replace("^Riv.*", "Flood") %>%
  str_replace("^Roc.*", "Debris Flow") %>%
  str_replace("^Rog.*", "NA") %>%
  str_replace("^Rot.*", "Funnel Cloud") %>%
  str_replace("^Rou.*", "High Surf") %>%
  str_replace("^Rur.*", "Flood") %>%
  str_replace("^Sev.*", "Thunderstorm Wind") %>%
  str_replace("^Sleet Storm$", "Sleet") %>%
  str_replace("^Smoke$", "Dense Smoke") %>%
  str_replace("^Southeast$", "NA") %>%
  str_replace("^Stre.*", "Flood") %>%
  str_replace("^Strong W.*", "Strong Wind") %>%
  str_replace("^Temp.*", "NA") %>%
  str_replace("^Tidal.*", "Flood") %>%
  str_replace("^Unseasonably H.*", "Excessive Heat") %>%
  str_replace("^Unseasonably W.*", "Heavy Rain") %>%
  str_replace("^Unseasonal L.*", "Extreme Cold/Wind Chill") %>%
  str_replace("^Wet Mi.*", "Thunderstorm Wind") %>%
  str_replace("^Wet [MWY].*", "Heavy Rain") %>%
  str_replace("^Whirl.*", "Thunderstorm Wind") %>%
  str_replace("^Wake L.*", "NA") %>%
  str_replace("^Wall C.*", "Funnel Cloud")
