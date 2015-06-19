# Identification of the hydrometereological events that caused the most harmful effects and had the greatest economic consequences

***

# Synopsis
We analyzed the U.S. National Oceanic and Atmospheric Administration's storm database from the past 60 years. Our aim was to identify the hydrometereological events that caused the most harmful effects for human health (injuries and fatalities) and had the greatest economic consequences in terms of property damage. We found that the most harmful metereological event was the tornado, which caused over 90,000 direct injuries in the last 60 years. Similarly, most deadly events were caused also by tornadoes, with about 5,600 deaths during the evaluated period of time. Finally, flooding had the greatest economic consequences, with over 150 billion dollars in property damages.

***

# Data Processing
In this section we describe (in words and code) how the data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database was loaded into R and processed for analysis. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The events in the database start in the year 1950 and end in November 2011. Data processing and analysis was done using R version 3.2.0 "Full of Ingredients" (R Foundation for Statistical Computing, Vienna, Austria).


```r
library(knitr)
opts_chunk$set(message = FALSE, fig.width = 9)
library(R.utils)
library(readr)
library(dplyr)
library(stringr)
library(ggvis)
```

We first downloaded the NOAA dataset from the Reproducible Research course on Coursera's web site. Along with the dataset, 2 more files were downloaded, indicating how the variables in the dataset are defined: the National Weather Service Storm Data Documentation (referenced here as the __NWS Manual__) and the National Climatic Data Center Storm Events FAQ. We finally unzipped the `Storm_Data.bz2` dataset and saved the unzipped dataset to a file named `Storm_Data.csv` in the working directory. We then loaded the dataset into a data frame named `Data` and selected the variables to be used.


```r
# Data file
URL_data <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(URL_data, "Files/Storm_Data.bz2", method = "wget")
# Storm data documentation
URL_Manual <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"
download.file(URL_Manual, "Files/NMS_Manual.pdf", method = "wget")
# FAQ
URL_FAQ <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf"
download.file(URL_FAQ, "Files/FAQ.pdf", method = "wget")
# Unzipping the dataset
bunzip2("Files/Storm_Data.bz2", "Files/Storm_Data.csv")
# Loading the dataset
Data <- read_csv("Files/Storm_Data.csv")
# Selecting the variables
Data <- Data %>%
  select(EVTYPE, PROPDMGEXP, PROPDMG, FATALITIES, INJURIES)
```

The major challenge we had for the analysis was the careless report of the events into the `EVTYPE` variable. Indeed, the __NWS Manual__ specifies 48 events (page 6) while we found 985 levels in `EVTYPE`. So, our first task was to recode these 985 levels of `EVTYPE` into the predefined 48 events, using a combination of string replacements and regular expressions. When it was not possible to assign a predefined event to a particular level we coded this particular level as `NA`. We finally filter the dataset to exclude rows with `NA` values and transform `EVTYPE` to factor.


```r
source("Files/Fix_EVTYPE.R")
Data <- Data %>%
  filter(EVTYPE != "NA")
```

The final R script used for recoding, as indicated in the previous R chunk, is available [here](https://github.com/alcideschaux/NOAA-StormData/blob/master/Files/Fix_EVTYPE.R).

We also noted similar inconsistencies in the `PROPDMGEXP` variable, with only 3 predefined levels (__NWS Manual__, page 12) and 18 levels in the downloaded dataset. So, our second task was to filter and recode `PROPDMGEXP` to include only the predefined levels. For this, we filter to keep only those levels that made sense. i.e., `m`, `M`, `K`, and `B`. We then replaced these string values for the corresponding numerical values. Finally, for estimating the total economic damage we combined `PROPDMG`and `PROPDMGEXP`, creating a new variable `PROPDMGTOTAL`:


```r
Data <- Data %>%
  filter(PROPDMGEXP %in% c("m", "M", "K", "B"))
Data$PROPDMGEXP <- Data$PROPDMGEXP %>%
  plyr::revalue(c("m" = 1e+6, "M" = 1e+6, "K" = 1e+3, "B" = 1e+9)) %>%
  as.numeric()
Data <- Data %>%
  mutate(PROPDMGTOTAL = PROPDMG * PROPDMGEXP)
```

This is the dataset we used for the data analysis. All datasets, scripts and associated files are available [here](https://github.com/alcideschaux/NOAA-StormData/tree/master/Files).



***

# Results
We first summarized all number of harmful events (i.e., injuries and fatalities) and property damage estimates by event type. For this, The we created a summary table containing the following variables:

* EVTYPE: hydrometereological event, as indicated in the __NWS Manual__
* ALL_INJURIES: total number of injured people
* ALL_FATALITIES: total number of people who died as a direct consequence of the event
* ALL_PROPDMG: total estimate of the property damage (in dollar amounts)


```r
Harmful <- Data %>%
  group_by(EVTYPE) %>%
  summarize(ALL_INJURIES = sum(INJURIES),
            ALL_FATALITIES = sum(FATALITIES),
            ALL_PROPDMG = sum(PROPDMGTOTAL))
Harmful %>%
  mutate(ALL_PROPDMG = format(ALL_PROPDMG, big.mark = ",")) %>%
  kable(align = c("l", "c", "c", "c"))
```



EVTYPE                                  ALL_INJURIES    ALL_FATALITIES      ALL_PROPDMG   
-------------------------------------  --------------  ----------------  -----------------
Astronomical Low Tide                        0                0              9,745,000    
Avalanche                                    71              103             3,721,800    
Blizzard                                    779               70            664,913,950   
Coastal Flood                                6                6             449,682,060   
Cold/Wind Chill                             177              188            245,579,450   
Debris Flow                                  49               29            327,408,100   
Dense Fog                                   709               51            22,829,500    
Dense Smoke                                  0                0               100,000     
Drought                                      28               4            1,053,038,600  
Dust Devil                                   31               1               719,130     
Dust Storm                                  184               5              5,619,000    
Extreme Cold/Wind Chill                      0                0               755,000     
Flash Flood                                 1608             776          16,991,195,460  
Flood                                       6757             410          150,113,968,500 
Frost/Freeze                                2055              62           3,999,037,010  
Funnel Cloud                                 2                0               194,600     
Hail                                        705               32          17,619,950,720  
Heat                                        2208             435            20,125,750    
Heavy Rain                                  137               50           3,253,891,190  
Heavy Snow                                  876               75           1,027,749,740  
High Surf                                   178               81            101,510,500   
High Wind                                   1167             195           5,881,880,960  
Hurricane (Typhoon)                         1328             106          85,356,410,010  
Lakeshore Flood                              0                0              7,570,000    
Lightning                                   3825             410           7,365,530,370  
Marine High/Strong/Thunderstorm Wind         26               16             7,186,340    
Rip Current                                 157              216              163,000     
Seiche                                       0                0               980,000     
Sleet                                        0                0                  0        
Storm Surge/Tide                             13               24          47,965,474,000  
Strong Wind                                 265               84            188,401,740   
Thunderstorm Wind                           2746             169           4,494,356,940  
Tornado                                    90447             5588         56,941,932,180  
Tropical Depression                          0                0              1,737,000    
Tropical Storm                              380               56           7,714,390,550  
Tsunami                                     129               33            144,062,000   
Volcanic Ash                                 0                0               500,000     
Watersprout                                  71               5             61,235,200    
Wildfire                                    1320              79           8,496,628,500  
Winter Weather                              1438             144           6,776,307,750  

We then focused on answering the 2 main questions of this study.

***

## 1) Across the United States, which types of events are most harmful with respect to population health?
For answering this question we identified the events that had the 5 highest total number of injured people and total number of people who died as a direct consequence of the event.

### Events that injured people the most
The 5 most harmful events that caused injuries are shown in this table:


```r
Most_Injuries <- Harmful %>%
  select(EVTYPE, ALL_INJURIES) %>%
  arrange(desc(ALL_INJURIES)) %>%
  head(5)
Most_Injuries %>%
  mutate(ALL_INJURIES = format(ALL_INJURIES, big.mark = ",")) %>%
  kable(align = c("l", "c"))
```



EVTYPE               ALL_INJURIES 
------------------  --------------
Tornado                 90,447    
Flood                   6,757     
Lightning               3,825     
Thunderstorm Wind       2,746     
Heat                    2,208     

The following plot shows the previous results:


```r
Most_Injuries %>%
  ggvis(~EVTYPE, ~ALL_INJURIES) %>%
  layer_bars(fill = ~EVTYPE) %>%
  add_axis("y", title_offset = 60)
```

<!--html_preserve--><div id="plot_id620989000-container" class="ggvis-output-container">
<div id="plot_id620989000" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id620989000_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id620989000" data-renderer="svg">SVG</a>
 | 
<a id="plot_id620989000_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id620989000" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id620989000_download" class="ggvis-download" data-plot-id="plot_id620989000">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id620989000_spec = {
  "data": [
    {
      "name": ".0/group_by1/count2/stack3_flat",
      "format": {
        "type": "csv",
        "parse": {
          "stack_lwr_": "number",
          "stack_upr_": "number"
        }
      },
      "values": "\"EVTYPE\",\"x_\",\"stack_lwr_\",\"stack_upr_\"\n\"Flood\",\"Flood\",0,6757\n\"Heat\",\"Heat\",0,2208\n\"Lightning\",\"Lightning\",0,3825\n\"Thunderstorm Wind\",\"Thunderstorm Wind\",0,2746\n\"Tornado\",\"Tornado\",0,90447"
    },
    {
      "name": ".0/group_by1/count2/stack3",
      "source": ".0/group_by1/count2/stack3_flat",
      "transform": [
        {
          "type": "treefacet",
          "keys": [
            "data.EVTYPE"
          ]
        }
      ]
    },
    {
      "name": "scale/fill",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"Flood\"\n\"Heat\"\n\"Lightning\"\n\"Thunderstorm Wind\"\n\"Tornado\""
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"Flood\"\n\"Heat\"\n\"Lightning\"\n\"Thunderstorm Wind\"\n\"Tornado\""
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n0\n94969.35"
    }
  ],
  "scales": [
    {
      "name": "fill",
      "type": "ordinal",
      "domain": {
        "data": "scale/fill",
        "field": "data.domain"
      },
      "points": true,
      "sort": false,
      "range": "category10"
    },
    {
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "name": "x",
      "type": "ordinal",
      "points": false,
      "padding": 0.1,
      "sort": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "group",
      "from": {
        "data": ".0/group_by1/count2/stack3"
      },
      "marks": [
        {
          "type": "rect",
          "properties": {
            "update": {
              "stroke": {
                "value": "#000000"
              },
              "fill": {
                "scale": "fill",
                "field": "data.EVTYPE"
              },
              "x": {
                "scale": "x",
                "field": "data.x_"
              },
              "y": {
                "scale": "y",
                "field": "data.stack_lwr_"
              },
              "y2": {
                "scale": "y",
                "field": "data.stack_upr_"
              },
              "width": {
                "scale": "x",
                "band": true
              }
            },
            "ggvis": {
              "data": {
                "value": ".0/group_by1/count2/stack3"
              }
            }
          }
        }
      ]
    }
  ],
  "legends": [
    {
      "orient": "right",
      "fill": "fill",
      "title": "EVTYPE"
    }
  ],
  "axes": [
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "titleOffset": 60,
      "layer": "back",
      "grid": true,
      "title": "ALL_INJURIES"
    },
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "layer": "back",
      "grid": true,
      "title": "EVTYPE"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 864,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id620989000").parseSpec(plot_id620989000_spec);
</script><!--/html_preserve-->

### Events that killed people the most
The 5 most harmful events that caused fatalities are shown in this table:


```r
Most_Fatalities <- Harmful %>%
  select(EVTYPE, ALL_FATALITIES) %>%
  arrange(desc(ALL_FATALITIES)) %>%
  head(5)
Most_Fatalities %>%
  mutate(ALL_FATALITIES = format(ALL_FATALITIES, big.mark = ",")) %>%
  kable(align = c("l", "c"))
```



EVTYPE         ALL_FATALITIES 
------------  ----------------
Tornado            5,588      
Flash Flood         776       
Heat                435       
Flood               410       
Lightning           410       

The following plot shows the previous results:


```r
Most_Fatalities %>%
  ggvis(~EVTYPE, ~ALL_FATALITIES) %>%
  layer_bars(fill = ~EVTYPE) %>%
  add_axis("y", title_offset = 60)
```

<!--html_preserve--><div id="plot_id498635497-container" class="ggvis-output-container">
<div id="plot_id498635497" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id498635497_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id498635497" data-renderer="svg">SVG</a>
 | 
<a id="plot_id498635497_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id498635497" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id498635497_download" class="ggvis-download" data-plot-id="plot_id498635497">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id498635497_spec = {
  "data": [
    {
      "name": ".0/group_by1/count2/stack3_flat",
      "format": {
        "type": "csv",
        "parse": {
          "stack_lwr_": "number",
          "stack_upr_": "number"
        }
      },
      "values": "\"EVTYPE\",\"x_\",\"stack_lwr_\",\"stack_upr_\"\n\"Flash Flood\",\"Flash Flood\",0,776\n\"Flood\",\"Flood\",0,410\n\"Heat\",\"Heat\",0,435\n\"Lightning\",\"Lightning\",0,410\n\"Tornado\",\"Tornado\",0,5588"
    },
    {
      "name": ".0/group_by1/count2/stack3",
      "source": ".0/group_by1/count2/stack3_flat",
      "transform": [
        {
          "type": "treefacet",
          "keys": [
            "data.EVTYPE"
          ]
        }
      ]
    },
    {
      "name": "scale/fill",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"Flash Flood\"\n\"Flood\"\n\"Heat\"\n\"Lightning\"\n\"Tornado\""
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"Flash Flood\"\n\"Flood\"\n\"Heat\"\n\"Lightning\"\n\"Tornado\""
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n0\n5867.4"
    }
  ],
  "scales": [
    {
      "name": "fill",
      "type": "ordinal",
      "domain": {
        "data": "scale/fill",
        "field": "data.domain"
      },
      "points": true,
      "sort": false,
      "range": "category10"
    },
    {
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "name": "x",
      "type": "ordinal",
      "points": false,
      "padding": 0.1,
      "sort": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "group",
      "from": {
        "data": ".0/group_by1/count2/stack3"
      },
      "marks": [
        {
          "type": "rect",
          "properties": {
            "update": {
              "stroke": {
                "value": "#000000"
              },
              "fill": {
                "scale": "fill",
                "field": "data.EVTYPE"
              },
              "x": {
                "scale": "x",
                "field": "data.x_"
              },
              "y": {
                "scale": "y",
                "field": "data.stack_lwr_"
              },
              "y2": {
                "scale": "y",
                "field": "data.stack_upr_"
              },
              "width": {
                "scale": "x",
                "band": true
              }
            },
            "ggvis": {
              "data": {
                "value": ".0/group_by1/count2/stack3"
              }
            }
          }
        }
      ]
    }
  ],
  "legends": [
    {
      "orient": "right",
      "fill": "fill",
      "title": "EVTYPE"
    }
  ],
  "axes": [
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "titleOffset": 60,
      "layer": "back",
      "grid": true,
      "title": "ALL_FATALITIES"
    },
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "layer": "back",
      "grid": true,
      "title": "EVTYPE"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 864,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id498635497").parseSpec(plot_id498635497_spec);
</script><!--/html_preserve-->

***

## 2) Across the United States, which types of events have the greatest economic consequences?
For answering this question we identified the events that had the 5 highest property damage costs as a consequence of the event, as shown in this table:


```r
Most_Damage <- Harmful %>%
  select(EVTYPE, ALL_PROPDMG) %>%
  arrange(desc(ALL_PROPDMG)) %>%
  head(5)
Most_Damage %>%
  mutate(ALL_PROPDMG = format(ALL_PROPDMG, big.mark = ",")) %>%
  kable(align = c("l", "c"))
```



EVTYPE                   ALL_PROPDMG   
--------------------  -----------------
Flood                  150,113,968,500 
Hurricane (Typhoon)    85,356,410,010  
Tornado                56,941,932,180  
Storm Surge/Tide       47,965,474,000  
Hail                   17,619,950,720  

The following plot shows the previous results:


```r
Most_Damage %>%
  ggvis(~EVTYPE, ~ALL_PROPDMG) %>%
  layer_bars(fill = ~EVTYPE) %>%
  add_axis("y", title_offset = 120)
```

<!--html_preserve--><div id="plot_id240802757-container" class="ggvis-output-container">
<div id="plot_id240802757" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id240802757_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id240802757" data-renderer="svg">SVG</a>
 | 
<a id="plot_id240802757_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id240802757" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id240802757_download" class="ggvis-download" data-plot-id="plot_id240802757">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id240802757_spec = {
  "data": [
    {
      "name": ".0/group_by1/count2/stack3_flat",
      "format": {
        "type": "csv",
        "parse": {
          "stack_lwr_": "number",
          "stack_upr_": "number"
        }
      },
      "values": "\"EVTYPE\",\"x_\",\"stack_lwr_\",\"stack_upr_\"\n\"Flood\",\"Flood\",0,150113968500\n\"Hail\",\"Hail\",0,17619950720\n\"Hurricane (Typhoon)\",\"Hurricane (Typhoon)\",0,85356410010\n\"Storm Surge/Tide\",\"Storm Surge/Tide\",0,47965474000\n\"Tornado\",\"Tornado\",0,56941932180"
    },
    {
      "name": ".0/group_by1/count2/stack3",
      "source": ".0/group_by1/count2/stack3_flat",
      "transform": [
        {
          "type": "treefacet",
          "keys": [
            "data.EVTYPE"
          ]
        }
      ]
    },
    {
      "name": "scale/fill",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"Flood\"\n\"Hail\"\n\"Hurricane (Typhoon)\"\n\"Storm Surge/Tide\"\n\"Tornado\""
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"Flood\"\n\"Hail\"\n\"Hurricane (Typhoon)\"\n\"Storm Surge/Tide\"\n\"Tornado\""
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n0\n157619666925"
    }
  ],
  "scales": [
    {
      "name": "fill",
      "type": "ordinal",
      "domain": {
        "data": "scale/fill",
        "field": "data.domain"
      },
      "points": true,
      "sort": false,
      "range": "category10"
    },
    {
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "name": "x",
      "type": "ordinal",
      "points": false,
      "padding": 0.1,
      "sort": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "group",
      "from": {
        "data": ".0/group_by1/count2/stack3"
      },
      "marks": [
        {
          "type": "rect",
          "properties": {
            "update": {
              "stroke": {
                "value": "#000000"
              },
              "fill": {
                "scale": "fill",
                "field": "data.EVTYPE"
              },
              "x": {
                "scale": "x",
                "field": "data.x_"
              },
              "y": {
                "scale": "y",
                "field": "data.stack_lwr_"
              },
              "y2": {
                "scale": "y",
                "field": "data.stack_upr_"
              },
              "width": {
                "scale": "x",
                "band": true
              }
            },
            "ggvis": {
              "data": {
                "value": ".0/group_by1/count2/stack3"
              }
            }
          }
        }
      ]
    }
  ],
  "legends": [
    {
      "orient": "right",
      "fill": "fill",
      "title": "EVTYPE"
    }
  ],
  "axes": [
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "titleOffset": 120,
      "layer": "back",
      "grid": true,
      "title": "ALL_PROPDMG"
    },
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "layer": "back",
      "grid": true,
      "title": "EVTYPE"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 864,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id240802757").parseSpec(plot_id240802757_spec);
</script><!--/html_preserve-->

***

# Conclusions
By far, the most harmful metereological event has been the tornado, which has caused over 90,000 injuries in the last 60 years. Flood, lighting, thunderstorm wind, and heat were among the most harmful events following tornadoes. Similarly, most deadly events were caused also by tornadoes, with about 5,600 deaths in the past 60 years. Flash flood, heat, flood, and lightning were among the most deadly events following tornadoes. Finally, flooding had the greatest economic consequences, with over 150 billion dollars in property damages. Other hydrometereological events that caused great property damage were hurricanes, tornadoes, storm surges/tides, and hail.
