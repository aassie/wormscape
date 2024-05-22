# README #

This is the development depository for Wormscape

# How to use:
This repository is composed of three scripts to be run successively:

  1. Worm_align.ijm - an ImageJ/Fiji plugin to mark worm and manipulate the picture to have them straightened
  2. AliProcess.ijm - a second ImageJ/Fiji plugin to extract intensity values of each fluorescent channel
  3. Intensity.Processing.Script.R - An R script to extra the information from the text file generated by the previous script and get everything ready for fancy R plotting.

# Worm_align.ijm

This script is a modified version of the  worm.aligner script by Hanneke Okkenhaug Original GitHub repository available [here](https://github.com/hannekeo/Worm-align) and published here [here](https://www.jove.com/t/61136)

Here is a condensed version:

1. Install the plugin in Fiji by going to `Plugin`>`Install` and selecting the file. The Worm_align plugin should then appear at the bottom of that menu
2. The script now automatically detect if there is a previous `Worm-align` run on the selected folder. You can start again from where you stopped or start from scratch. If you start from scratch it will overwrite the existing folder. 
3. A message box will pop up. **DO NOT** click on OK straight away! Select the *WIDTH* of a worm, then click on OK
4. A series of boxes will ask you to choose a LUT color for each channel you have. Make sure you use **Grayscales** for your brightfield channel.
5. Let the script work until the next message box pops up.
6. Again, **DO NOT** click on OK straight away! 
7. Trace a line along the *LENGTH*
8. If there is no worm on the openned picture, add a random, very small line; otherwise, the script will stop. Small segments are filtered out later with the R script.
9. Once you select a worm, press `Cmd + t` (Mac) or `Crtl + t` (windows) to add that worm to the marked list. A new little box will pop up and show you the coordinates of your worms. You can tick the `Show all` option to see which worms have a line and `labels` to see how many selections have been made.
10. Once all worms are selected, click ok on the message box, let the script work, and move to the next picture
11. Repeat steps 6-10 for all the pictures in your experiment

Here are some additional tips to go around the potential frustration of this script:

- The script will stop if you click too early (by reflex) on the `OK` button of one of the information panels.
- If the script stopped for some reason and you still have some pictures to process, you can restart the process.
  - The script now automatically detect if `Worm-align` ran on the selected folder before
  - If you want to restart at a specific picture, delete it's associated `_ROIs.zip` in the data folder.

# AliProcess.ijm

This script will process the previously marked worms' position to extract single-channel information and convert the gray intensity into a comparable, usable number.
If you messed up the loading on the previous step, bring back all raw files in the same folder.

To use the script, open the script with Fiji and click `Run`

It will ask you to:

1. Select the folder that contains the output of Worm_align.jim
2. Select the folder that contains the original raw pictures

Then let the script work; once done, you can close Fiji and move to R.

# wormscape.R

This final script gathers all the information generated in the previous step in usable R objects and has basic plots.
This step requires a Metadata file with the following required columns: `Well`, `Plate` and `ChannelColor`.


## Wormscape Output

At the end of this lengthy process, you will have an object called: `WormscapeResults`.
This is a list of 3 lists. Each list contain the data formatted differently, one channel per list.

- `Average.Fluorescence`: contains tibbles with the average fluorescence intensity and length per worm, has metadata
- `Profile.Fluorescence`: contains tibbles with the longitudinal fluorescence profile of each worm, has metadata
- `Matrix.Fluorescence` : contains tibbles of a long format matrix of the average pixel value of the worm fluorescence profile along the x and y axis (normalized in percent). Doesn't have metadata

Example:
If you want to access Average fluorescence of the first channel defined as Green in your `channel.fluorescence` object.
You can access it with either `WormscapeResults[[1]][[1]]` or `WormscapeResults$Average.Fluorescence$Green`

## Analyzing the data
In the example below, we will pretend to have analyzed an experiment with **two** plates and measured the **green** and **red** channels.

### General fluorescence data:
The first data frame is straightforward, giving an overview of the average fluorescence per worm. The average fluorescence here is the sum of the 16-bit gray values divided by the length in pixels of the worm. 

#### Plot fluorescences against each other:

The code below plots the green values against the red values and facet the data per plate

```{R}
WormscapeResults[[1]][[1]] %>%
  select(!ChannelColor) %>%
  rename(Gint=IntMean)  %>%
  left_join(WormscapeResults[[1]][[2]] %>%
              rename(Rint=IntMean) %>%
              select(!ChannelColor)) %>%
  ggplot(aes(x=Gint,y=Rint,col=Type))+
    geom_point()+
    facet_wrap(~Plate,scales = "free")
```

The code below plots the box plot for different colors per well and plates

```{R}
WormscapeResults[[1]][[1]] %>%
  rbind(WormscapeResults[[1]][[2]]) %>%
  ggplot(aes(x=well,y=IntMean,col=ChannelColor))+
    geom_boxplot()+
    facet_wrap(~Plate,scales = "free")
```


### Longitudinal fluorescence data:

TBD

### Heatmap fluorescence average:

TBD

### Fancy in-depth population analysis


# To do:

- Merge first and second script
- Implement automatic detection of worm
