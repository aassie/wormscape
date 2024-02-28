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
2. Once you start the plugin, a box asks if you are restarting a failed annotation run.
  Please take a look at the note below if you messed up or if the script was interrupted on your previous run. But if you did click `yes` 
3. A message box will pop up. Do not click on ok straight away! Select the width of a worm, then click on OK
4. A series of boxes will ask you to adjust the contrast/Brightness of the pictures. Adjust as you see fit, but I advise you to keep everything the same. Proper exposure time should have been selected at the microscope. Note down the brightness contrast values in case it crashes (see below).
5. Once You have adjusted (or have not) the pictures, let the script work until the next message box pops up
6. Again, wait to click on ok until you have selected all your worms !!!
7. If there is no worm in the well you are going over, you should still add a random, very small line; otherwise, the script will stop. Small segments are filtered out later with the R script.
8. Once you select a worm, press `Cmd + t` (Mac) or `Crtl + t` (windows) to add that worm to the marked list. A new little box will pop up and show you the coordinates of your worms. You can tick the `Show all` option to see which worms have a line and `labels` to see how many selections have been made.
9. Once all worms are selected, click ok on the message box, let the script work, and move to the next picture
10. Repeat steps 6-9 for all the pictures in your experiment

Here are some additional tips to go around the potential frustration of this script:

- The script will stop if you click too early (by reflex) on the `OK` button of one of the information panels.
- If the script stopped for some reason and you still have some pictures to process, you can restart the process. 
  - Note down which picture you were processing. 
  - If you already traced some line over worms on the picture you were processing save the ROI position as a text file in the roi panel.
  - Move the pictures you have already processed from the raw picture folder into a different one (Like a `Done` folder). 
  - Rerun the script, and on the first dialogue panel, click `Yes`. It will reopen your first picture for brightness adjustment and worm length determination.
  - Make sure that the min/max intensity values used for the initial adjustment steps are the same.
  - The first picture will close and restart at your last picture. 
  - If you had ROIs saved as a text file, you can reload them through the ROI panel and continue.

# AliProcess.ijm

This script will process the previously marked worms' position to extract single-channel information and convert the gray intensity into a comparable, usable number.
If you messed up the loading on the previous step, bring back all raw files in the same folder.

To use the script, open the script with Fiji and click `Run`

It will ask you to:

1. Select the folder that contains the output of Worm_align.jim
2. Select the folder that contains the original raw pictures

Then let the script work, once done you can close Fiji and move to R.

# Intensity.Processing.Script.R

This final script gathers all the information generated in the previous step in usable R objects and has basic plots

Open the script in R and follow the instructions.
