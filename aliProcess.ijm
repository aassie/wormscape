// Wormscape AliProcess
// Alignement Processing script
// Post process the output of Worm_align.jim.
// Split worm picture by channels and extra the fluorescence intensity information to text files.
// Adrien Assie, February 2024

// Todo, merge to original processing script.

Train=getBoolean("Do you want to save Brightfield picture for machine learning training?");

l=50
waitForUser("Please select the folder that contains the output of Worm_align.jim");
WAinput = getDirectory("Please select the folder that contains the output of Worm_align.jim");
//waitForUser("Please select the folder that contains the original raw pictures");
//RawInput = getDirectory("Please select the folder that contains the original raw pictures");

cinput=WAinput+"CellProfiler/";
sinput=WAinput+"WormScape_Output/";
if(Train==1){
	TrainOut=WAinput+"Training_Output/";
}
File.makeDirectory(sinput);
File.makeDirectory(TrainOut);
RawInput=WAinput+"data/";
list = getFileList(RawInput);

// ROI crap control
if(roiManager("count")>0){
	roiManager("Set Line Width", 1);
	roiManager("Deselect");
	roiManager("Delete");
}

for (i=0; i<list.length; i++){
	if(endsWith(list[i], "_ROIs.zip")){
		isel=list[i];
		processFile(isel,cinput,RawInput,sinput,TrainOut);
	}
}


function processFile(isel,cinput,RawInput,sinput,TrainOut){
		roidata=isel;
		name=replace(isel,"\\_ROIs\\.zip","");
		roiManager("Open", RawInput+roidata);
		roiManager("Set Line Width", l);

		for (c=1;c<4;++c){
			cpic="Ch"+c+"_"+name+".tif";
			print(cpic);
			path=cinput+cpic;
			print(path);
			open(path);
			if(c>1){
				run("Subtract Background...", "rolling="+l);
			}
			montImg = getTitle();
			if(c==1){
				if(Train==1){
					for (s=0; s<roiManager("count"); ++s) {
					selectWindow(montImg);
					run("Duplicate...", "title=crop");
		            roiManager("Select", s);
		            if(Roi.size>2){
		            	run("Fit Rectangle");
		           		run("Crop");
						saveAs("jpg", TrainOut+File.separator+name+"_worm_"+s+1+".tif");
						}
				 	run("Close");
				 	}
				}
			}
			for (s=0; s<roiManager("count"); ++s) {
				selectWindow(montImg);
				roiManager("Select", s);
				run("Straighten...");
				getDimensions(width, height, channels, slices, frames);
				w = width;
				h = height;
				saveAs("Tiff", sinput+File.separator+name+"_worm_Ch"+c+"_"+s+1+".tif");
			 	saveAs("Text Image",sinput+File.separator+name+"_worm_Ch"+c+"_"+s+1+".txt");
			 	run("Close");}
			close(montImg);
		}

	roiManager("Set Line Width", 1);
	roiManager("Deselect");
	roiManager("Delete");
}
waitForUser("It worked without crashing!!! Go you!");
