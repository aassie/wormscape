// Wormscape AliProcess
// Alignement Processing script
// Post process the output of Worm_align.jim.
// Split worm picture by channels and extra the fluorescence intensity information to text files.
// Adrien Assie, February 2024

// Todo, merge to original processing script.
	
macro "Worm_Process" {
	Train=getBoolean("Do you want to save Brightfield picture for machine learning training?");
	waitForUser("Please select the folder that contains the output of Worm_align");
	wait(500);
	WAinput = getDirectory("Please select the folder that contains the output of Worm_align");
	//waitForUser("Please select the folder that contains the original raw pictures");
	//RawInput = getDirectory("Please select the folder that contains the original raw pictures");
	
	sett=WAinput+File.separator+"data"+File.separator+"Settings.csv";
	cinput=WAinput+"CellProfiler"+File.separator;
	sinput=WAinput+"WormScape_Output"+File.separator;
	//print(WAinput+File.separator+"data"+File.separator+"Reference_ROIs.zip")
	
	if(File.exists(sett)==1){
		print("Found setting file");
		Table.open(sett);
		Table.title("Settings.csv");
		ChanNum=Table.size;
		print(ChanNum);
		close("Settings.csv");
		list = getFileList(cinput);
		open(cinput+File.separator+list[1]);
		roiManager("Open", WAinput+"data"+File.separator+"Reference_ROIs.zip");
		roiManager("Select", 0);
		//run("Set Measurements...", "display redirect=None decimal=3");
		run("Measure");
		Table.title("Results");
		line_width = Table.get("Length", 0);
		l = line_width;
		//print(l);
		roiManager("Delete");
		run("Close All");
	}else {
		print("Missing setting file");
		exit;
	}
	
	if(Train==1){
		TrainOut=WAinput+"Training_Output"+File.separator;
	}
	File.makeDirectory(sinput);
	File.makeDirectory(TrainOut);
	RawInput=WAinput+"data"+File.separator;
	list = getFileList(RawInput);
	
	// ROI crap control
	if(roiManager("count")>0){
		roiManager("Set Line Width", 1);
		roiManager("Deselect");
		roiManager("Delete");
	}
	
	for (i=0; i<list.length; i++){
		if(list[i]=="Reference_ROIs.zip"){
			print("Reference, skipping");
			}else{
		if(endsWith(list[i], "_ROIs.zip")){
			isel=list[i];
			processFile(isel,cinput,RawInput,sinput,TrainOut,l);
			}
		}
	}
	waitForUser("It worked without crashing!!! Go you!");
	
	function processFile(isel,cinput,RawInput,sinput,TrainOut,l){
			roidata=isel;
			name=replace(isel,"\\_ROIs\\.zip","");
			roiManager("Open", RawInput+roidata);
			roiManager("Set Line Width", l);
	
			for (c=1;c<(ChanNum+1);++c){
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
					print(l);
					run("Straighten...","title=test line="+l);
					getDimensions(width, height, channels, slices, frames);
					w = width;
					h = height;
					saveAs("Tiff", sinput+File.separator+name+"_worm"+(s+1)+"_Ch"+c+".tif");
				 	saveAs("Text Image",sinput+File.separator+name+"_worm"+(s+1)+"_Ch"+c+".tif");
				 	run("Close");}
				close(montImg);
			}
	
		roiManager("Set Line Width", 1);
		roiManager("Deselect");
		roiManager("Delete");
	}
}