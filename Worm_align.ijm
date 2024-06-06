// Worm-align
// Original script forked from from github https://github.com/hannekeo/Worm-align/blob/master/Worm_align.ijm
// The script has then been modified and adapted for Wormscape.
// Adrien Assie, May 2024

macro "Worm_Align"{
	//Sanity check
	roiManager("Reset");
	//Let's Go!
	waitForUser("Please select the folder that contains your images");
	wait(500);
	input = getDirectory("Please select the folder that contains your images");
	list = getFileList(input);
	File.makeDirectory(input+"tmp");
	tmp = input+File.separator+"tmp";
	out1 = File.getParent(tmp);
	File.delete(tmp);
	//Loop to check if a previous run was done
	if(File.exists(out1 + "_output")==1){
		print("Previous Wormscape Run detected");
		ynBox=getBoolean("Previous Wormscape Run detected.\nDo you want to load the previous settings.");
		if(ynBox==1){
			output = out1 + "_output";
			CP = output+File.separator+"CellProfiler";
			data = output+File.separator+"data";
			aligned = output+File.separator+"aligned";
			single = output+File.separator+"single_worms";
			previousRun=1;
			fileCheck=1;
			checkVar=0;
			//Loop to find the last processed picture
			while(fileCheck==1){
				if(checkVar<lengthOf(list)){
					if(File.exists(data+File.separator+File.getNameWithoutExtension(input+list[checkVar])+"_ROIs.zip")==1){
						print("Picture "+checkVar+": Found ROIs");
						checkVar++;
					}else{
						print("Picture "+(checkVar+1)+": ROIs not found");
						fileCheck=0;
					}
				}else{
					print("All ROIs were found, worm align step appear to be done");
					showMessage("All ROIs were found, worm align step appear to be done");
					exit;
				}
			}
		}else{
			newRun=1;
			}
	}else{
		newRun=1;
	}
	if(newRun==1){
		print("New Wormscape Run detected");
		File.makeDirectory(out1 + "_output");
		output = out1 + "_output";
		File.makeDirectory(output+File.separator+"CellProfiler");
		CP = output+File.separator+"CellProfiler";
		File.makeDirectory(output+File.separator+"data");
		data = output+File.separator+"data";
		File.makeDirectory(output+File.separator+"aligned");
		aligned = output+File.separator+"aligned";
		File.makeDirectory(output+File.separator+"single_worms");
		single = output+File.separator+"single_worms";
		previousRun=0;
		checkVar=0;
	}
	// Open the first image to get the settings
	setimg = input + list[0];
	run("Bio-Formats Windowless Importer", "open="+setimg+" color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
	title = getTitle();

	// Standardising the pixel size
	getDimensions(width, height, channels, slices, frames);
	getPixelSize(unit, pixelWidth, pixelHeight);
	 p1 = pixelWidth;
	 p2 = pixelHeight;
	 w = width;
	 h = height;
	 c = channels;
	 a = (p1/1)*w;
	 b = (p2/1)*h;
	run("Size...", "width=&a height=&b constrain average interpolation=Bilinear");
	run("Properties...", "channels=&c slices=1 frames=1 unit=micron ");
	
	test=0;
while(test < 1){
	String.resetBuffer;
	if(previousRun==1){
		roiManager("Open", data+File.separator+"Reference_ROIs.zip");
		run("Set Measurements...", "display redirect=None decimal=3");
		run("Measure");
		line_width = Table.get("Length", 0);
		l = line_width;
		roiManager("Delete");
	}else{
		// Measure the width for the montage lines
		// This parameter sets the height of the panels in the montage.
		setTool("line");
		waitForUser("Draw a line across the width of a representative worm.\nThe width will be used to generate the montage.");
		roiManager("Add");
		roiManager("Save", data+File.separator+"Reference_ROIs.zip");
		run("Set Measurements...", "display redirect=None decimal=3");
		run("Measure");
		line_width = Table.get("Length", 0);
		l = line_width;
		Table.create("Results");
		roiManager("Delete");
	}
	

	// Getting all required settings from the example image
	ImgBit = bitDepth();
	if(ImgBit==24){
		test=1;
	}else{
		selectWindow(title);
		luts = getList("LUTs");

		for(j=1;j<channels+1;j++){
			setSlice(j);
			// Which channel is which color?
			// Are any channels to be excluded from the montage
			Dialog.create("Worm-align macro");
			Dialog.addMessage("Which is which? Enter channel information. \nRemove the tick if you want the channel excluded from the montage.-- ");
			Dialog.addString("Channel "+j, 0);
			Dialog.addToSameRow();
			Dialog.addChoice("LUT", luts);
			Dialog.addToSameRow();
			Dialog.addCheckbox("include", 1);
			Dialog.show();
			chI = Dialog.getString();
			chL = Dialog.getChoice();
			montage = Dialog.getCheckbox();
			Table.set("Channel", j-1, j);
			Table.set("LUT", j-1, chL);
			Table.set("Info", j-1, chI);
			Table.set("Montage", j-1, montage);
			Table.set("Folder", j-1, input);}
			Table.update;

		// Apply settings
		selectWindow(title);
		for(m=1; m<channels+1; m++){
			lut = Table.getString("LUT", m-1);
			selectWindow(title);
			setSlice(m);
			run(lut);
			mont = Table.get("Montage", m-1);
			string = String.append(mont);}

		if(channels>1){
			active = String.buffer;
			Stack.setDisplayMode("composite");
			Stack.setActiveChannels(active);}}

	// Check that all settings are fine
	Dialog.create("Checking all settings");
	Dialog.addCheckbox("Tick this box if you are happy with the settings", 0);
  	Dialog.addCheckbox("Montage of worms from single image", 1);
  	Dialog.addCheckbox("Combined montage", 1);
  	Dialog.show;
	test = Dialog.getCheckbox();
  	msing = Dialog.getCheckbox();
  	mall = Dialog.getCheckbox();

	// Save the table with settings used to generate the montage
	selectWindow("Results");
	saveAs("Results", data+File.separator+"Settings.csv");
	Table.rename("Results", "Settings");}
	selectWindow(title);
	run("Close");
	
	// The setup process is now completed and the macro proceeds to process all images in the input folder
	// Open all files in the input folder - checkVar is used as minimal image number to process
	for (i=checkVar; i<list.length; i++){
		image = list[i];
	    path = input + image;
	    open(path);
	
		name=File.nameWithoutExtension();
		original  = getTitle();
		getDimensions(width, height, channels, slices, frames);
		getPixelSize(unit, pixelWidth, pixelHeight);
	 	p1 = pixelWidth;
	 	p2 = pixelHeight;
	 	w = width;
	 	h = height;
	 	c = channels;
	 	a = (p1/1.50)*w;
	 	b = (p2/1.50)*h;
	
		// Standardising the pixel size
		selectWindow(original);
		run("Size...", "width=&a height=&b constrain average interpolation=Bilinear");
		run("Properties...", "channels=&c slices=1 frames=1 unit=micron ");
	
		exp_run=0;
		while(exp_run < 1){
			ImgBit = bitDepth();
			if(ImgBit==24){
				montImg = getTitle();
				exp_run=1;
			}else{
				// Adjusting the Intensity settings for all channels according to custom setting
				for(m=1; m<channels+1; m++){
					selectWindow("Settings");
					lut = Table.getString("LUT", m-1);
					selectWindow(original);
					setSlice(m);
					run(lut);}
	
				// Creating a copy of all image channels
				// Making sure the channels are on/off as required for the montage
				// The output image for the montage is an RGB image, as we need to preserve the B&C settings across all images
				selectWindow(original);
				if(channels==1){
					// This bit runs if there is one channel only
					selectWindow("Settings");
					selectWindow(original);
					run("Duplicate...", " ");
					run("RGB Color");
					montImg = getTitle();
	
					// Save the original
					selectWindow(original);
					saveAs("Tiff", CP+File.separator+"Original_"+name+".tif");
	
					// Save the green channel (in our case, but it could be any single channel image)
					saveAs("Tiff", CP+File.separator+"Ch1_"+name+".tif");
	
					// Save a rough worm mask to be used in CellProfiler
					setAutoThreshold("Huang dark");
					setOption("BlackBackground", true);
					run("Convert to Mask");
					saveAs("Tiff", CP+File.separator+"Mask_"+name+".tif");
					run("Close");
				}else{
					// This bit runs for multi channel images
					for(m=1; m<channels+1; m++){
					selectWindow("Settings");
					ch = Table.get("Channel", m-1);
					lab = Table.getString("LUT", m-1);
					mont = Table.get("Montage", m-1);
					string = String.append(mont);
					selectWindow(original);
					run("Duplicate...", "duplicate channels=ch");
					saveAs("Tiff", CP+File.separator+"Ch"+m+"_"+name+".tif");
					if(lab=="Grays"){
						// Save a rough worm mask to be used in CellProfiler
						// This part of the macro can be adjusted to fit your own data
						run("Minimum...", "radius=2");
						setAutoThreshold("Triangle");
						run("Convert to Mask");
						run("Watershed");
						saveAs("Tiff", CP+File.separator+"Mask_"+name+".tif");
						run("Close");
					}else{
						run("Close");}}
						selectWindow(original);
						active = String.buffer;
						Stack.setDisplayMode("composite");
						Stack.setActiveChannels(active);
						run("RGB Color");
						montImg = getTitle();
	
						// Saving the original
						selectWindow(original);
						saveAs("Tiff", CP+File.separator+"Original_"+name+".tif");
						run("Close");}}}
	
		// Select the worms for analysis
		newImage("mask", "8-bit black", a, b, 1);
		setTool("polyline");
		run("Line Width...", "line=1");
		selectWindow(montImg);
	
		waitForUser("Please draw lines along the length of each worm you want to analyse.\nClick Ctrl+T to add each worm. Click OK when finished");
		// Generating ROIs for the montage and the line mask
	 	if(roiManager("count")==0){
			selectWindow(montImg);
			saveAs("Tiff", CP+File.separator+"Original_"+name+".tif");
			run("Close");
		}else{
			// Saving the ROIs
			roiManager("Deselect");
			roiManager("Show All");
			roiManager("Save", data+File.separator+name+"_ROIs.zip");
	
		 	roiColors = loadColorsfromLut("glasbey_on_dark.lut");
			for (s=0; s<roiManager("count"); ++s) {
				z=(s+1);
				selectWindow(montImg);
				roiManager("Select", s);
				roiManager("Set Color", roiColors[z]);
				roiManager("Set Line Width", l);
				run("Straighten...");
				getDimensions(width, height, channels, slices, frames);
	 			w = width;
	 			h = height;
	 			saveAs("Tiff", single+File.separator+name+"_worm_"+z+".tif");
	 			roiManager("Set Line Width", 1);}}
	
		// Save the labeled overlay image
		selectWindow(montImg);
		roiManager("Show All with labels");
		roiManager("Set Line Width", 5);
		run("Flatten");
		saveAs("Png", data+File.separator+"Overlay_"+name+".png");
		run("Close");
	
		// Saving the line mask
		selectWindow("mask");
		roiManager("Show All without labels");
		roiManager("Set Line Width", 1);
		run("Set Measurements...", "min display redirect=None decimal=0");
		roiManager("Show All");
		roiManager("Measure");
		wormL = Table.getColumn("Length");
	
		for (s=0; s<roiManager("count"); ++s) {
			selectWindow("mask");
			roiManager("Select", s);
			roiManager("Set Line Width", 8);
			run("Line to Area");
			roiManager("Update");
			setColor(s+1);
			roiManager("Select", s);
			fill();}
	
		// Generating QC file for lines
		run("Clear Results");
		roiManager("Show All");
		roiManager("Measure");
		selectWindow("Results");
	
		Table.renameColumn("Min", "Worm number");
		Table.renameColumn("Max", "-");
		Table.setColumn("Length", wormL);
		Table.update;
		saveAs("Results", data+File.separator+name+"_QC.csv");
		run("Close");
	
		selectWindow("mask");
		saveAs("Tiff", CP+File.separator+"Lines_"+name+".tif");
		run("Close");
	
		// Creating the montage
		// This step works even if no worms are selected
		if(msing==1){
			if(roiManager("count")>1){
				run("Images to Stack", "method=[Copy (top-left)] name=Stack title=worm use");
				getDimensions(width, height, channels, slices, frames);
				c = slices;
				run("Make Montage...", "columns=1 rows=&c scale=1 border=2");
				saveAs("Tiff", aligned+File.separator+name+"_worms.tif");
				run("Close");
				selectWindow("Stack");
				run("Close");
			}else{
				selectWindow(name+"_worm_1.tif");
				saveAs("Tiff", aligned+File.separator+name+"_worms.tif");
				run("Close");}}
	
		roiManager("Delete");
		run("Close All");}
	
		// Create a single montage of the worms selected from the all images in the input folder
		if(mall==1){
			list2 = getFileList(single);
			for (k=0; k<list2.length; k++){open(single+File.separator+list2[k]);}
				run("Images to Stack", "method=[Copy (top-left)] name=Stack title=worm use");
				getDimensions(width, height, channels, slices, frames);
				c = slices;
				run("Make Montage...", "columns=1 rows=&c scale=1 border=2");
				saveAs("Tiff", aligned+File.separator+"ALLworms.tif");
				run("Close");
				selectWindow("Stack");
				run("Close");  	}
	
	// Functions below from ROI color coder macro
	// ROI_Color_Coder.ijm
	//
	// This macro colorizes ROIs listed in the Manager by matching ROI measurements to an index
	// of a LUT file. It can be used, e.g., to obtain a particle-size 'heat-map' of segmented
	// objects. It will terminate if the LUT file cannot be read.
	//
	// More info at http://imagejdocu.tudor.lu/doku.php?id=macro:roi_color_coder
	
	function loadColorsfromLut(lutROI) {
		path = getDirectory("luts")+lutROI;
	    list = File.openAsRawString(path);
	    rgbColor = split(list,"\n");
	    if(rgbColor.length<255 || rgbColor.length>257)
	        verboseExit("Error reading "+path, "Reason: Found unexpected number of columns");
		    hexColor = newArray(256);
	    	firstStrg = substring(rgbColor[0], 0, 1);
	    if (isNaN(firstStrg))
	        k = 1;    // the lut file has a header (Index Red Green Blue)
	     else
	        k = 0;    // there is no header. First row is index 0
	    for(i=k; i<256; i++) {
	        hex = rgbToHex(rgbColor[i]);
	        hexColor[i] = hex;
	    }
	    return  hexColor;
		 function rgbToHex(color) {
		    color1 = split(color,"\t");
		    if(color1.length==1)
		        color1 = split(color," ");
		    if(color1.length==1)
		        verboseExit("Chosen LUT does not seem to be either a tab-delimited",
		                    "or a space-delimited text file");
		    if(color1.length==4)
		        i = 1;    // first column of the lut file is the index number
		    else
		        i = 0;    // first column of the lut file is the red value
		    r = color1[0+i]; g = color1[1+i]; b = color1[2+i];
		    return ""+toHex(r)+""+toHex(g)+""+toHex(b);
		}
		
		function toHex(n) {
		    n = parseInt(n);
		    if(n==0 || isNaN(n))
		        return "00";
		    n = maxOf(0,n); n = minOf(n,255); n = round(n);
		    hex = ""+substring("0123456789ABCDEF",((n-n%16)/16),((n-n%16)/16)+1)+
		          substring("0123456789ABCDEF",(n%16),(n%16)+1);
		    return hex;
		}
	}
showMessage("The macro is finished.");
}