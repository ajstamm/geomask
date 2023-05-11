#purpose of this R script is to take an input shapefile of points
#and randomly move the points a specified distance
#and output the corresponding shapefile and KML file
#Programmed in R2.13.0 for Windows XP
#later adapted to work in R3.0.2 for Windows 7
#by Gwen LaSelva, June 2011-November 2013

#eventually, add features to keep the points within an area
#and to make the distance the point is moved depend on population density
#non default libraries needed: sp, rgdal to read in shapefile
#tcktk and svDialogs for GUI

#load default libraries. Need this to run as batch.
#currently requires shapefiles to be in lat/long
#might want to look at function spTransform() to change projection if needed

#v1.1 June 17, 2011, add minimum distance
#v1.2 July 20, 2011, add log file and adjust maximum distances if needed
#Aug 9, 2011 improve error catching for opening files - catch files with wrong capitalization
#November, 2013 adapt to work with R 3.0.2: 
#	changed guiDlgOpen to dlgOpen, guiDlgMessage to dlgMessage, dlgSave
#	add $res after dlgOpen and dlgMessage where needed

library(datasets)
library(utils)

setWindowTitle(":  NYSDOH Geomask Tool") #adds this as a suffix to the usual window title

setStatusBar("NYSDOH Geomask Tool is running.  Please wait for dialogs.")


library(grDevices)
library(graphics)
library(stats)
library(methods)
#library(gpclib) #in case not automaticaly loaded
if(exists("gpclibPermit")==TRUE){gpclibPermit()} #might be disabled by default, this enables it
library(svDialogs) #for guiDlgOpen, it needs svMisc
library(rgdal)
library(tcltk)
#tclRequire("BWidget")
step<-1

###########################################################################
#custom dialog function to input distance (free text) and distance units (from dropdown)
#uses R package tclTk
#list of function arguments: gattitle, gatmessage,gathelpfile,gatdefault
#fuction returns eiether "go back" or free text, and data field selection, and unit selection
#	(3 items)
###############################################################################
#this function from /pkg/svMisc/R/TempEnv.R, needed for gatgui and gatInput functions
TempEnv <-
function() {
    pos <-  match("TempEnv", search())
    if (is.na(pos)) { # Must create it
        TempEnv <- list()
        attach(TempEnv, pos = length(search()) - 1)
        rm(TempEnv)
        pos <- match("TempEnv", search())
    }
    return(pos.to.env(pos))
}
gatInput<-function(gattitle="GAT input window",gathelpfile=paste("file://",getwd(),"/help/Geomask help.htm",sep=""),
	gatdefault="default text",gatmessage="Please enter something in the box",dd1choices=c("NONE","some"),dd2choices=c("m","km","mi")){
#use this function for free text input, like the minimum values
assign("res.inbox",value=character(0),envir=TempEnv())
assign("res2.inbox",value=dd1choices[1],envir=TempEnv())
assign("res3.inbox",value=character(0),envir=TempEnv())
assign("res4.inbox",value=dd1choices[1],envir=TempEnv())
assign("res5.inbox",value=dd2choices[1],envir=TempEnv())


    inbox <- tktoplevel()
    tktitle(inbox) <- gattitle
    wlabel <- tklabel(inbox, text = gatmessage)
    varText <- tclVar("0")
    varText2 <-tclVar(gatdefault)
    mintext <- tkentry(inbox, width = "20", textvariable = varText)
    maxtext <- tkentry(inbox, width = "20", textvariable = varText2)

    onOk <- function() {
	    pickmin <- tclvalue(dd1pick)
	    pickmax <- tclvalue(dd2pick)
	    pickunit<-tclvalue(dd3pick)
        assign(".res.inbox",value=tclvalue(varText),envir=TempEnv())
	  assign(".res2.inbox",value=pickmin,envir=TempEnv())
	  assign(".res3.inbox",value=tclvalue(varText2),envir=TempEnv())
	  assign(".res4.inbox",value=pickmax,envir=TempEnv())
	  assign(".res5.inbox",value=pickunit,envir=TempEnv())
        tkdestroy(inbox)
        return(res.inbox)
    }
    onCancel<-function(){tkdestroy(inbox);quit(save="no")}
    OnBack<-function(){
    assign(".res.inbox", "go back", envir = TempEnv())
    tkdestroy(inbox)}
    OnHelp <- function(){browseURL(gathelpfile)}
    wbutOK <- tkbutton(inbox, text = "  Next>  ", width = "12", command = onOk, 
        default = "active")
    #wlabSep <- tklabel(butFrame, text = " ")
    wbutCancel <- tkbutton(inbox, text = "Cancel", width = "12", 
        command = onCancel)

help.but <- tkbutton(inbox,text="  Help  ",width=12,command=OnHelp)
back.but <- tkbutton(inbox,text=" <Back  ",width=12,command=OnBack)

dd1pick<-tclVar(dd1choices[1]) #default selection is first in list - fields for min
tl1 <- ttkcombobox(inbox, values=dd1choices, textvariable=dd1pick, state="readonly") 

dd2pick <- tclVar(dd1choices[1]) #default selection is first in list -fields for max
tl2 <- ttkcombobox(inbox, values=dd1choices, textvariable=dd2pick, state="readonly") 

dd3pick <- tclVar(dd2choices[1]) #default selection is first in list -units
tl3 <- ttkcombobox(inbox, values=dd2choices, textvariable=dd3pick, state="readonly") 

label1<-tklabel(inbox,text="or get minimum distance from data:")
label2<-tklabel(inbox,text="or get maximum distance from data:")
label3<-tklabel(inbox,text="units:")
    tkgrid(wlabel, column=0,row=0,sticky = "w", padx = 5, pady = 5) #label above free entry box
    tkgrid(mintext, column=0,row=1,sticky="e",padx = 5, pady = 5)
    tkgrid(label1, column=1,row=1, sticky="e",padx = 5, pady = 5) #text label for dropdown
    tkgrid(tl1,column=2,row=1, sticky="w",padx = 5, pady = 5)#dropdown

    tkgrid(maxtext,column=0,row=2,sticky="e",padx=5,pady=5)
    tkgrid(label2,column=1,row=2,sticky="e",padx=5,pady=5)
    tkgrid(tl2,column=2,row=2,sticky="w",padx=5,pady=5)

    tkgrid(label3,column=0,row=3, sticky="e", padx=5, pady=5) #select units text
    tkgrid(tl3, column=1,row=3,sticky="w", padx=5, pady=5) #select units dropdown

    tkgrid(back.but,column=0,row=4,sticky = "e",padx=5,pady=5) #add extra buttons
    tkgrid(wbutOK,column=1,row=4,sticky = "w",padx=5,pady=5) #add extra buttons
    tkgrid(wbutCancel,column=2,row=4,sticky = "w",padx=5,pady=5) #add extra buttons
    tkgrid(help.but,column=3,row=4,sticky = "e",padx=5,pady=5) #add extra buttons

    #tkgrid(butFrame, pady = 5,padx=5)


    #for (row in 0:2) tkgrid.rowconfigure(inbox, row, weight = 0)
    #for (col in 0:0) tkgrid.columnconfigure(inbox, col, weight = 0)
    .Tcl("update idletasks")
    tkwm.resizable(inbox, 0, 0)
    tkbind(inbox, "<Return>", onOk)
    tkwm.deiconify(inbox)
    tkfocus(maxtext)
    tkselection.from(maxtext, "0")
    tkselection.to(maxtext, as.character(nchar(gatdefault)))
    tkicursor(maxtext, as.character(nchar(gatdefault)))
    tkwait.window(inbox)
    res<-get(".res.inbox", envir=TempEnv(), inherits=FALSE)
    res2<-get(".res2.inbox", envir=TempEnv(), inherits=FALSE)
    res3<-get(".res3.inbox",envir=TempEnv(), inherits=FALSE)
    res4<-get(".res4.inbox",envir=TempEnv(), inherits=FALSE)
    res5<-get(".res5.inbox",envir=TempEnv(), inherits=FALSE)

    remove(res.inbox,envir=TempEnv(),inherits=FALSE)
    remove(res2.inbox,envir=TempEnv(),inherits=FALSE)
    remove(res3.inbox,envir=TempEnv(),inherits=FALSE)
    remove(res4.inbox,envir=TempEnv(),inherits=FALSE)
    remove(res5.inbox,envir=TempEnv(),inherits=FALSE)
    choices=c(res,res2,res3,res4,res5)
    return(choices)
}#end gatInput function
#####################################################################
#end custom dialog function
####################################################################

###################################################################
#begin user input section
###################################################################
while(step<100){#get user input until finalized
##prompt user for point file
if(step==1){setStatusBar(paste("NYSDOH Geomask Tool: Looking for point file."))
print("in step one")
userfile<-dlgOpen(title = "Select Shapefile of Points to move", defaultFile = "albanypoints.shp", defaultDir = getwd(), multi=FALSE, filters=c("Shapefiles (*.shp)","*.shp"))$res
if(length(userfile)==0){quit(save="no")} #this will happend if you hit cancel

#remove extension if present
periodloc<-regexpr(".",userfile,fixed=TRUE) #will be -1 if no match, otherwise location(s) of matches
if(periodloc>0){userfile<-substr(userfile,1,periodloc[1]-1)}

checkfile=file.access(paste(userfile,".shp",sep=""),mode=4) #-1 for bad, 0 for OK

if(checkfile==0){#found shapefile
#if found likely good shapefile

#get point file
#find input file name and path
slashloc=max(unlist(gregexpr("/",userfile,fixed=TRUE))) #find location of last slash, divides path and file name
userfilein<-substr(userfile,slashloc+1,nchar(userfile))
userpathin<-substr(userfile,1,slashloc-1)

mypoints.shp<-try(readOGR(dsn=userpathin,layer=userfilein)) #reads it in as spatialpolygonsdataframe with projection information
if(class(mypoints.shp)!="try-error"){
step<-1.5
print("checking projection")
#find if these maps are lat/long or not
#myproj<-proj4string(nytown.shp)=="+proj=longlat"
myproj<-grepl("longlat",proj4string(mypoints.shp),fixed=TRUE) #returns logical vector
if(is.na(myproj)){myproj<-FALSE} #default to not lat/long if something goes wrong
if(myproj==FALSE){dlgMessage("Sorry, the point file must be in latitude/longitude",title="Unsuitable Projection",type="ok",icon="error")
quit(save="no")}
#get a list of all numeric fields
listitems<-names(mypoints.shp@data)
listtype<-sapply(mypoints.shp@data,class)
listtype[listtype=="factor"]<-FALSE
listtype[listtype=="character"]<-FALSE
listtype[listtype=="integer"]<-TRUE
listtype[listtype=="numeric"]<-TRUE
numlistitems<-listitems[listtype==TRUE]
}else{#try fails
print("Sorry, couldn't find point shapefile")
dlgMessage("Sorry, couldn't open your point shapefile.  Please check capitalization of file name.", title = "File error",type="ok",icon="error")
#quit(save="no")
step<-1
}
}else{#checkfile not good-else needs to be on the same line as bracket so R knows the statement continues
print("Sorry, couldn't find point shapefile")
dlgMessage("Sorry, couldn't find your point shapefile", title = "File error",type="ok",icon="error")
#quit(save="no")
ste<-1
}##end prompt user for point file
}#end step 1

if(step==1.5){##prompt user for boundary file within which to keep points when they are moved
setStatusBar(paste("NYSDOH Geomask Tool: Looking for boundary file."))

userfileb<-dlgOpen(title = "Select Shapefile of boundaries to keep points in", defaultFile = "albanytown.shp", defaultDir = getwd(), multi=FALSE, filters=c("Shapefiles (*.shp)","*.shp"))$res

if(length(userfileb)==0){quit(save="no")}
#remove extension if present
periodloc<-regexpr(".",userfileb,fixed=TRUE) #will be -1 if no match, otherwise location(s) of matches
if(periodloc>0){userfileb<-substr(userfileb,1,periodloc[1]-1)}

checkfile=file.access(paste(userfileb,".shp",sep=""),mode=4) #-1 for bad, 0 for OK

if(checkfile==0){#if checkfile=0 found shapefile
 #get boundary file
 #find input file name and path
 slashloc=max(unlist(gregexpr("/",userfileb,fixed=TRUE))) #find location of last slash, divides path  and file name
 userfileinb<-substr(userfileb,slashloc+1,nchar(userfileb))
 userpathinb<-substr(userfileb,1,slashloc-1)
 mybound.shp<-try(readOGR(dsn=userpathinb,layer=userfileinb))

 if(class(mybound.shp)!="try-error"){#found good boundary file
  step<-2 
  #also need to check the projection here
  myprojb<-grepl("longlat",proj4string(mybound.shp),fixed=TRUE) #returns logical vector
  if(is.na(myprojb)){myprojb<-FALSE} #default to not lat/long if something goes wrong
  if(myprojb==FALSE){dlgMessage("Sorry, the boundary file must be in latitude/  longitude",title="Unsuitable Projection",type="ok",icon="error")
  quit(save="no")}
  #if projection is lat/lon, myproj=TRUE, otherwise FALSE
}else{#try fails
  print("Sorry, couldn't open boundary shapefile")
  dlgMessage("Sorry, couldn't open your boundary shapefile.  Please check file name capitalization.",  title = "File error",type="ok",icon="error")
  #quit(save="no")
  step<-1.5
}#end try fails
}else{#checkfile is not zero.  else needs to be on the same line as bracket so R knows the statement continues
 print("Sorry, couldn't find boundary shapefile.")
 dlgMessage("Sorry, couldn't find your boundary shapefile", title = "File error",type="ok",icon="error")
#quit(save="no")
 step<-1.5
}##end if misspelled file name

}#end step 1.5, prompt for boundary file

if(step==2){userdist<-1000 #in meters, fornow

setStatusBar(paste("NYSDOH Geomask Tool: enter maximum distance to move points"))

userdistc<-gatInput(gatmessage=paste("Enter minimum distance:"), gattitle = "Maximum distance to move points", gatdefault = as.character(userdist),gathelpfile=paste("file://",getwd(),"/help/Geomask help.htm",sep=""),dd1choices=c("NONE",numlistitems),dd2choices=c("m","ft","km","mi"))

if(userdistc[2]=="NONE"){
userdistmin<-rep(as.numeric(userdistc[1]),times=length(mypoints.shp@data[,1]))
}else{userdistmin<-mypoints.shp@data[,userdistc[2]]}

if(userdistc[4]=="NONE"){
userdistmax<-rep(as.numeric(userdistc[3]),times=length(mypoints.shp@data[,1]))
}else{userdistmax<-mypoints.shp@data[,userdistc[4]]}

#check entered data for any obvious errors
while(class(userdistmax)!="numeric"&&class(userdistmax)!="integer"&&userdistc[1]!="go back"){
userdistc<-gatInput(gatmessage = paste("Please reenter maximum distance to move points"), gattitle = "Maximum distance", gatdefault = "1000",gathelpfile=paste("file://",getwd(),"/help/Geomask help.htm",sep=""),dd1choices=c("NONE",numlistitems),dd2choices=c("m","ft","km","mi"))
if(userdistc[2]=="NONE"){userdistmax<-as.numeric(userdistc[1])}
else{userdistmax<-mypoints.shp@data[,userdistc[2]]}
}#end while

if(userdistc[1]=="go back"){step<-1}else{step<-3}

if(length(which(userdistmax<=0))==length(userdistmax)&&userdistc[1]!="go back"){#all maximum distances are zero or less
mymessage<-"Please specify a maximum distance greater than zero."
mycancel<-dlgMessage(mymessage,title=paste("Maximum distance<=0"),type="ok",icon="error")$res
step<-2
}

if(length(which(userdistmax<userdistmin))>0&&userdistc[1]!="go back"&&step!=2){#at least one max distance is less than the minimum
mymessage<-"Maximum distance(s) must be greater than or equal to minimum distance(s).  Please choose different minimum or maximum distance(s)."
mycancel<-dlgMessage(mymessage,title=paste("Maximum<Minimum distance"),type="ok",icon="error")$res
step<-2
}

if(length(which(userdistmin==userdistmax))>0&&userdistc[1]!="go back"&&step!=2){#then at least one max and min are the same
mymessage<-"At least one minimum distance is the same as the maximum distance.  This will result in some points being moved a fixed distance.  Are you sure you want to proceed?"
if(userdistc[2]=="NONE"&&userdistc[4]=="NONE"&&userdistmax[1]>0){mymessage<-"The minimum and maximum distances are the same.  This will result in points being moved a fixed distance.  Are you sure you want to proceed?"}
mycancel<-dlgMessage(mymessage,title=paste("Maximum=minimum distance"),type="yesnocancel",icon="error")$res

if(mycancel=="yes"){step<-3} #done with user input 
if(mycancel=="no"){step<-2} #if no, stay at this step
if(mycancel=="cancel"){quit(save="no")}

} 


}#end step 2

if(step==3){

setStatusBar(paste("NYSDOH Geomask Tool: confirm your selections"))

if(userdistc[2]=="NONE"&&userdistc[4]=="NONE"){mymessage<-paste("Do you want to move points from ",userfilein," at least ",userdistc[1],userdistc[5]," and at most ",userdistc[3],userdistc[5]," but keep them within the boundaries of ",userfileinb,"?")
}else if(userdistc[2]=="NONE"&&userdistc[4]!="NONE"){mymessage<-paste("Do you want to move points from ",userfilein," at least ",userdistc[1],userdistc[5]," and the maximum distance specified in the data field ",userdistc[4],"in", userdistc[5]," but keep them within the boundaries of ",userfileinb,"?")
}else if(userdistc[2]!="NONE"&&userdistc[4]=="NONE"){mymessage<-paste("Do you want to move points from ",userfilein," the minimum distance specified in the data field ",userdistc[2],"in", userdistc[5],"and at most",userdistc[3],userdistc[5]," but keep them within the boundaries of ",userfileinb,"?")
}else if(userdistc[2]!="NONE"&&userdistc[4]!="NONE"){mymessage<-paste("Do you want to move points from ",userfilein," the minimum distance specified in the data field ",userdistc[2],"in", userdistc[5],"and the maximum distance specified in the data field",userdistc[4],"in", userdistc[5]," but keep them within the boundaries of ",userfileinb,"?")
}else{
mymessage<-paste("Something is wrong with the program.  Please e-mail gdb02@health.state.ny.us")}

mycancel<-dlgMessage(mymessage,title=paste("Confirm your choices"),type="yesnocancel",icon="question")$res

if(mycancel=="yes"){step<-100} #done with user input 
if(mycancel=="no"){step<-2} #if no, go back a step
if(mycancel=="cancel"){quit(save="no")}
}#end step 3, confirmation
} #end the while step<100

###############################################################################################
#end of user input section.  Next section moves the points
###############################################################################################

starttime<-Sys.time()

#convert units to meters if needed
if(userdistc[5]=="km"){userdistmax<-userdistmax*1000
				userdistmin<-userdistmin*1000}
if(userdistc[5]=="mi"){userdistmax<-userdistmax*1609.344
				userdistmin<-userdistmin*1609.344}
if(userdistc[5]=="ft"){userdistmax<-userdistmax/3.2808399
			userdistmin<-userdistmin/3.2808399}

recordmax<-userdistmax

#now, want to display the points
#will want to confirm that the class is "SpatialPointsDataFrame" for mypoints.shp
#class of mybound.shp is "SpatialPolygonsDataFrame"

plot(mybound.shp)
points(mypoints.shp, pch=21, col="black",bg="red") #produces red circles outlined in black
#title("Points before moving")

npoints<-nrow(mypoints.shp)
#set.seed(seed, kind = NULL, normal.kind = NULL)

set.seed(Sys.time()) #attempt to set the seed based on time to avoid duplicate random number sequences
newdis<-runif(npoints,min=userdistmin,max=userdistmax)
newang<-runif(npoints,min=0,max=2*pi)

#formulas to move points
#for y: original latitude+ SIN(r1*2*PI())*r2*500/111000
#for x: original longitude+COS(r1*2*PI())*r2*500)/(COS(RADIANS(original lat))*111321)

mycoords<-coordinates(mypoints.shp)
#coordinates of areas gives label points, one for each area
myx<-mycoords[,1]
myy<-mycoords[,2]

#make sure arguments of sin/cosine are in radians.  
mynewy<-myy+sin(newang)*newdis/111000  #approx 111 km per degree latitude
mynewx<-myx+cos(newang)*newdis/(cos(myy*pi/180)*111321)

minchangeflag<-FALSE
maxchangeflag<-FALSE

#now, also need to get coordinates for polygons, and check if points are inside
x<-length(mybound.shp@polygons)
for(n in 1:x){
myp<-mybound.shp@polygons[[n]] #here '1' is the first polygon in the shapefile (can loop through), 9 is albany for my example data
myp2<-myp@Polygons[[1]]
mypc<-myp2@coords
mypx<-mypc[,1]
mypy<-mypc[,2]
#check if points are in polygon.  0=exterior, 1=interior, 2&3 are on edge/vertex
checkifin<-point.in.polygon(myx,myy,mypx,mypy)
checkifnewin<-point.in.polygon(mynewx,mynewy,mypx,mypy)

#find what area the point is in, and correct maximum distances based on the maximum possible in the area if needed

y<-length(checkifin)
for(p in 1:y){#check each area
if(checkifin[p]>1){checkifin[p]<-1} #treat edges as interior
if(checkifnewin[p]>1){checkifnewin[p]<-1}

#reset these values to defaults
trycount<-0

if(checkifin[p]==1){#if point is in area
#for all points, reset maximum to maximum possible given boundary restriction
mydistances<-spDistsN1(mypc,mycoords[p,],longlat=TRUE)*1000 #what units are these distances in? kilometers, so multiply by 1000 to get meters

#mypc is polygon coordinates, mycoords is original point coordinates
if(max(mydistances)<userdistmax[p]){#maximum distance is outside of farthest point of polygon - is unnecessary
userdistmax[p]<-max(mydistances)
maxchangeflag<-TRUE}

recordmax[p]<-userdistmax[p] #keep track of the maximum maximum tried-either what the user entered or the farthest point in the boundary polygon

if(userdistmin[p]>userdistmax[p]/2){userdistmin[p]<-userdistmax[p]/2
minchangeflag<-TRUE
}
newdis[p]<-runif(1,min=userdistmin[p],max=userdistmax[p])
newang[p]<-runif(1,min=0,max=2*pi)
mynewy[p]<-myy[p]+sin(newang[p])*newdis[p]/111000  #approx 111 km per degree latitude
mynewx[p]<-myx[p]+cos(newang[p])*newdis[p]/(cos(myy[p]*pi/180)*111321)
checkifnewin[p]<-point.in.polygon(mynewx[p],mynewy[p],mypx,mypy)
if(checkifnewin[p]>1){checkifnewin[p]<-1}
trycount<-trycount+1
}#end if point is in area

while(checkifin[p]==1&&checkifnewin[p]==0){#print(paste("moving point",as.character(p)," so it is within boundary"))
#if the old point is in an area, but the new point is not, need to fix it
trycount<-trycount+1

if(trycount/100==floor(trycount/100)){#if try at least 100 times and cannot get a point in the polygon
#change the maximum distance
userdistmax[p]<-userdistmax[p]-userdistmax[p]*0.1
if(userdistmin[p]>userdistmax[p]/2){userdistmin[p]<-userdistmax[p]/2
minchangeflag<-TRUE}
maxchangeflag<-TRUE} #end changing max distance after 100 tries  

if(trycount/10==floor(trycount/10)){#If try at least 10 times and cannot get a point in the polygon, 
#change minimum distance
userdistmin[p]<-userdistmin[p]-userdistmin[p]*0.1
minchangeflag<-TRUE}

#get new candidate point
newdis[p]<-runif(1,min=userdistmin[p],max=userdistmax[p])
newang[p]<-runif(1,min=0,max=2*pi)
mynewy[p]<-myy[p]+sin(newang[p])*newdis[p]/111000  #approx 111 km per degree latitude
mynewx[p]<-myx[p]+cos(newang[p])*newdis[p]/(cos(myy[p]*pi/180)*111321)
checkifnewin[p]<-point.in.polygon(mynewx[p],mynewy[p],mypx,mypy)
if(checkifnewin[p]>1){checkifnewin[p]<-1}
}#for discrepant points
}#after cycling through number of points
}#cycle through all areas

########################################################################
#end moving points section.  Next distplay and output results
########################################################################

mynewcoords<-data.frame(mynewx,mynewy)
mydata<-data.frame(mypoints.shp)
#add my minimum/maximum distances
mydata<-cbind(mydata,userdistmin,recordmax)
names(mydata)[names(mydata)=="userdistmin"] = "min_dist" # rename 
names(mydata)[names(mydata)=="recordmax"] = "max_dist" # rename 
coordinates(mydata)<-mynewcoords
proj4string(mydata)<-proj4string(mypoints.shp) #assign original projection to new points

#dev.new()
#use points to add new data to same plot
points(mydata, pch=24, col="black",bg="blue") #produces green triangles outlined in black
title("Points before and after moving")
legend(x="topleft",legend=c("before","after"),horiz=FALSE,pch=c(21,24),col=c("black","black"),pt.bg=c("red","blue"))

userfileout<-"" #set as default missing
endtime<-Sys.time()

#warn user if minimum or maximum have been changed
if(minchangeflag==TRUE&&maxchangeflag==TRUE){dlgMessage("There was difficulty moving the points within the area specified.  Maximum and minimum distances were changed for at least some points.  Please check output file for details",title="WARNING: Parameters changed",type="ok",default=1,icon="warning")}

if(minchangeflag==TRUE&&maxchangeflag==FALSE){dlgMessage("There was difficulty moving the points within the area specified.  Minimum distances were changed for at least some points.  Please check output file for details",title="WARNING: Parameters changed",type="ok",default=1,icon="warning")}

if(minchangeflag==FALSE&&maxchangeflag==TRUE){dlgMessage("There was difficulty moving the points within the area specified.  Maximum distances were changed for at least some points.  Please check output file for details",title="WARNING: Parameters changed",type="ok",default=1,icon="warning")}

#get the user to enter filename to save file
setStatusBar(paste("NYSDOH Geomask Tool: save file"))

while(userfileout==""){
userout<-dlgSave(title = "Save Moved Points As", defaultFile = "", defaultDir = "",defaultExt = "", filters = c("Shapefiles (*.shp)", "*.shp"))$res
#now remove any file extention
periodloc=regexpr(".",userout,fixed=TRUE) #will be -1 if no match, otherwise location(s) of matches
if(periodloc>0){userout<-substr(userout,1,periodloc[1]-1)}

#find output file name and path
slashloc<-max(unlist(gregexpr("/",userout,fixed=TRUE))) #find location of last slash, divides path and file name
userfileout<-substr(userout,slashloc+1,nchar(userout))
userpathout<-substr(userout,1,slashloc-1)
#filename shouldn't contain ;=+<>|"[]/\'<>:*?

checkfile<-regexpr(";|:|\\+|=|<|>|\\||\\[|\\]|/|\"|'|\\*|\\?\n",userfileout,perl=TRUE)
if(checkfile[1]!="-1"){userfileout<-""}

checkfile<-charmatch("\\",userfileout,nomatch=-1)
if(checkfile[1]!="-1"){userfileout<-""}

if(userfileout==""){
dlgMessage("Your file name may have been invalid.  Please reenter your file name.", title="WARNING", type="ok")}
} #end while no good file name

#change names of old coordinates
names(mydata)[names(mydata)=="coords.x1"]<-"prev_x" #change the name of the old coordinates
names(mydata)[names(mydata)=="coords.x2"]<-"prev_y" #change the name of the old coordinates

#write  out shapefile using OGR
#output file doesn't seem to have projection
writeOGR(mydata, userpathout, userfileout, driver="ESRI Shapefile",verbose=TRUE,overwrite_layer=TRUE) #seems fast

kmlfilename<-paste(userpathout,paste(userfileout,"kml",sep="."),sep="/")
#for KML need writeOGR(object,fileName,layerName,driver="KML") 
#where layer name will be distplayed in layer list of Google Earth
mykmlnames<-paste("NameField=",names(mydata)[1],sep="") #default to use the first field as ID
writeOGR(mydata,kmlfilename,"moved_points", driver="KML",dataset_options=mykmlnames,verbose=FALSE)
#might be able to specifcy NameField, DescriptionField, AltitudeMode
#NameField=mydata@data[1]
writeOGR(mydata, userpathout, userfileout, driver="MapInfo File",verbose=TRUE,overwrite_layer=TRUE) #seems fast

################################################################################
#create a log file
#should contain input files, output file, min and max distances or sources (and units), date, run time
#(8 items)
################################################################################
#begin log file
logfile<-paste(userpathout,paste(userfileout,"txt",sep="."),sep="/")

setStatusBar(paste("NYSDOH Geomasking Tool: Writing log file ",logfile))

logtext<-"NYSDOH Geomasking Tool log"
write("",file=logfile,ncolumns=length(logtext),append=TRUE)
#logtext<-c("The current date is ", format(Sys.Date()))
#write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)
logtext<-c("The current date and time are ", format(Sys.time()))
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

logtext<-c("The input point file is: ",userfile)
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)
logtext<-c("Number of points moved: ",length(mydata@data[,1]))
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

logtext<-c("The input boundary file is: ",userfileb)
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)
write("",file=logfile,ncolumns=length(logtext),append=TRUE)

if(userdistc[2]=="NONE"){
logtext=c("Minimum distance requested was ",userdistc[1])
}else{logtext=c("Minimum distance was obtained from field ",userdistc[2],userdistc[5])}
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

if(userdistc[4]=="NONE"){#be sure to include units=userdistc[5]
logtext=c("Maximum distance requested was ",userdistc[3],userdistc[5])
}else{
logtext=c("Maximum distance was obtained from field ",userdistc[4])}
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

logtext<-c("The time this program took to move the points: ",endtime-starttime)
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

logtext<-c("The output file(s): ",userout)
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

#end code to create log file
##############################################################################

setStatusBar(paste("NYSDOH Geomask Tool: find your results in: ",userpathout))





