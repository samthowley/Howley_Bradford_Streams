#!/usr/bin/python2
from Tkinter import *
import tkFileDialog
import tkMessageBox
import os
import time
import datetime
import csv
import numpy
import calendar
from scipy import stats
import math
import pylab as P
# # Customize matplotlib (for UNIGE)
# P.rcParams.update(
    # {
        # 'text.usetex': False,
        # 'font.family': 'stixgeneral',
        # 'mathtext.fontset': 'stix',
    # }
# )

P.rcParams.update({
    "pdf.fonttype"        : 42
})

from matplotlib.backends.backend_pdf import PdfPages
import fnmatch
from collections import defaultdict

#______________________________________________________________________________________________________________
#----------- SETTINGS------------------------------------------------------------------------------------------
#______________________________________________________________________________________________________________
resultsfoldername = 'C:\SAM\DATATOOLS'
samplelogfoldername = 'C:\SAM\SAMPLELOG'
inputfoldername = 'C:\UserData'
GUITitle='G2508 calculator'
GUIWindowTitle='G2508 calculator 20230210'

# this is the list of Picarro variables to report on (with slope, intercept and stdev if chosen)
varsToSummarize=['N2O_dry','CO2_dry','CH4_dry','NH3','H2O','OutletValve','ChemDetect']
# this is the list of Picarro variables to report on (only report the mean)
varsToSummarizeStatic=['ChemDetect']
# this is the list of Picarro variables to chart in the PDFs
varsToChart=['N2O_dry','CO2_dry','CH4_dry','NH3','H2O','OutletValve']

# time it takes to reach a stable concentration after inserting the needle (seconds) (240)
stabilizingTimeToIgnore=240;
# time to use for analysis of the sample (minutes) (4)
analysisTimeMinutes=4;
# extra time to plot after each sample (120) seconds
extraTimeToPlot=120

# offset computer time to UTC (-4/-5 for Quebec, +1/+2/+3 for Europe)
offsetUTC=0
# by default save slope and intercept? (0=no, 1=yes)
alwaysDoSlope=0
# by default savestdev? (0=no, 1=yes)
alwaysDoStdev=0

#______________________________________________________________________________________________________________
#----------- No changes needed below here----------------------------------------------------------------------
#______________________________________________________________________________________________________________

def MergeResults():
    global sampletimefilename
    global mergedresultsfilename
    global tempfilename

    opensampletimefile=open(sampletimefilename, 'rb')
    try:
        sampletimes = numpy.genfromtxt(opensampletimefile, delimiter=',', dtype=None, names=True, deletechars="~!@#$%^&*()-=+~\|]}[{';: /?.>,<.",encoding='ascii')
    except:
        sampletimes = numpy.genfromtxt(opensampletimefile, delimiter=',', dtype=None, names=True, deletechars="~!@#$%^&*()-=+~\|]}[{';: /?.>,<.")
    # Need to convert to a 2d array if only one sample
    if len(sampletimes.shape) == 0:
       sampletimes = numpy.array([sampletimes])
    print ("amount samples to be summarized:" +str(len(sampletimes['SampleName'])))

    sampleepochtimes=[]; 
    c1=0
    # run through sampletimefile and find first and last date
    for row in sampletimes['SampleName']:
      samplestartstr=str(sampletimes['Year'][c1])+" "+str(sampletimes['Month'][c1])+" "+str(sampletimes['Day'][c1])+" "+str(sampletimes['Hour'][c1])+" "+str(sampletimes['Minute'][c1]) +" "+str(sampletimes['Second'][c1])
      samplestructtime=time.strptime(samplestartstr, "%Y %m %d %H %M %S")
      sampleepochtime=calendar.timegm(samplestructtime)
      sampleepochtimes.append(sampleepochtime)
      c1=c1+1

    sampleepochtimes=sorted(sampleepochtimes)
    firstsampletime=min(sampleepochtimes)
    lastsampletime=max(sampleepochtimes)  
    #print sampleepochtimes
    
    print ("script is located in:")
    print(os.path.abspath(os.path.dirname(__file__)))
    #os.chdir(resultsfoldername)
    mergedresultsfilename=datetime.datetime.now().strftime('%Y%m%d_%H%M%S') + '_merged_results.csv'
    openresultsfile=open(resultsfoldername+"/"+ mergedresultsfilename, 'wb')
    resultswriter = csv.writer(openresultsfile, delimiter=',')

    colNames=['Date','Local_Time','Epoch_Time']
    for var in varsToSummarize:
        colNames.append(var)
    for var in varsToSummarizeStatic:
        colNames.append(var)
    resultswriter.writerow(colNames)
    

    os.chdir(inputfoldername)
      
    # find all .dat files in the data folder (scipt will get slower when more data is in this folder)
    for dirpath, dirs, files in os.walk(inputfoldername):
        for file in files:
            if file.endswith(".dat"):
                junk,datestr,timestr,junk2=file.split("-",3)
                filetimestr=datestr+ timestr[0:6]
                filestructtime = time.strptime(filetimestr, '%Y%m%d%H%M%S')
                fileepochtime=calendar.timegm(filestructtime)
                
                # check if data file could contain data of samples of interest 86400 seconds is a day
                if fileepochtime > firstsampletime-86401 and fileepochtime < lastsampletime+86401:
                    print ("USING file: "+file)
                    try:
                        openinputfile=open(os.path.join(dirpath, file),'rb') 
                        try:
                            fd = numpy.genfromtxt(openinputfile, dtype=None, names=True,deletechars="~!@#$%^&*()-=+~\|]}[{';: /?.>,<.",encoding='ascii',invalid_raise = False)
                        except:
                            fd = numpy.genfromtxt(openinputfile, dtype=None, names=True,deletechars="~!@#$%^&*()-=+~\|]}[{';: /?.>,<.",invalid_raise = False)
                        openinputfile.close()
                        c1=0
                        for row in fd['TIME']:
                            temptime,junk=row.split(".",1)
                            datetimestr=str(fd['DATE'][c1])+" "+str(temptime)
                            tempstructtime=time.strptime(datetimestr, "%Y-%m-%d %H:%M:%S")
                            tempepochtime=time.mktime(tempstructtime)
                            
                            if UTCAuto.get()==1:
                                UTCOff=autoUTCOffset
                            else:
                                UTCOff=int(UTCoffsetentry.get())
                            tempepochtime=tempepochtime+UTCOff*3600

                            gooddate=time.strftime("%Y-%m-%d", tempstructtime)
                            goodtime=(time.strftime("%H:%M:%S", tempstructtime))
                            
                            varData=[]
                            for var in varsToSummarize:
                                try:
                                    varData.append(fd[var][c1])
                                except:
                                    varData.append(0.0)
                            for var in varsToSummarizeStatic:
                                try:
                                    varData.append(fd[var][c1])
                                except:
                                    varData.append(0.0)        
                            resultswriter.writerow([gooddate,goodtime,fd['EPOCH_TIME'][c1]]+varData)
                            c1=c1+1
                    except Exception as e:
                        print "could not read: " + file
                        if hasattr(e, 'message'):
                            print(e.message)
                        else:
                            print(e)
                       
         
         
    openresultsfile.close()
   
def askopenresultsfilename():
    global sampletimefilename  # file with the sample names and times (switcherlog)
    global mergedresultsfilename
    global tempfilename
      
    # get filename
    fileopen_opt = options = {}
    options['defaultextension'] = '.csv' 
    options['filetypes'] = [('csv files', '.csv'),('all files', '.*')]
    options['initialdir'] = samplelogfoldername
    options['initialfile'] = 'sample_times_names.csv'
    options['parent'] = root
    options['title'] = 'Choose a csv file with samplenames and times to open'
    sampletimefilename = tkFileDialog.askopenfilename(**fileopen_opt)

    # open file 
    if sampletimefilename:
        tempfilename=datetime.datetime.now().strftime('%Y%m%d_%H%M%S')
        MergeResults()

        opensampletimefile=open(sampletimefilename, 'rb')
        os.chdir(resultsfoldername)
        openinputfile=open(mergedresultsfilename, 'rb')
        resultsfileName=tempfilename + '_results.csv'
        openresultsfile=open(resultsfileName, 'wb')

        pdffile1 = PdfPages(tempfilename +'_charts_sample_only.pdf')
        pdffile2 = PdfPages(tempfilename +'_charts_whole_run.pdf')

        try:
            sampletimes = numpy.genfromtxt(opensampletimefile, delimiter=',', dtype=None, names=True,encoding='ascii')
            CRDSdata = numpy.genfromtxt(openinputfile, delimiter=',', dtype=None, names=True,encoding='ascii')
        except:
            sampletimes = numpy.genfromtxt(opensampletimefile, delimiter=',', dtype=None, names=True)
            CRDSdata = numpy.genfromtxt(openinputfile, delimiter=',', dtype=None, names=True)
        # Need to convert to a 2d array if only one sample
        if len(sampletimes.shape) == 0:
           sampletimes = numpy.array([sampletimes])

        print ("amount samples : "+str(len(sampletimes['SampleName'])))
        amountDataLines=len(CRDSdata['Epoch_Time'])
        print ("amount datalines : "+str(amountDataLines))

        # create output file for csv results, fill the column names
        resultswriter = csv.writer(openresultsfile, dialect='excel')
        colNames=['SampleName', 'Rundate','Runtime', 'Position']
        for var in varsToSummarize:
            colNames.append(var+'_mean')
            if doSlopeIntercept.get()>0:
                colNames.extend([var+'_slope',var+'_intercept'])
            if doSaveStdev.get()>0: 
                colNames.append(var+'_stdev')
        for var in varsToSummarizeStatic:
            colNames.append(var)
        resultswriter.writerow(colNames)

        # get some input and create counters
        stabilizesec=float(pretimeentry.get())
        sampletimesec=float(sampletimeentry.get())*60        
        # just a counter c1 for keeping track of where we are in the samplelist file
        c1=0 
        # just a counter c2 for keeping track of where we are in the Picarro results list
        c2=0

        # from here do for every sample recorded in SAMs log:
        for row in sampletimes['SampleName']:  
            # create some empty variables to fill with data
            sampleDict = defaultdict()
            sampleDict['xsec']=[]; sampleDict['xsecs']=[]
            for var in varsToSummarize:
                sampleDict[var]=[]
                sampleDict[var+'s']=[]
            for var in varsToSummarizeStatic:
                sampleDict[var]=[] 
                sampleDict[var+'s']=[]            
             
            # sort out the time at which the needle was injected into the vial 
            samplestartstr=str(sampletimes['Year'][c1])+" "+str(sampletimes['Month'][c1])+" "+str(sampletimes['Day'][c1])+" "+str(sampletimes['Hour'][c1])+" "+str(sampletimes['Minute'][c1]) +" "+str(sampletimes['Second'][c1])
            samplestructtime=time.strptime(samplestartstr, "%Y %m %d %H %M %S")
            sampleepochtime=calendar.timegm(samplestructtime)
            if UTCAuto.get()==1:
                UTCOff=autoUTCOffset
            else:
                UTCOff=int(UTCoffsetentry.get())
                  
            sampleepochtime=sampleepochtime-UTCOff*3600
            print sampletimes['SampleName'][c1]
            print time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)
            print ("epoch: " + str(sampleepochtime))
            
            while True:
                if c2+1 > amountDataLines:
                    print ("WARNING: Found no more data for this sample")
                    break
                            
                # discard data before sample is started and stabilized
                if sampleepochtime > CRDSdata['Epoch_Time'][c2]:
                    c2=c2+1
                # while the sample is filling the cavity, just record the data 
                elif sampleepochtime+stabilizesec > CRDSdata['Epoch_Time'][c2]:             
                    sampleDict['xsec'].append(CRDSdata['Epoch_Time'][c2]-sampleepochtime)
                    for var in varsToSummarize:
                        sampleDict[var].append(CRDSdata[var][c2])
                    for var in varsToSummarizeStatic:
                        sampleDict[var].append(CRDSdata[var][c2])  
                    c2=c2+1       
                # concentration is supposed to be stable, record data for analysis     
                elif sampleepochtime+stabilizesec+sampletimesec > CRDSdata['Epoch_Time'][c2]:
                    sampleDict['xsecs'].append(CRDSdata['Epoch_Time'][c2]-sampleepochtime)
                    for var in varsToSummarize:
                        sampleDict[var+'s'].append(CRDSdata[var][c2])
                    for var in varsToSummarizeStatic:
                        sampleDict[var+'s'].append(CRDSdata[var][c2]) 
                    c2=c2+1   
                # Needle is retracted, record extra data for the plots
                elif sampleepochtime+stabilizesec+sampletimesec+extraTimeToPlot > CRDSdata['Epoch_Time'][c2]:
                    sampleDict['xsec'].append(CRDSdata['Epoch_Time'][c2]-sampleepochtime)
                    for var in varsToSummarize:
                        sampleDict[var].append(CRDSdata[var][c2])
                    for var in varsToSummarizeStatic:
                        sampleDict[var].append(CRDSdata[var][c2])               
                    c2=c2+1
                # done
                else:
                    break

                           
            print 'amount data points for this sample:' + str(len(sampleDict['xsec']))
            rundate=time.strftime("%Y%m%d", samplestructtime)
            runtime=time.strftime("%H%M%S", samplestructtime)
            
            # start a row of data to be written to the csv later
            dataRow=[sampletimes['SampleName'][c1],rundate,runtime, sampletimes['Position'][c1]]
            
            # if data is available for this sample
            if len(sampleDict['xsec'])>10:
                # calculate mean and stdev
                for var in varsToSummarize:
                    sampleDict[var+'mean']=numpy.mean(sampleDict[var+'s'])
                    sampleDict[var+'stdev']=numpy.std(sampleDict[var+'s'])
                for var in varsToSummarizeStatic:
                    sampleDict[var+'mean']=numpy.mean(sampleDict[var+'s'])
                
                # add data to the row dependent on chosen options
                for var in varsToSummarize:
                    dataRow.append(sampleDict[var+'mean'])
                    if doSlopeIntercept.get()>0:
                        sampleDict[var+'slope'],sampleDict[var+'intercept'],linr,linp,stderr=stats.linregress(sampleDict['xsecs'],sampleDict[var+'s'])
                        dataRow.extend([sampleDict[var+'slope'],sampleDict[var+'intercept']]) 
                    if doSaveStdev.get()>0: 
                        dataRow.append(sampleDict[var+'stdev'])                                                
                for var in varsToSummarizeStatic:
                    dataRow.append(sampleDict[var+'mean'])               
                # write the row to the csv
                resultswriter.writerow(dataRow)
                
                # start preparing the variables for plotting in the pdfs
                xs = numpy.array(sampleDict['xsecs']) 
                x = numpy.array(sampleDict['xsec'])               
                i=0; y=defaultdict(); ys=defaultdict();
                for var in varsToChart:
                    ys[var]=numpy.array(sampleDict[var+"s"])
                    y[var]=numpy.array(sampleDict[var])
                    i=i+1
           
                #______________ SAMPLE ONLY PDF_______________________________
                fig = P.figure(figsize=(15, 21))         
                chartSeries=defaultdict()
                i=1
                for var in varsToChart:
                    subPlot=len(varsToChart)*100+10+i;
                    chartSeries[var]=fig.add_subplot(subPlot)
                    print ("_________________xs______________:")
                    print len(xs)
                    print xs
                    print ("_________________ys[var]______________:")
                    print var
                    print len(ys[var] )
                    print ys[var]                 
                    
                    chartSeries[var].scatter(xs,ys[var])
                    chartSeries[var].set_xlim(left=0)
                    chartSeries[var].grid()
                    chartSeries[var].set_ylabel(var, color='b')
                    if i==1:
                        chartSeries[var].set_title('Sample Name: '+str(sampletimes['SampleName'][c1])+' position:'+str(sampletimes['Position'][c1])+' time:  '+time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)) 
                    if i==len(varsToChart):
                        chartSeries[var].set_xlabel('time (seconds)', color='b')
                    
                    # set axis to not not have an offset nor scientific notation
                    chartSeries[var].ticklabel_format(useOffset=False,style='plain')    
                        
                    i=i+1                    
                pdffile1.savefig(dpi=150)
                P.close('all')
                     
                #________________________ WHOLE RUN PDF_______________________________
                fig = P.figure(figsize=(15, 21))
                i=1
                for var in varsToChart:
                    subPlot=len(varsToChart)*100+10+i;
                    chartSeries[var]=fig.add_subplot(subPlot)
                    chartSeries[var].scatter(xs,ys[var])
                    chartSeries[var].scatter(x,y[var], marker='+')
                    chartSeries[var].set_xlim(left=0)
                    chartSeries[var].grid()
                    chartSeries[var].set_ylabel(var, color='b')
                    if i==1:
                        chartSeries[var].set_title('Sample Name: '+str(sampletimes['SampleName'][c1])+' position:'+str(sampletimes['Position'][c1])+' time:  '+time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)) 
                    if i==len(varsToChart):
                        chartSeries[var].set_xlabel('time (seconds)', color='b')
                    i=i+1              
                pdffile2.savefig(dpi=150)
                P.close('all')
                
            # if there is no data for this sample (either an analyzer crash, but more likely an timezone issue), write na
            else:
                for var in varsToSummarize:
                    dataRow.append('na')
                    if doSlopeIntercept.get()>0:
                        dataRow.extend(['na','na']) 
                    if doSaveStdev.get()>0: 
                        dataRow.append('na')                                                 
                for var in varsToSummarizeStatic:
                    dataRow.append('na') 
                print 'NO DATA FOUND FOR THIS SAMPLE'
                # write the row to the csv
                resultswriter.writerow(dataRow)
            #print separator between samples            
            print '---------------NEXT SAMPLE-------------------------------'   
            c1=c1+1
    
    openinputfile.close()
    openresultsfile.close()
    pdffile1.close()
    pdffile2.close()
    print '---------------DONE-------------------------------'

#____________________________________________________________________________________________________________
#--------------------GUI-------------------------------------------------------------------------------------
#____________________________________________________________________________________________________________

root = Tk()
root.title(GUIWindowTitle)
#__________________________________LOGO&TITLE________________________________________
bigtitle = Label(root, anchor=W, font=('times', 20, 'bold'), fg='white',bg='blue', text=GUITitle)
bigtitle.grid(row=0,column=0,columnspan=10,sticky=[N,S,E,W])
#____________________________OPTIONS______________________________________________________
optionstitle = Label(root, anchor=W, font=('times', 12, 'bold'), text="options:")
optionstitle.grid(row=1,column=0, columnspan=3, sticky=[N,S,E,W])

pretimeentrytitle = Label(root, anchor=W, text="stabilizing time to ignore at start (s):")
pretimeentrytitle.grid(row=3,column=0, columnspan=1, sticky=[E])
pretimeentry= Entry(root,width=4)
pretimeentry.insert(0,stabilizingTimeToIgnore)
pretimeentry.grid(row=3,column=1, columnspan=1, sticky=[W])

sampletimeentrytitle = Label(root, anchor=W, text="sampling time to include (min):")
sampletimeentrytitle.grid(row=4,column=0, columnspan=1, sticky=[E])
sampletimeentry= Entry(root,width=4)
sampletimeentry.insert(0,analysisTimeMinutes)
sampletimeentry.grid(row=4,column=1, columnspan=1, sticky=[W])

try:
    autoUTCOffset= int(-time.timezone)
    autoUTCOffset=autoUTCOffset/60/60
except:
    autoUTCOffset="error"
    
def activateUTCEntry():
    if UTCAuto.get()==1:
        UTCoffsetentry.config(state=DISABLED)
    else:
        UTCoffsetentry.config(state=NORMAL)      
UTCAuto= IntVar()
doUTCAuto = Checkbutton(root, text="Automatically detect UTC offset",command=activateUTCEntry, variable=UTCAuto)
doUTCAuto.select()
doUTCAuto.grid(row=12,column=0, columnspan=1, sticky=W)
UTCautoOffset= Label(root, anchor=W, text="detected offset:"+str(autoUTCOffset))
UTCautoOffset.grid(row=12,column=1, columnspan=1, sticky=[W])

UTCoffsettitle = Label(root, anchor=W, text="Manual entry for offset local time UTC :")
UTCoffsettitle.grid(row=13,column=0, columnspan=1, sticky=[E])
UTCoffsetentry= Entry(root,width=4,state=DISABLED)
UTCoffsetentry.insert(0,offsetUTC)
UTCoffsetentry.grid(row=13,column=1, columnspan=1, sticky=[W])

doSlopeIntercept=IntVar()
doSlopeInterceptApply = Checkbutton(root, text="Save slope and intercept", variable=doSlopeIntercept)
if alwaysDoSlope>0:
    doSlopeInterceptApply.select()
doSlopeInterceptApply.grid(row=14,column=0, columnspan=1, sticky=W)

doSaveStdev=IntVar()
doSaveStdevApply = Checkbutton(root, text="Record Stdev", variable=doSaveStdev)
if alwaysDoStdev>0:
    doSaveStdevApply.select()
doSaveStdevApply.grid(row=15,column=0, columnspan=1, sticky=W)


f0=Frame(root,height=1, width=450, bg="grey")
f0.grid(row=24,column=0, columnspan=4, pady=5,sticky=S)

calcfluxtitle = Label(root, anchor=W, font=('times', 12, 'bold'), text="Calculate results")
calcfluxtitle.grid(row=25,column=0, columnspan=4, sticky=[N,S,E,W])

buttonopenconcfile=Button(root, text='open sampletime file', command=askopenresultsfilename)
buttonopenconcfile.grid(row=28,column=1,columnspan=1,sticky=[W])
calcfluxhelp3 = Label(root, anchor=W, text="results are saved in "+str(resultsfoldername))
calcfluxhelp3.grid(row=29,column=0, columnspan=4, sticky=[N,S,E,W])

# #___________This starts the program__________________________________________________________________________
root.mainloop(  )



