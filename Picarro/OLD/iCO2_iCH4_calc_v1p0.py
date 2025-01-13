#!/usr/bin/python2

from Tkinter import *
import tkFileDialog
import tkMessageBox

import os
import string
import time
import datetime
import csv
import numpy
import calendar
from scipy import stats
#from scipy import optimize
#from scipy import linspace
import math
import pylab as P
from matplotlib.backends.backend_pdf import PdfPages
import fnmatch
from collections import defaultdict

resultsfoldername = 'C:\SAM\DATATOOLS'
samplelogfoldername = 'C:\SAM\SAMPLELOG'
inputfoldername = 'C:\UserData'


# this is the list of Picarro variable to report on (with slope and intercept if chosen)
varsToSummarize=['HP_12CH4','HP_13CH4','HP_Delta_iCH4_Raw','HR_12CH4','HR_13CH4','HR_Delta_iCH4_Raw', 
   '12CO2','13CO2','Delta_Raw_iCO2']
varsToSummarizeStatic=['H2O', 'OutletValve', 'ChemDetect']

varsToChart=['12CO2','Delta_Raw_iCO2','HR_12CH4','HR_Delta_iCH4_Raw','OutletValve']
amountCharts=len(varsToChart)

def MergeResults():
    global sampletimefilename
    global mergedresultsfilename
    global tempfilename

    opensampletimefile=open(sampletimefilename, 'rb')
    try:
        sampletimes = numpy.genfromtxt(opensampletimefile, delimiter=',', dtype=None, names=True, deletechars="~!@#$%^&*()-=+~\|]}[{';: /?.>,<.",encoding='ascii')
    except:
        sampletimes = numpy.genfromtxt(opensampletimefile, delimiter=',', dtype=None, names=True, deletechars="~!@#$%^&*()-=+~\|]}[{';: /?.>,<.")
    # KNOWN BUG ONLY WORKS IF MORE THAN ONE SAMPLES
    print "amount samples to be merged:" 
    print len(sampletimes['SampleName'])

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
    print sampleepochtimes
       
    os.chdir(resultsfoldername)
    mergedresultsfilename=datetime.datetime.now().strftime('%Y%m%d_%H%M%S') + '_merged_results.csv'
    openresultsfile=open(mergedresultsfilename, 'wb')
    resultswriter = csv.writer(openresultsfile, delimiter=',')

    colNames=['Date','Local_Time','Epoch_Time']
    for var in varsToSummarize:
        colNames.append(var)
    for var in varsToSummarizeStatic:
        colNames.append(var)
    resultswriter.writerow(colNames)
    os.chdir(inputfoldername)
      
      
      

    # for root, dirs, files in os.walk("/mydir"):
        # for file in files:
            # if file.endswith(".txt"):
                # print(os.path.join(root, file))  
    for dirpath, dirs, files in os.walk(inputfoldername):
        for file in files:
            if file.endswith(".dat"):
                junk,datestr,timestr,junk2=file.split("-",3)
                filetimestr=datestr+ timestr[0:6]
                print(filetimestr)
                filestructtime = time.strptime(filetimestr, '%Y%m%d%H%M%S')
                print(filestructtime)
                fileepochtime=calendar.timegm(filestructtime)

                print ("file: "+file)
                print ("datestr: " +filetimestr) 
                print("fileepochtime :" +str(fileepochtime))
                print("firstsampletime :" +str(firstsampletime))
                print("lastsampletime :" +str(lastsampletime))
                if fileepochtime > firstsampletime-86400 and fileepochtime < lastsampletime+86400:
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
                            tempepochtime=tempepochtime+(int(UTCoffsetentry.get())*3600)
                            #tempstructtime=time.localtime(tempepochtime)

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
                    except:
                       print "could not read: " + file
         
         
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
      print "amount samples"      
      print len(sampletimes['SampleName'])
      print "amount datalines"    
      print len(CRDSdata['Epoch_Time'])
      amountrows=len(CRDSdata['Epoch_Time'])
      
      resultswriter = csv.writer(openresultsfile, dialect='excel')
      colNames=['SampleName', 'Rundate','Runtime', 'Position']
      if doSlopeIntercept.get()>0:
        for var in varsToSummarize:
            colNames.append(var+'_mean')
            colNames.append(var+'_slope')
            colNames.append(var+'_intercept')
        for var in varsToSummarizeStatic:
            colNames.append(var)
      else:
        for var in varsToSummarize:
            colNames.append(var)
        for var in varsToSummarizeStatic:
            colNames.append(var)  
      resultswriter.writerow(colNames)

      stabilizesec=float(pretimeentry.get())
      sampletimesec=float(sampletimeentry.get())*60        
      
      # just a counter c1 for keeping track of where we are in the samplelist file
      c1=0 
      # just a counter c2 for keeping track of where we are in the results file
      c2=0
      
      for row in sampletimes['SampleName']:     
        sampleDict = defaultdict()
        sampleDict['xsec']=[]; sampleDict['xsecs']=[]
        for var in varsToSummarize:
            sampleDict[var]=[]
            sampleDict[var+'s']=[]
        for var in varsToSummarizeStatic:
            sampleDict[var]=[] 
            sampleDict[var+'s']=[]            
         
         
        samplestartstr=str(sampletimes['Year'][c1])+" "+str(sampletimes['Month'][c1])+" "+str(sampletimes['Day'][c1])+" "+str(sampletimes['Hour'][c1])+" "+str(sampletimes['Minute'][c1]) +" "+str(sampletimes['Second'][c1])
        samplestructtime=time.strptime(samplestartstr, "%Y %m %d %H %M %S")
        #sampleepochtime=time.mktime(samplestructtime)
        sampleepochtime=calendar.timegm(samplestructtime)
        #sampleepochtime=sampleepochtime-(int(UTCoffsetentry.get())*3600)
        print sampletimes['SampleName'][c1]
        print time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)
        print sampleepochtime
        # discard data before sample is started and stabilized
        while sampleepochtime > CRDSdata['Epoch_Time'][c2]:
            c2=c2+1 
        while sampleepochtime+stabilizesec > CRDSdata['Epoch_Time'][c2]:
            
            sampleDict['xsec'].append(CRDSdata['Epoch_Time'][c2]-sampleepochtime)
            for var in varsToSummarize:
                sampleDict[var].append(CRDSdata[var][c2])
            for var in varsToSummarizeStatic:
                sampleDict[var].append(CRDSdata[var][c2]) 
            
            c2=c2+1            
        while sampleepochtime+stabilizesec+sampletimesec > CRDSdata['Epoch_Time'][c2]:
            sampleDict['xsecs'].append(CRDSdata['Epoch_Time'][c2]-sampleepochtime)
            for var in varsToSummarize:
                sampleDict[var+'s'].append(CRDSdata[var][c2])
            for var in varsToSummarizeStatic:
                sampleDict[var+'s'].append(CRDSdata[var][c2]) 
            
            c2=c2+1   
        while sampleepochtime+stabilizesec+sampletimesec+120 > CRDSdata['Epoch_Time'][c2]:
            sampleDict['xsec'].append(CRDSdata['Epoch_Time'][c2]-sampleepochtime)
            for var in varsToSummarize:
                sampleDict[var].append(CRDSdata[var][c2])
            for var in varsToSummarizeStatic:
                sampleDict[var].append(CRDSdata[var][c2]) 
            
            c2=c2+1   
        c2=0
        print 'amount readings for this sample:' + str(len(sampleDict['xsec']))
        rundate=time.strftime("%Y%m%d", samplestructtime)
        runtime=time.strftime("%H%M%S", samplestructtime)
         
        if len(sampleDict['xsec'])>2:
            #xsecs=[]; y12CH4s=[]; y13CH4s=[]; yd13CCH4s=[]; y12CO2s=[]; y13CO2s=[]; yd13CCO2s=[]; yH2Os=[]; yChemDetect
            for var in varsToSummarize:
                sampleDict[var+'mean']=numpy.mean(sampleDict[var+'s'])
            for var in varsToSummarizeStatic:
                sampleDict[var+'mean']=numpy.mean(sampleDict[var+'s'])

            dataRow=[sampletimes['SampleName'][c1],rundate,runtime, sampletimes['Position'][c1]]
            
            if doSlopeIntercept.get()>0:       
                for var in varsToSummarize:
                    sampleDict[var+'slope'],sampleDict[var+'intercept'],linr,linp,stderr=stats.linregress(sampleDict['xsecs'],sampleDict['y'+var+'s'])
                    dataRow.append(sampleDict[var+'mean'],sampleDict[var+'slope'],sampleDict[var+'intercept'])
                for var in varsToSummarizeStatic:
                    dataRow.append(sampleDict[var+'mean'])
            
            else:
                for var in varsToSummarize:
                    dataRow.append(sampleDict[var+'mean'])
                for var in varsToSummarizeStatic:
                    dataRow.append(sampleDict[var+'mean'])               
            
            resultswriter.writerow(dataRow)
            
            
            xs = numpy.array(sampleDict['xsecs'])   
            y1s = numpy.array(sampleDict['12CO2s'])
            y2s = numpy.array(sampleDict['Delta_Raw_iCO2s'])
            y3s = numpy.array(sampleDict['HR_12CH4s'])
            y4s= numpy.array(sampleDict['HR_Delta_iCH4_Raws'])
            y5s = numpy.array(sampleDict['OutletValves'])
            
            x = numpy.array(sampleDict['xsec'])   
            y1 = numpy.array(sampleDict['12CO2'])
            y2 = numpy.array(sampleDict['Delta_Raw_iCO2'])
            y3 = numpy.array(sampleDict['HR_12CH4'])
            y4= numpy.array(sampleDict['HR_Delta_iCH4_Raw'])
            y5 = numpy.array(sampleDict['OutletValve'])
            
            
            #______________ SAMPLE ONLY PDF_______________________________
            fig = P.figure(figsize=(16, 16))
            
            line1=fig.add_subplot(511)
            line1.scatter(xs, y1s)
            line1.set_xlim(left=0)       
            line1.grid()
            line1.set_title('Sample Name: '+str(sampletimes['SampleName'][c1])+' time:  '+time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)) 
            line1.set_ylabel('12CO2 concentration (ppmv)', color='b')
            
            line2=fig.add_subplot(512)
            line2.scatter(xs, y2s)
            line2.set_xlim(left=0)
            line2.grid() 
            line2.set_ylabel('d13C-CO2 (permil)', color='b')
                              
            line3=fig.add_subplot(513)
            line3.scatter(xs, y3s)
            line3.set_xlim(left=0)
            line3.grid()
            line3.set_ylabel('12CH4 concentration (ppmv)', color='b')
            
            line4=fig.add_subplot(514)
            line4.scatter(xs, y4s)
            line4.set_xlim(left=0)
            line4.grid()
            line4.set_ylabel('d13C-CH4 (permil)', color='b')
            
            line5=fig.add_subplot(515)
            line5.scatter(xs, y5s)
            line5.set_xlim(left=0)
            line5.grid()
            line5.set_ylabel('OutletValve (0=closed, 65536=open)', color='b')
            line5.set_xlabel('time (seconds)', color='b')
            pdffile1.savefig(dpi=150)
            P.close('all')
                 
            #________________________ WHOLE RUN PDF_______________________________
            fig = P.figure(figsize=(16, 16))
            
            line1=fig.add_subplot(511)
            line1.scatter(xs, y1s)
            line1.scatter(x, y1, marker='+')
            line1.set_xlim(left=0)         
            line1.grid()
            line1.set_title('Sample Name: '+str(sampletimes['SampleName'][c1])+'        time:  '+time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)) 
            line1.set_ylabel('12CO2 concentration (ppmv)', color='b')
            
            line2=fig.add_subplot(512)
            line2.scatter(xs, y2s)
            line2.scatter(x, y2, marker='+')
            line2.set_xlim(left=0)
            line2.grid() 
            line2.set_ylabel('d13C-CO2 (permil)', color='b')
                              
            line3=fig.add_subplot(513)
            line3.scatter(xs, y3s)
            line3.scatter(x, y3, marker='+')
            line3.set_xlim(left=0)
            line3.grid()
            line3.set_ylabel('12CH4 concentration (ppmv)', color='b')

            line4=fig.add_subplot(514)
            line4.scatter(xs, y4s)
            line4.scatter(x, y4, marker='+')
            line4.set_xlim(left=0)
            line4.grid()
            line4.set_ylabel('d13C-CH4 (permil)', color='b')
            
            line5=fig.add_subplot(515)
            line5.scatter(xs, y5s)
            line5.scatter(x, y5, marker='+')
            line5.set_xlim(left=0)
            line5.grid()
            line5.set_ylabel('OutletValve (0=closed, 65536=open)', color='b')
            line5.set_xlabel('time (seconds)', color='b')
            
            pdffile2.savefig(dpi=150)
            P.close('all')
   
        else:
            resultswriter.writerow([sampletimes['SampleName'][c1],rundate,runtime, sampletimes['Position'][c1],
               'na', 'na', 'na', 'na','na',
               'na','na', 'na', 'na', 'na',
               'na', 'na', 'na','na', 'na',
               'na', 'na', 'na','na', 'na'])
            print 'NO DATA FOUND FOR THIS SAMPLE'
        print '----------------------------------------------'   
        c1=c1+1
      
      openinputfile.close()
      openresultsfile.close()
      pdffile1.close()
      pdffile2.close()
#____________________________________________________________________________________________________________
#--------------------GUI-----------------------------------------------------------------------------------
#_____________________________________________________________________________________________________________

# create a root TkInter frame
root = Tk()
root.title('iCH4 iCO2 results calculator 20190624')

#__________________________________LOGO&TITLE________________________________________

bigtitle = Label(root, anchor=W, font=('times', 20, 'bold'), fg='white',bg='blue', text="UQAM iCH4 & iCO2 calculator ")
bigtitle.grid(row=0,column=0,columnspan=10,sticky=[N,S,E,W])

#____________________________OPTIONS______________________________________________________
optionstitle = Label(root, anchor=W, font=('times', 12, 'bold'), text="options:")
optionstitle.grid(row=1,column=0, columnspan=3, sticky=[N,S,E,W])


pretimeentrytitle = Label(root, anchor=W, text="stabilizing time to ignore at start (s):")
pretimeentrytitle.grid(row=3,column=0, columnspan=1, sticky=[E])
pretimeentry= Entry(root,width=4)
pretimeentry.insert(0,"240")
pretimeentry.grid(row=3,column=1, columnspan=1, sticky=[W])

sampletimeentrytitle = Label(root, anchor=W, text="sampling time to include (min):")
sampletimeentrytitle.grid(row=4,column=0, columnspan=1, sticky=[E])
sampletimeentry= Entry(root,width=4)
sampletimeentry.insert(0,"4")
sampletimeentry.grid(row=4,column=1, columnspan=1, sticky=[W])

UTCoffsettitle = Label(root, anchor=W, text="Offset local time UTC (-4 summer, -5 winter):")
UTCoffsettitle.grid(row=13,column=0, columnspan=1, sticky=[E])
UTCoffsetentry= Entry(root,width=4)
UTCoffsetentry.insert(0,"0")
UTCoffsetentry.grid(row=13,column=1, columnspan=1, sticky=[W])

doSlopeIntercept=IntVar()
doSlopeInterceptApply = Checkbutton(root, text="Save slope and intercept", variable=doSlopeIntercept)
# uncomment if you always want slope and intercepts saved
#doSlopeInterceptApply.select()
doSlopeInterceptApply.grid(row=14,column=0, columnspan=1, sticky=W)

# _______________________EXTRACT USE FULL DATA_____________________________________________
f0=Frame(root,height=1, width=450, bg="grey")
f0.grid(row=24,column=0, columnspan=4, pady=5,sticky=S)

calcfluxtitle = Label(root, anchor=W, font=('times', 12, 'bold'), text="Calculate results")
calcfluxtitle.grid(row=25,column=0, columnspan=4, sticky=[N,S,E,W])

buttonopenconcfile=Button(root, text='open sampletime file', command=askopenresultsfilename)
buttonopenconcfile.grid(row=28,column=1,columnspan=1,sticky=[W])
calcfluxhelp3 = Label(root, anchor=W, text="results are saved in c:/SAM/DATATOOLS")
calcfluxhelp3.grid(row=29,column=0, columnspan=4, sticky=[N,S,E,W])

# #_____________________________________________________________________________________________________________

root.mainloop(  )



