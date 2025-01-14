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
from scipy import optimize
from scipy import linspace
import math
import pylab as P
from matplotlib.backends.backend_pdf import PdfPages
import fnmatch

#confidence interval for linear regression analysis
confidence_interval=90.0

resultsfoldername = 'C:\DATA_TOOLS_UOFS'

def MergeResults():
   global sampletimefilename
   global mergedresultsfilename
   global tempfilename
   
   inputfoldername = 'C:\UserData\DataLog_User'
   opensampletimefile=open(sampletimefilename, 'rb')
   sampletimes = numpy.genfromtxt(opensampletimefile, delimiter=',', dtype=None, names=True)
   
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
   mergedresultsfilename=datetime.datetime.now().strftime('%Y%m%d_%H%M%S') + '_merged_results.txt'
   openresultsfile=open(mergedresultsfilename, 'wb')
   resultswriter = csv.writer(openresultsfile, delimiter='\t')
   
   resultswriter.writerow(['Date','Local_Time','Epoch_Time','N2O_ppbv','d15N', 'd15Nalpha', 'd15Nbeta','d18O', 'CO2_ppmv','OutletValve'])
   os.chdir(inputfoldername)
      
   for dirpath, dirs, files in os.walk(inputfoldername):
      for filename in fnmatch.filter(files, '*.dat'):
        openinputfile=open(os.path.join(dirpath, filename),'rb')
        #print filename
        junk,datestr,junk2=filename.split("-",2)
        #print datestr
        YMD=int(datestr)
        yearoffile=YMD/10000
        monthoffile=(YMD-yearoffile*10000)/100
        dayoffile=YMD-yearoffile*10000-monthoffile*100

        filetimestr=str(yearoffile)+" "+str(monthoffile)+" "+str(dayoffile)+" 00 00 00"
        filestructtime=time.strptime(filetimestr, "%Y %m %d %H %M %S")
        fileepochtime=calendar.timegm(filestructtime)

        if fileepochtime > firstsampletime-86400 and fileepochtime < lastsampletime+86400:
            try:
                fd = numpy.genfromtxt(openinputfile, dtype=None, names=True)
                c1=0
                for row in fd['TIME']:
                   temptime,junk=row.split(".",1)
                   datetimestr=str(fd['DATE'][c1])+" "+str(temptime)
                   tempstructtime=time.strptime(datetimestr, "%Y-%m-%d %H:%M:%S")
                   tempepochtime=time.mktime(tempstructtime)
                   tempepochtime=tempepochtime
                   tempstructtime=time.localtime(tempepochtime)
                   
                   gooddate=time.strftime("%Y-%m-%d", tempstructtime)
                   goodtime=(time.strftime("%H:%M:%S", tempstructtime))

                   resultswriter.writerow([gooddate,goodtime,fd['EPOCH_TIME'][c1],fd['N2O'][c1],fd['d15N'][c1],fd['d15Nalpha'][c1],fd['d15Nbeta'][c1],fd['d18O'][c1],fd['co2_corr'][c1],fd['OutletValve'][c1]])
                   c1=c1+1 
            except:
                print("error reading"+ filename)
        openinputfile.close()
         
   openresultsfile.close()
   
def askopenresultsfilename():
   global sampletimefilename  # file with the sample names and times (switcherlog)
   global mergedresultsfilename
   global tempfilename
      
   # get filename
   fileopen_opt = options = {}
   options['defaultextension'] = '.csv' 
   options['filetypes'] = [('csv files', '.csv'),('all files', '.*')]
   options['initialdir'] = 'C:\OpenSampler'
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

      sampletimes = numpy.genfromtxt(opensampletimefile, delimiter=',', dtype=None, names=True)
      print "amount samples"      
      print len(sampletimes['SampleName'])
      
      iN2Odata = numpy.genfromtxt(openinputfile, delimiter='\t', dtype=None, names=True)
      print "amount datalines"    
      print len(iN2Odata['Epoch_Time'])
      amountrows=len(iN2Odata['Epoch_Time'])
      
      resultswriter = csv.writer(openresultsfile, dialect='excel')
      resultswriter.writerow(['SampleName', 'Rundate','Runtime', 'Position', 'N2Omean_ppbv', 
      'N2Oslope', 'N2Ointercept','d15Nmean','d15Nslope','d15Nintercept', 'd15Amean',
      'd15Aslope', 'd15Aintercept','d15Bmean', 'd15Bslope', 'd15Bintercept',
      'd18Omean','d18Ointercept','d18Oslope','CO2mean_ppbv','OutletValvemean'])
      stabilizesec=float(pretimeentry.get())
      sampletimesec=float(sampletimeentry.get())*60        
      
      # just a counter c1 for keeping track of where we are in the samplelist file
      c1=0 
      # just a counter c2 for keeping track of where we are in the results file
      c2=0
      
      for row in sampletimes['SampleName']:
         xsec=[]; yN2O=[]; yd15N=[]; yd15A=[]; yd15B=[]; yd18O=[]; yCO2=[]; yOutletValve=[];
         xsecs=[]; yN2Os=[]; yd15Ns=[]; yd15As=[]; yd15Bs=[]; yd18Os=[]; yCO2s=[]; yOutletValves=[];
         
         samplestartstr=str(sampletimes['Year'][c1])+" "+str(sampletimes['Month'][c1])+" "+str(sampletimes['Day'][c1])+" "+str(sampletimes['Hour'][c1])+" "+str(sampletimes['Minute'][c1]) +" "+str(sampletimes['Second'][c1])
         samplestructtime=time.strptime(samplestartstr, "%Y %m %d %H %M %S")
         #sampleepochtime=time.mktime(samplestructtime)
         sampleepochtime=calendar.timegm(samplestructtime)
         sampleepochtime=sampleepochtime-(int(UTCoffsetentry.get())*3600)
         print sampletimes['SampleName'][c1]
         print time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)
         print sampleepochtime
         # discard data before sample is started and stabilized
         while sampleepochtime > iN2Odata['Epoch_Time'][c2]:
            c2=c2+1 
         while sampleepochtime+stabilizesec > iN2Odata['Epoch_Time'][c2]:
            xsec.append(iN2Odata['Epoch_Time'][c2]-sampleepochtime)
            yN2O.append(iN2Odata['N2O_ppbv'][c2])
            yd15N.append(iN2Odata['d15N'][c2])
            yd15A.append(iN2Odata['d15Nalpha'][c2])
            yd15B.append(iN2Odata['d15Nbeta'][c2])
            yd18O.append(iN2Odata['d18O'][c2])
            yCO2.append(iN2Odata['CO2_ppmv'][c2])
            yOutletValve.append(iN2Odata['OutletValve'][c2])
            c2=c2+1            
         while sampleepochtime+stabilizesec+sampletimesec > iN2Odata['Epoch_Time'][c2]:
            xsecs.append(iN2Odata['Epoch_Time'][c2]-sampleepochtime)
            yN2Os.append(iN2Odata['N2O_ppbv'][c2])
            yd15Ns.append(iN2Odata['d15N'][c2])
            yd15As.append(iN2Odata['d15Nalpha'][c2])
            yd15Bs.append(iN2Odata['d15Nbeta'][c2])
            yd18Os.append(iN2Odata['d18O'][c2])
            yCO2s.append(iN2Odata['CO2_ppmv'][c2])
            yOutletValves.append(iN2Odata['OutletValve'][c2])
            c2=c2+1
         while sampleepochtime+stabilizesec+sampletimesec+120 > iN2Odata['Epoch_Time'][c2]:
            xsec.append(iN2Odata['Epoch_Time'][c2]-sampleepochtime)
            yN2O.append(iN2Odata['N2O_ppbv'][c2])
            yd15N.append(iN2Odata['d15N'][c2])
            yd15A.append(iN2Odata['d15Nalpha'][c2])
            yd15B.append(iN2Odata['d15Nbeta'][c2])
            yd18O.append(iN2Odata['d18O'][c2])
            yCO2.append(iN2Odata['CO2_ppmv'][c2])
            yOutletValve.append(iN2Odata['OutletValve'][c2])
            c2=c2+1   
         c2=0
         print 'amount readings for this sample:' + str(len(yN2Os))
         rundate=time.strftime("%Y%m%d", samplestructtime)
         runtime=time.strftime("%H%M%S", samplestructtime)
         
         if len(yN2Os)>2:
            N2Omean=numpy.mean(yN2Os)
            d15Nmean=numpy.mean(yd15Ns)
            d15Amean=numpy.mean(yd15As)
            d15Bmean=numpy.mean(yd15Bs)
            d18Omean=numpy.mean(yd18Os)
            CO2mean=numpy.mean(yCO2s)
            OutletValvemean=numpy.mean(yOutletValves)
            
            N2Oslope, N2Ointercept, N2Olinr, N2Olinp, N2Ostd_err = stats.linregress(xsecs,yN2Os)
            d15Nslope, d15Nintercept, d15Nlinr, d15Nlinp, d15Nstd_err = stats.linregress(xsecs,yd15Ns)
            d15Aslope, d15Aintercept, d15Alinr, d15Alinp, d15Astd_err = stats.linregress(xsecs,yd15As)
            d15Bslope, d15Bintercept, d15Blinr, d15Blinp, d15Bstd_err = stats.linregress(xsecs,yd15Bs)
            d18Oslope, d18Ointercept, d18Olinr, d18Olinp, d18Ostd_err = stats.linregress(xsecs,yd18Os)
            
            resultswriter.writerow([sampletimes['SampleName'][c1],rundate,runtime, sampletimes['Port'][c1],
            N2Omean, N2Oslope, N2Ointercept, d15Nmean, d15Nslope, d15Nintercept, d15Amean, d15Aslope, d15Aintercept, d15Bmean, d15Bslope, d15Bintercept,
            d18Omean, d18Ointercept, d18Oslope, CO2mean, OutletValvemean])
                     
            #______________ SAMPLE ONLY PDF_______________________________
            fig = P.figure(figsize=(16, 16))
            xs = numpy.array(xsecs)            
            y1s = numpy.array(yN2Os)
            y2s = numpy.array(yd15As)
            y3s = numpy.array(yd15Bs)
            y4s = numpy.array(yd18Os)
            y5s = numpy.array(yOutletValves)
            
            x = numpy.array(xsec)            
            y1 = numpy.array(yN2O)
            y2 = numpy.array(yd15A)
            y3 = numpy.array(yd15B)
            y4 = numpy.array(yd18O)
            y5 = numpy.array(yOutletValve)
            
            # (m,b)=P.polyfit(xs,y1s,1)
            # y12 = P.polyval([m,b],x)
            # (m,b)=P.polyfit(xs,y2s,1)
            # y22 = P.polyval([m,b],x)
            # (m,b)=P.polyfit(xs,y3s,1)
            # y32 = P.polyval([m,b],x)
            # (m,b)=P.polyfit(xs,y4s,1)
            # y42 = P.polyval([m,b],x)
            # (m,b)=P.polyfit(xs,y5s,1)
            # y52 = P.polyval([m,b],x) 
 
            line1=fig.add_subplot(511)
            line1.scatter(xs, y1s)
            line1.set_xlim(left=0)       
            line1.grid()
            line1.set_title('Sample Name: '+str(sampletimes['SampleName'][c1])+'        time:  '+time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)) 
            line1.set_ylabel('N2O concentration (ppbv)', color='b')
            
            line2=fig.add_subplot(512)
            line2.scatter(xs, y2s)
            line2.set_xlim(left=0)
            line2.grid() 
            line2.set_ylabel('d15N alpha', color='b')
                              
            line3=fig.add_subplot(513)
            line3.scatter(xs, y3s)
            line3.set_xlim(left=0)
            line3.grid()
            line3.set_ylabel('d15N beta', color='b')
            
            line4=fig.add_subplot(514)
            line4.scatter(xs, y4s)
            line4.set_xlim(left=0)
            line4.grid()
            line4.set_ylabel('d18O', color='b')
            
            line5=fig.add_subplot(515)
            line5.scatter(xs, y5s)
            line5.set_xlim(left=0)
            line5.grid()
            line5.set_ylabel('OutletValve', color='b')
            
            line5.set_xlabel('time (seconds)', color='b')
            pdffile1.savefig(dpi=150)
            P.close()
            
            
            #________________________ WHOLE RUN PDF_______________________________
            fig = P.figure(figsize=(16, 16))
            
            line1=fig.add_subplot(511)
            line1.scatter(xs, y1s)
            line1.scatter(x, y1, marker='+')
            line1.set_xlim(left=0)         
            line1.grid()
            line1.set_title('Sample Name: '+str(sampletimes['SampleName'][c1])+'        time:  '+time.strftime("%d %b %Y %H:%M:%S ", samplestructtime)) 
            line1.set_ylabel('N2O concentration (ppbv)', color='b')
            
            line2=fig.add_subplot(512)
            line2.scatter(xs, y2s)
            line2.scatter(x, y2, marker='+')
            line2.set_xlim(left=0)
            line2.grid() 
            line2.set_ylabel('d15N alpha', color='b')
                              
            line3=fig.add_subplot(513)
            line3.scatter(xs, y3s)
            line3.scatter(x, y3, marker='+')
            line3.set_xlim(left=0)
            line3.grid()
            line3.set_ylabel('d15N beta', color='b')

            line4=fig.add_subplot(514)
            line4.scatter(xs, y4s)
            line4.scatter(x, y4, marker='+')
            line4.set_xlim(left=0)
            line4.grid()
            line4.set_ylabel('d18O', color='b')
            
            line5=fig.add_subplot(515)
            line5.scatter(xs, y5s)
            line5.scatter(x, y5, marker='+')
            line5.set_xlim(left=0)
            line5.grid()
            line5.set_ylabel('OutletValve', color='b')            
            line5.set_xlabel('time (seconds)', color='b')
            pdffile2.savefig(dpi=150)
            P.close()
   
         else:
            resultswriter.writerow([sampletimes['SampleName'][c1],rundate,runtime, sampletimes['Port'][c1],
               'na', 'na', 'na', 'na','na','na','na', 'na', 'na', 'na','na', 'na', 'na','na'])
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
root.title('iN2O results calculator 20150911')

#__________________________________LOGO&TITLE________________________________________

bigtitle = Label(root, anchor=W, font=('times', 20, 'bold'), fg='white',bg='darkgreen', text="iN2O 15N 18O calculator ")
bigtitle.grid(row=0,column=0,columnspan=10,sticky=[N,S,E,W])

#____________________________OPTIONS______________________________________________________
optionstitle = Label(root, anchor=W, font=('times', 12, 'bold'), text="options:")
optionstitle.grid(row=1,column=0, columnspan=3, sticky=[N,S,E,W])


pretimeentrytitle = Label(root, anchor=W, text="stabilizing time to ignore at start (s):")
pretimeentrytitle.grid(row=3,column=0, columnspan=1, sticky=[E])
pretimeentry= Entry(root,width=4)
pretimeentry.insert(0,"270")
pretimeentry.grid(row=3,column=1, columnspan=1, sticky=[W])

sampletimeentrytitle = Label(root, anchor=W, text="sampling time to include (min):")
sampletimeentrytitle.grid(row=4,column=0, columnspan=1, sticky=[E])
sampletimeentry= Entry(root,width=4)
sampletimeentry.insert(0,"5")
sampletimeentry.grid(row=4,column=1, columnspan=1, sticky=[W])

UTCoffsettitle = Label(root, anchor=W, text="Offset local time UTC (SK: -6):")
UTCoffsettitle.grid(row=13,column=0, columnspan=1, sticky=[E])
UTCoffsetentry= Entry(root,width=4)
UTCoffsetentry.insert(0,"-6")
UTCoffsetentry.grid(row=13,column=1, columnspan=1, sticky=[W])

# _______________________CALC INDIVIDUAL FLUXES_____________________________________________
f0=Frame(root,height=1, width=450, bg="grey")
f0.grid(row=24,column=0, columnspan=4, pady=5,sticky=S)

calcfluxtitle = Label(root, anchor=W, font=('times', 12, 'bold'), text="Calculate results")
calcfluxtitle.grid(row=25,column=0, columnspan=4, sticky=[N,S,E,W])
calcfluxhelp = Label(root, anchor=W, text="Open a merged results file")
calcfluxhelp.grid(row=26,column=0, columnspan=4, sticky=[N,S,E,W])
calcfluxhelp2 = Label(root, anchor=W, text="input concentrations in ppbv (=nl/l)")
calcfluxhelp2.grid(row=27,column=0, columnspan=4, sticky=[N,S,E,W])

buttonopenconcfile=Button(root, text='open sampletime file', command=askopenresultsfilename)
buttonopenconcfile.grid(row=28,column=1,columnspan=1,sticky=[W])
calcfluxhelp3 = Label(root, anchor=W, text="results are saved in data_tools_uofs")
calcfluxhelp3.grid(row=29,column=0, columnspan=4, sticky=[N,S,E,W])

# #_____________________________________________________________________________________________________________

root.mainloop(  )



