#!/usr/bin/env python
"""

Run and graph the results of patest_suggested_vs_streaminfo_latency.c

Requires matplotlib for plotting: http://matplotlib.sourceforge.net/

"""
import os
from pylab import *
import numpy
from matplotlib.backends.backend_pdf import PdfPages

testExeName = "PATest.exe" # rename to whatever the compiled patest_suggested_vs_streaminfo_latency.c binary is
dataFileName = "patest_suggested_vs_streaminfo_latency.csv" # code below calls the exe to generate this file

inputDeviceIndex = -1 # -1 means default
outputDeviceIndex = -1 # -1 means default
sampleRate = 44100
pdfFilenameSuffix = "_wmme"

pdfFile = PdfPages("patest_suggested_vs_streaminfo_latency_" + str(sampleRate) + pdfFilenameSuffix +".pdf") #output this pdf file


def loadCsvData( dataFileName ):
    params= ""
    inputDevice = ""
    outputDevice = ""

    startLines = file(dataFileName).readlines(1024)
    for line in startLines:
        if "output device" in line:
            outputDevice = line.strip(" \t\n\r#")
        if "input device" in line:
            inputDevice = line.strip(" \t\n\r#")
    params = startLines[0].strip(" \t\n\r#")

    data = numpy.loadtxt(dataFileName, delimiter=",", skiprows=4).transpose()

    class R(object): pass
    result = R()
    result.params = params
    for s in params.split(','):
        if "sample rate" in s:
            result.sampleRate = s

    result.inputDevice = inputDevice
    result.outputDevice = outputDevice
    result.suggestedLatency = data[0]
    result.halfDuplexOutputLatency = data[1]
    result.halfDuplexInputLatency = data[2]
    result.fullDuplexOutputLatency = data[3]
    result.fullDuplexInputLatency = data[4]
    return result;


def setFigureTitleAndAxisLabels( framesPerBufferString ):
    title("PortAudio suggested (requested) vs. resulting (reported) stream latency\n" + framesPerBufferString)
    ylabel("PaStreamInfo::{input,output}Latency (s)")
    xlabel("Pa_OpenStream suggestedLatency (s)")
    grid(True)
    legend(loc="upper left")

def setDisplayRangeSeconds( maxSeconds ):
    xlim(0, maxSeconds)
    ylim(0, maxSeconds)


# run the test with different frames per buffer values:

compositeTestFramesPerBufferValues = [0]
# powers of two
for i in range (1,11):
    compositeTestFramesPerBufferValues.append( pow(2,i) )

# multiples of 50
for i in range (1,20):
    compositeTestFramesPerBufferValues.append( i * 50 )

# 10ms buffer sizes
compositeTestFramesPerBufferValues.append( 441 )
compositeTestFramesPerBufferValues.append( 882 )

# large primes
#compositeTestFramesPerBufferValues.append( 39209 )
#compositeTestFramesPerBufferValues.append( 37537 )
#compositeTestFramesPerBufferValues.append( 26437 )

individualPlotFramesPerBufferValues = [0,64,128,256,512] #output separate plots for these

isFirst = True    

for framesPerBuffer in compositeTestFramesPerBufferValues:
    commandString = testExeName + " " + str(inputDeviceIndex) + " " + str(outputDeviceIndex) + " " + str(sampleRate) + " " + str(framesPerBuffer) + ' > ' + dataFileName
    print commandString
    os.system(commandString)

    d = loadCsvData(dataFileName)

    if isFirst:
        figure(1) # title sheet
        gcf().text(0.1, 0.0,
           "patest_suggested_vs_streaminfo_latency\n%s\n%s\n%s\n"%(d.inputDevice,d.outputDevice,d.sampleRate))
        pdfFile.savefig()
        
        
    figure(2) # composite plot, includes all compositeTestFramesPerBufferValues

    if isFirst:
        plot( d.suggestedLatency, d.suggestedLatency, label="Suggested latency" )
    
    plot( d.suggestedLatency, d.halfDuplexOutputLatency )
    plot( d.suggestedLatency, d.halfDuplexInputLatency )
    plot( d.suggestedLatency, d.fullDuplexOutputLatency )
    plot( d.suggestedLatency, d.fullDuplexInputLatency )

    if framesPerBuffer in individualPlotFramesPerBufferValues: # individual plots
        figure( 3 + individualPlotFramesPerBufferValues.index(framesPerBuffer) )

        plot( d.suggestedLatency, d.suggestedLatency, label="Suggested latency" )
        plot( d.suggestedLatency, d.halfDuplexOutputLatency, label="Half-duplex output latency" )
        plot( d.suggestedLatency, d.halfDuplexInputLatency, label="Half-duplex input latency" )
        plot( d.suggestedLatency, d.fullDuplexOutputLatency, label="Full-duplex output latency" )
        plot( d.suggestedLatency, d.fullDuplexInputLatency, label="Full-duplex input latency" )

        if framesPerBuffer == 0:
            framesPerBufferText = "paFramesPerBufferUnspecified"
        else:
            framesPerBufferText = str(framesPerBuffer)
        setFigureTitleAndAxisLabels( "user frames per buffer: "+str(framesPerBufferText) )
        setDisplayRangeSeconds(2.2)
        pdfFile.savefig()
        setDisplayRangeSeconds(0.1)
        setFigureTitleAndAxisLabels( "user frames per buffer: "+str(framesPerBufferText)+" (detail)" )
        pdfFile.savefig()

    isFirst = False

figure(2)
setFigureTitleAndAxisLabels( "composite of frames per buffer values:\n"+str(compositeTestFramesPerBufferValues) )
setDisplayRangeSeconds(2.2)
pdfFile.savefig()
setDisplayRangeSeconds(0.1)
setFigureTitleAndAxisLabels( "composite of frames per buffer values:\n"+str(compositeTestFramesPerBufferValues)+" (detail)" )
pdfFile.savefig()

pdfFile.close()

#uncomment this to display interactively, otherwise we just output a pdf
#show()
