# A functionality for developers.
# Disclaimer: At the time of writing only used on Windows; will not work out of the box on other OS.
#
# To use this:
# 1. Add
#       [TimeAndPitch]
#       TuningOn=1
#    to audacity.cfg (see https://manual.audacityteam.org/man/preferences.html#stored to find out where it is)
# 2. From the audacity.cfg directory, create `./TimeAndPitchTuning/overrideLogTime.txt`
# 3. Enter a time in seconds, e.g. (0.5)
# 4. Run your Audacity build and play a track with pitch shifting and formant preservation enabled.
#
# You will hear a gentle noise burst at the specified time, which indicates precisely the place where the snapshot was taken.
# Now you can run this script from lib-time-and-pitch's root directory.
#
# Note: the value in `overrideLogTime.txt` gets re-read on each playback, but to enable/disable the tuning,
# you have to change the config file and then restart Audacity.

import matplotlib.pyplot as plt
import os, sys

# Add the path to the local AppData\Roaming directory
appdata_path = os.path.join(os.getenv('APPDATA'), 'audacity', 'TimeAndPitchTuning')
sys.path.append(appdata_path)

import FormantShifterLog as FSL
import numpy as np

sampleRate = FSL.sampleRate
fftSize = FSL.fftSize
numBins = fftSize // 2 + 1
# Calculate `freqs` array using linspace
f = np.linspace(0, sampleRate / 2, numBins)
q = np.linspace(0, 1000*fftSize/sampleRate, fftSize)

yscale = 'log'
# yscale = 'linear'
markersize = 0
xlim = sampleRate/2

plt.figure()
plt.yscale(yscale)
plt.plot(f, FSL.weights)
plt.xlim(0, sampleRate/2)
plt.title('weights')
plt.grid()

plt.figure()
plt.title('fftSize:' + str(fftSize))

plt.subplot(2, 1, 1)
plt.yscale(yscale)
plt.plot(f, FSL.magnitude, marker='x', markersize=markersize)
plt.plot(f, FSL.envelope, linestyle='dotted', marker='o', markersize=markersize, fillstyle='none')
plt.legend(['mag', 'env'])
plt.xlim(0, xlim)
plt.ylabel('input')
plt.grid()

plt.subplot(2, 1, 2)
plt.yscale(yscale)
plt.plot(f, FSL.weightedMagnitude, marker='x', markersize=markersize)
plt.plot(f, FSL.envelopeResampled, linestyle='dotted', marker='o', markersize=markersize, fillstyle='none')
plt.legend(['mag', 'env'])
plt.xlim(0, xlim)
plt.ylabel('output')
plt.grid()

plt.figure()
plt.plot(q, FSL.cepstrum)
plt.plot(q, FSL.cepstrumLiftered, linestyle='dotted')
plt.title('cepstrum')
plt.grid()
plt.xlim(0, 1000*fftSize/sampleRate/2)
plt.xlabel('quefrency (ms)')

plt.show()
