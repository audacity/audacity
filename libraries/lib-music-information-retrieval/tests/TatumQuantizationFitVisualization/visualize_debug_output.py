from debug_output import *

# Make a plot of the ODF, with peaks marked. Add another layer where the tatums are shown, for example as vertical lines.
import matplotlib.pyplot as plt
import math

plt.figure(1)
# Plot the ODF, using odfSr to convert from samples to seconds
plt.plot([i/odfSr for i in range(len(odf))], odf)
# Plot the peaks
plt.plot([i/odfSr for i in odf_peak_indices], [odf[i] for i in odf_peak_indices], 'ro')
tatumsPerSecond = tatumRate / 60.0

# Plot the tatums as vertical lines, offset by `lag` samples
numTatums = int(math.ceil(len(odf) / odfSr * tatumsPerSecond))
for i in range(numTatums):
    plt.axvline(x=i/tatumsPerSecond + lag / odfSr, color='g', linestyle='--')

plt.xlim(0, len(odf) / odfSr)
plt.title(wavFile)

# Plot `rawOdf` and `movingAverage` on the same plot.
plt.figure(2)
plt.plot([i/odfSr for i in range(len(odf))], rawOdf)
plt.plot([i/odfSr for i in range(len(odf))], movingAverage)
plt.xlim(0, len(odf) / odfSr)
plt.grid(True)
plt.title(wavFile)

# Plot `odfAutoCorr`, whose length is half that of `odf` + 1. Add a layer where the peaks at `odfAutoCorrPeakIndices` are marked.
plt.figure(3)
plt.plot([i/odfSr for i in range(len(odfAutoCorr))], odfAutoCorr)
plt.plot([i/odfSr for i in odfAutoCorrPeakIndices], [odfAutoCorr[i] for i in odfAutoCorrPeakIndices], 'ro')
plt.xlim(0, len(odfAutoCorr) / odfSr)
plt.grid(True)
plt.title(wavFile)

plt.show()
