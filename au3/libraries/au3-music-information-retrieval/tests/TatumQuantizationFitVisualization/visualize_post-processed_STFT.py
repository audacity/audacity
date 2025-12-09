from stft_log import *

# Now we have a variable called `stft` which is a vector of vectors of floats, each vector of floats is a frame of the STFT.
# We can visualize this using matplotlib's `imshow` function.

# Plot the STFT
import matplotlib.pyplot as plt

# Use imshow, scaling the axes to show Hz on the y axis and seconds on the x axis.
# Transform `stft` to a numpy matrix.
import numpy as np
stft = np.transpose(np.array(stft))

plt.imshow(stft, aspect='auto', cmap='viridis', interpolation='nearest', origin='lower')
plt.colorbar()
plt.show()
