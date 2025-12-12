### Setup 1

(Almost) worst-case scenario settings:

1 clip:

-   mono
-   1mn
-   fits viewport horizontally
-   default height (shouldn't matter much)

Spectrogram settings:

-   Algorithm: Frequencies
-   Window size: 32768
-   Zero-padding factor: 16

| OMP | PFFFT | Release | Time |
| --- | ----- | ------- | ---- |
| Y   | N     | Y       | 3s   |
| Y   | Y     | Y       | 4s   |
| N   | Y     | Y       | 6s   |
| N   | N     | Y       | 15s  |

### Setup 2

Trying to give as much work as possible yet with only one track (to change settings more conveniently using the Preview button) and staying within the FFT size boundaries where PFFFT yields optimal performance (according to pffft.h):

1 clip:

-   stereo
-   1mn
-   fits viewport horizontally
-   default height (shouldn't matter much)

Spectrogram settings:

-   Algorithm: Reassignment
-   Window size: 8192
-   Zero-padding factor: 1

| OMP | PFFFT | Release | Time |
| --- | ----- | ------- | ---- |
| Y   | N     | Y       | 1/6s |
| Y   | Y     | Y       | 1/6s |
| N   | Y     | Y       | 1/2s |
| N   | N     | Y       | 1/2s |

### Setup 3

7 stereo tracks each with a 1mn clip filling the viewport.
Settings:

-   reassignment
-   8192

Changing zero-padding factor from 2 to 1 and observe the time it takes to update.

| OS    | QtConcurrent | PFFFT | Time |
| ----- | ------------ | ----- | ---- |
| Linux | 1            | 0     | 0.5  |
| Linux | 1            | 1     | 0.5  |
| Linux | 0            | 0     | 4    |
| Mac   | 1            | 1     | 1/3  |
| Mac   | 1            | 0     | 1/3+ |
| Mac   | 0            | 0     | 1.5  |
| Win   | 1            | 0     | 0.5+ |
| Win   | 1            | 1     | 0.5  |
| Win   | 0            | 0     | 3    |

Current vs Concurrent:
* Linux 8 times faster
* Mac 6 times faster
* Windows 5 times faster
