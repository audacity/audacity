Imagine there were an audio cable plugged from your output back to your input. You press play, and at some stage, the project audio will reach back the input and become recorded. Latency compensation simply consists of discarding all incoming audio until the project audio arrives. Then users could play their project, record themselves on the fly and the incoming audio would be in sync without further manual adjustment required.

Audacity already has a latency-compensation setting for the user to adjust manually. Automatically adjusting this setting optimally would be trivial as soon as we knew the round-trip latency of the audio device.

PortAudio reports these latencies, but experiments showed that they weren't reliable. Here I make an experiment that I believe to provide exact estimates of round-trip latency.

## Description of experiment

- Connect the selected output device to the selected input device with an audio cable.
- Before recording starts, create a wav file.
- In the audio IO callback (on the audio driver thread)
    - overwrite the output buffer with one second of silence, then with a sine wave
    - write the samples of the input buffer to the wav file.
- When recording stops, close the wav file.
- Open the file in Audacity and observe the latency.

## Results

| OS  | Host        | requested buffer size (\*) | PA-reported output buffer size (\*\*) | PA-reported input buffer size | Measured round-trip | error (reported - actual) |
| --- | ----------- | -------------------------- | ------------------------------------- | ----------------------------- | ------------------- | ------------------------- |
| Win | WASAPI      | 0.25                       | 0.27                                  | 0.26                          | 0.075               | 0.455                     |
| Win | WASAPI      | 0.1                        | 0.12                                  | 0.11                          | 0.075               | 0.155                     |
| Win | WASAPI      | 0.02                       | 0.04                                  | 0.03                          | 0.075               | -0.005                    |
| Win | DirectSound | 0.25                       | 0.25                                  | 0.0625                        | 0.42                | -0.1075                   |
| Win | DirectSound | 0.1                        | 0.1                                   | 0.025                         | 0.115               | 0.01                      |
| Win | DirectSound | 0.02                       | didn't work                           | didn't work                   | didn't work         | didn't work               |
| Win | ASIO        | 0.5                        | 0.050498866213151930                  | 0.047936507936507937          | 0.1                 | -0.0015646258503401456    |
| Win | ASIO        | 0.1                        | 0.050498866213151930                  | 0.047936507936507937          | 0.1                 | -0.0015646258503401456    |
| Win | ASIO        | 0.02                       | 0.026507936507936508                  | 0.024716553287981859          | 0.054               | -0.0027755102040816285    |

(\*) applies to both input and output, e.g. 0.25 means 0.25 for the input + 0.25 for the output, in total 0.5.
(\*\*) for one same requested size, may differ wildly depending on whether only playback is active or playback and recording. Here we write the values obtained when both are active.
