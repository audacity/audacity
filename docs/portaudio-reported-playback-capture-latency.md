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

| OS  | Host        | requested buffer size (ms) (\*) | PA-reported output buffer size (ms) (\*\*) | PA-reported input buffer size (ms) | Measured round-trip (ms) | error (reported - actual) (ms) |
| --- | ----------- | ------------------------------- | ------------------------------------------ | ---------------------------------- | ------------------------ | ------------------------------ |
| Win | WASAPI      | 250                             | 270                                        | 260                                | 75                       | 455                            |
| Win | WASAPI      | 100                             | 120                                        | 110                                | 75                       | 155                            |
| Win | WASAPI      | 20                              | 40                                         | 30                                 | 75                       | -5                             |
| Win | DirectSound | 250                             | 250                                        | 62.5                               | 420                      | -107.5                         |
| Win | DirectSound | 100                             | 100                                        | 25                                 | 115                      | 10                             |
| Win | DirectSound | 20                              | didn't work                                | didn't work                        | didn't work              | didn't work                    |
| Win | ASIO        | 500                             | 50.5                                       | 47.9                               | 100                      | -1.6                           |
| Win | ASIO        | 100                             | 50.5                                       | 47.9                               | 100                      | -1.6                           |
| Win | ASIO        | 20                              | 26.5                                       | 24.7                               | 54                       | -2.8                           |

(\*) applies to both input and output, e.g. 250 means 250 for the input + 250 for the output, in total 500.
(\*\*) for one same requested size, may differ wildly depending on whether only playback is active or playback and recording. Here we write the values obtained when both are active.
