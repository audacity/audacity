VfdnReverbProcessor
===================


Description
-----------

TimeAndPitch algorithm with arbitrary variable pitch and time-scaling factors.

Built and tested for the x86_64 and arm64 architecture, in Windows (Visual Studio 2022) and macOS (XCode 14.0.1).

Warning on parameter range
--------------------------

> The TimeAndPitch effect was designed to alter pitch and time by arbitrary factors.
Both pitch and time factor parameters were thoroughly tested in the range [0.5, 2.0].
More extreme settings are supported and, since this is a development distribution, no limits 
were set internally, so that they can be tailored to the final application. Therefore, it is
strongly recommended that both parameters are limited to reasonable and well-tested ranges
**to prevent unforeseen loud or noisy output with indiscriminate pitch or time factors**.


Quick start usage
-----------------

The following reference snippets suggest how to load and use the effect, either from the output or input point of view (i.e. time reference).

```C++
// Example 1: pull output samples (e.g. using a custom input stream interface)

// ...

TimeAndPitch tp;
int BUFFERSIZE = 1024;
tp.setup(2, BUFFERSIZE);
const int latency = tp.getLatencySamples(); // Useful to trim the output's initial silence if needed

// ...

// `Reader` is a fictional input stream class, capable of reading n input samples on demand and seeking a position.
int32_t readTimeAndPitch(float** out, int num_samples, Reader& in, double timeStretch = 1.0, double pitchFactor = 1.0)
{
  int p = 0;
  bool end_of_file = false;
  tp.setTimeStretchAndPitchFactor(timeStretch, pitchFactor);
  while (num_samples > 0)
  {
    int num_samples_to_retrieve = std::min(num_samples, BUFFERSIZE);
    while (tp.getNumAvailableOutputSamples() < num_samples_to_retrieve)
    {
      int feed_now = std::min(tp.getSamplesToNextHop(), BUFFERSIZE);

      // Example of `read(out, n)` API to provide available samples from the current position.
      // temp_buffer is a pre-allocated audio buffer of BUFFERSIZE length.
      int valid_samples_read = in.read(temp_buffer, feed_now);
      tp.feedAudio(temp_buffer, valid_samples_read);

      if (valid_samples_read < feed_now)
      {
        // End of input reached
        if (LOOPING) // fictional setting enabling seamless looped time stretching
        {
          // Example of `seek(pos)` API to rewind the reader to the loop start,
          // in this example sample 0.
          in.seek(0);
        }
        else
        {
          // No more data to read. One could feed (feed_now - valid_samples_read)
          // silent samples here, to force tp to produce more output.
          end_of_file = true;
          break;
        }
      }
    }

    float* smp[2] = {out[0] + p, out[1] + p};
    int num_samples_retrieved = std::min(num_samples_to_retrieve, tp.getNumAvailableOutputSamples());
    tp.retrieveAudio(smp, num_samples_retrieved);

    p += num_samples_retrieved;
    num_samples -= num_samples_retrieved;
    if (end_of_file)
      break;
  }
  return p;
}
```

```C++
// Example 2: push input samples, e.g. for a simpler offline test

// ...

TimeAndPitch tp;
int BUFFERSIZE = 1024;
tp.setup(2, BUFFERSIZE);
const int latency = tp.getLatencySamples(); // Useful to trim the output's initial silence if needed

// ...

int s = 0;
int s_out = 0;

// float** ptr point to stereo input buffers, float** ptr_out points to large engough output buffers
while ((s < (NUM_SAMPLES_IN - BUFFERSIZE)) && (s_out < NUM_SAMPLES_OUT))
{
  int n = BUFFERSIZE; // n can be <= BUFFERSIZE

  tp.setTimeStretchAndPitchFactor(TIME_STRETCH, PITCH_RATIO); // can be changed at block processing level

  tp.feedAudio(ptr, n);

  while (s_out + BUFFERSIZE < NUM_SAMPLES_OUT)
  {
    int out_samples = std::min(tp.getNumAvailableOutputSamples(), BUFFERSIZE);
    if (out_samples == 0)
    {
      break;
    }
    tp.retrieveAudio(ptr_out, out_samples);
    ptr_out[0] += out_samples;
    ptr_out[1] += out_samples;
    s_out += out_samples;
  }

  ptr[0] += n;
  ptr[1] += n;
  s += n;
}
```

\[14/07/2023\] Latency in output samples
---------------------------------------------

A more practical and accurate latency calculation is now available:
```C++
/**
  Latency in output samples, calculated on the given timeStretch factor
*/
int getLatencySamplesForStretchRatio(float timeStretch) const;
```
It is recommended to use this new method in place of `getLatencySamples()`, as long as it's possible to discard the initial silent samples once the initial stretch factor is known. This will minimise undesired side-effects of the previous latency compensation like, in some corner cases, chopping a transient at the very beginning of playback.

Example:
```C++
// Replace `const int latency = tp.getLatencySamples();` in "Example 2" above with:
const int latency = tp.getLatencySamples(TIME_STRETCH); // Discard `latency` output samples
// ...
```
