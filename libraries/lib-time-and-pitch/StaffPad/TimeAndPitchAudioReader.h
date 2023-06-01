#pragma once

#include "BaseAudioReader.h"
#include "Signals.h"
#include "TimeAndPitch.h"

namespace staffpad::audio
{

class TimeAndPitchAudioReader : public BaseAudioReader
{
public:
   TimeAndPitchAudioReader(std::unique_ptr<BaseAudioReader> reader)
       : _base_reader(std::move(reader))
       , _sig(2, _block_size)
   {
      _time_and_pitch = std::make_unique<TimeAndPitch>(2); // 2 channels
      _time_and_pitch->setup(4096, _block_size);
      _initial_samples_to_discard = _time_and_pitch->getLatencySamples();
      _silent_samples_to_append =
         _looping ? 0 : _time_and_pitch->getLatencySamples();
   }

   virtual ~TimeAndPitchAudioReader()
   {
   }

   AudioFormat getFormat() const override
   {
      return _base_reader->getFormat();
   }

   bool canSeek() const override
   {
      return _base_reader->canSeek();
   }

   /// set the position based on input samples
   bool setPosition(int64_t newSamplePosition) override
   {
      if (newSamplePosition != _base_reader->getPosition())
      {
         _base_reader->setPosition(
            newSamplePosition); // todo: needs to move further into the past to
                                // make up latency
         _initial_samples_to_discard = _time_and_pitch->getLatencySamples();
         _silent_samples_to_append =
            _looping ? 0 : _time_and_pitch->getLatencySamples();
         _time_and_pitch->reset();
      }
      return canSeek();
   }

   int64_t getPosition() const override
   {
      return _base_reader->getPosition();
   }

   int64_t getNumOfSourceSamples() const override
   {
      return _base_reader->getNumOfSourceSamples();
   }

   int32_t read(Signal32& outputSignal) override
   {
      return _base_reader->read(outputSignal);
   }

   /// Read with time stretch and pitch shift. Do not mix this call with normal
   /// read() calls.
   int32_t readTimeAndPitch(
      Signal32& outputSignal, double timeStretch = 1.0,
      double pitchFactor = 1.0)
   {
      PPK_ASSERT(outputSignal.getNumOfChannels() == 2);

      int num_samples = outputSignal.getNumOfDataPoints();
      int p = 0;
      bool end_of_file = false;
      _time_and_pitch->setTimeStretchAndPitchFactor(timeStretch, pitchFactor);
      while (num_samples > 0)
      {
         int num_samples_to_retrieve = std::min(num_samples, _block_size);
         while (_time_and_pitch->getNumAvailableOutputSamples() <
                num_samples_to_retrieve)
         {
            int feed_now =
               std::min(_time_and_pitch->getSamplesToNextHop(), _block_size);
            _sig.ensureDimensions(2, feed_now);
            int samples_read = _read_from_base_reader_append_silence(_sig);
            _time_and_pitch->feedAudio(_sig.getReadPointer(), samples_read);
            if (samples_read < feed_now)
            {
               if (_looping)
               {
                  _base_reader->setPosition(0);
               }
               else
               {
                  end_of_file = true;
                  break;
               }
            }
         }

         if (int n = int(_initial_samples_to_discard * timeStretch + 0.5f);
             n > 0)
         {
            n = std::min({ n, _time_and_pitch->getNumAvailableOutputSamples(),
                           outputSignal.getNumOfDataPoints(), _block_size });
            // dump to the beginning of the block to be overwritten be real data
            // later
            float* dmp[2] = { outputSignal.getWriteChannelPtr(0),
                              outputSignal.getWriteChannelPtr(1) };
            _time_and_pitch->retrieveAudio(dmp, n);
            _initial_samples_to_discard -= int(n / timeStretch + 0.5f);
         }
         else
         {
            _initial_samples_to_discard =
               0; // can still be 1 or 2 for extreme factors due to rounding
            float* smp[2] = { outputSignal.getWriteChannelPtr(0) + p,
                              outputSignal.getWriteChannelPtr(1) + p };

            int num_samples_retrieved = std::min(
               num_samples_to_retrieve,
               _time_and_pitch->getNumAvailableOutputSamples());
            _time_and_pitch->retrieveAudio(smp, num_samples_retrieved);

            p += num_samples_retrieved;
            num_samples -= num_samples_retrieved;
         }
         if (end_of_file)
            break;
      }
      return p;
   }

   void setQuality(const TimeAndPitch::QualitySettings& settings)
   {
      _time_and_pitch->setQuality(settings);
   }

   void setLooping(bool flag)
   {
      _looping = flag;
      _silent_samples_to_append =
         _looping ? 0 : _time_and_pitch->getLatencySamples();
   }

private:
   /// read from the base reader, but append some silence at the end. We need
   /// this to get the final samples out
   int32_t _read_from_base_reader_append_silence(Signal32& sig)
   {
      int n = _base_reader->read(_sig);
      if (n < sig.getNumOfDataPoints())
      {
         int silent_n =
            std::min(sig.getNumOfDataPoints() - n, _silent_samples_to_append);
         sig.setToValue(0.f, n, n + silent_n, -1);
         _silent_samples_to_append -= silent_n;
         return n + silent_n;
      }
      return n;
   }

   static constexpr int _block_size = 512;
   std::unique_ptr<BaseAudioReader> _base_reader;
   Signal32 _sig;
   std::unique_ptr<TimeAndPitch> _time_and_pitch;
   int _initial_samples_to_discard = 0;
   int _silent_samples_to_append = 0;
   bool _looping = false;
};

typedef std::shared_ptr<TimeAndPitchAudioReader> SharedTimeAndPitchAudioReader;

} // namespace staffpad::audio
