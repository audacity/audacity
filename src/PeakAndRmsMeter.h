/**********************************************************************

  Audacity: A Digital Audio Editor

  PeakAndRmsMeter.h

  Dominic Mazzoni

  Paul Licameli split from MeterPanel.h

**********************************************************************/
#ifndef __AUDACITY_PEAK_AND_RMS_METER__
#define __AUDACITY_PEAK_AND_RMS_METER__

#include "LockFreeQueue.h"
#include "Meter.h"

// Increase this when we add support for multichannel meters
// (most of the code is already there)
const int kMaxMeterBars = 2;

class wxString;

class MeterUpdateMsg
{
   public:
   int numFrames;
   float peak[kMaxMeterBars];
   float rms[kMaxMeterBars];
   bool clipping[kMaxMeterBars];
   int headPeakCount[kMaxMeterBars];
   int tailPeakCount[kMaxMeterBars];

   /* neither constructor nor destructor do anything */
   MeterUpdateMsg() { }
   ~MeterUpdateMsg() { }
   /* for debugging purposes, printing the values out is really handy */
   /** \brief Print out all the values in the meter update message */
   wxString toString();
   /** \brief Only print meter updates if clipping may be happening */
   wxString toStringIfClipped();
};

// The MeterPanel passes itself messages via this queue so that it can
// communicate between the audio thread and the GUI thread.
// This class uses lock-free synchronization with atomics.
//
using MeterUpdateQueue = LockFreeQueue<MeterUpdateMsg>;

class AUDACITY_DLL_API PeakAndRmsMeter
   : public Meter
   , public NonInterferingBase
{
public:
   struct Stats {
      void Reset(bool resetClipping)
      {
         peak = 0.0;
         rms = 0.0;
         peakHold = 0.0;
         peakHoldTime = 0.0;
         if (resetClipping) {
            clipping = false;
            peakPeakHold = 0.0;
         }
         tailPeakCount = 0;
      }
      float  peak{ 0 };
      float  rms{ 0 };
      float  peakHold{ 0 };
      double peakHoldTime{ 0 };
      bool   clipping{ false };
      int    tailPeakCount{ 0 };
      float  peakPeakHold{ 0 };
   };

   PeakAndRmsMeter(int dbRange,
      float decayRate = 60.0f // dB/sec
   );
   ~PeakAndRmsMeter() override;

   //! Call from the main thread to consume from the inter-thread queue
   /*!
    Updates the member mStats, to detect clipping, sufficiently longheld peak,
    and a trailing exponential moving average of the RMS signal, which may be
    used in drawing
    */
   void Poll();

   //! Receive one message corresponding to given time
   /*!
    Default implementation does nothing
    @param time clock time relative to last Reset()
    @param msg its `peak` and `rms` adjusted to dB when `mdB`
    */
   virtual void Receive(double time, const MeterUpdateMsg &msg);

   void Clear() override;
   void Reset(double sampleRate, bool resetClipping) override;

   //! Update the meters with a block of audio data
   /*!
    Process the supplied block of audio data, extracting the peak and RMS
    levels to send to the meter. Also record runs of clipped samples to detect
    clipping that lies on block boundaries.
    This method is thread-safe!  Feel free to call from a different thread
    (like from an audio I/O callback).

    @param numChannels The number of channels of audio being played back or
    recorded.
    @param numFrames The number of frames (samples) in this data block. It is
    assumed that there are the same number of frames in each channel.
    @param sampleData The audio data itself.
    */
   void Update(unsigned numChannels,
      unsigned long numFrames, const float *sampleData, bool interleaved)
   override;

   //! Find out if the level meter is disabled or not.
   /*!
    This method is thread-safe!  Feel free to call from a
    different thread (like from an audio I/O callback).
    */
   bool IsDisabled() const override;

   bool IsClipping() const;
   int GetDBRange() const;

protected:
   MeterUpdateQueue mQueue{ 1024 };
   float     mDecayRate{}; // dB/sec
   unsigned  mNumBars{ 0 };
   Stats  mStats[kMaxMeterBars]{};
   int mNumPeakSamplesToClip{ 3 };
   int       mDBRange{ 60 };
   bool      mDB{ true };
   bool mMeterDisabled{};

private:
   double mRate{};
   double mT{};
   int mPeakHoldDuration{ 3 };
   bool mDecay{ true };
};
#endif // __AUDACITY_METER_PANEL__
