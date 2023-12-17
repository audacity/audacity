/*!********************************************************************

Audacity: A Digital Audio Editor

@file Meter.h

Paul Licameli split from MeterPanelBase.h

**********************************************************************/

#ifndef __AUDACITY_METER__
#define __AUDACITY_METER__

//! AudioIO uses this to send sample buffers for real-time display updates
class AUDIO_DEVICES_API Meter /* not final */
{
public:
   virtual ~Meter();

   //! This member function will be called from a low-latency thread.
   virtual void Update(unsigned numChannels,
      unsigned long numFrames, const float *sampleData) = 0;

   //! This member function will be called from a low-latency thread.
   virtual bool IsDisabled() const = 0;

   /*!
    @name Main thread's callbacks
    @{
    */
   virtual void Clear() = 0;
   virtual void Reset(double sampleRate, bool resetClipping) = 0;
   /*!
    @}
    */
};

#endif
