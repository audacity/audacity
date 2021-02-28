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

   virtual void Clear() = 0;
   virtual void Reset(double sampleRate, bool resetClipping) = 0;
   virtual void UpdateDisplay(unsigned numChannels,
                      int numFrames, float *sampleData) = 0;
   virtual bool IsMeterDisabled() const = 0;
   virtual float GetMaxPeak() const = 0;
   virtual bool IsClipping() const = 0;
   virtual int GetDBRange() const = 0;
};

#endif
