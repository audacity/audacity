/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_STEREO_TO_MONO__
#define __AUDACITY_EFFECT_STEREO_TO_MONO__

#include "Effect.h"

class EffectStereoToMono: public Effect {

public:

   EffectStereoToMono();

   virtual wxString GetEffectName() {
      return wxString(_("Stereo to Mono"));
   }

   virtual std::set<wxString> GetEffectCategories() {
     std::set<wxString> result;
     result.insert(wxT("http://lv2plug.in/ns/lv2core#MixerPlugin"));
     return result;
   }

   // Used internally, users will not see this.  Do not translate.
   virtual wxString GetEffectIdentifier() {
      return wxT("StereoToMono");
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Stereo to Mono"));
   }
   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
 protected:
    virtual bool Process();

private:
   bool ProcessOne(int);

   sampleCount mStart;
   sampleCount mEnd;
   WaveTrack *mLeftTrack;
   WaveTrack *mRightTrack;
   WaveTrack *mOutTrack;

};

#endif

