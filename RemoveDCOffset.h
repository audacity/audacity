/**********************************************************************

  Audacity: A Digital Audio Editor

  RemoveDCOffset.h

  Patrick Kallenbach

  Adapted from Normalize
  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REMOVEDCOFFSET__
#define __AUDACITY_EFFECT_REMOVEDCOFFSET__

#include "StatefulEffect.h"
#include "Biquad.h"
#include "ShuttleAutomation.h"
#include <wx/weakref.h>
#include <functional>

class wxCheckBox;
class wxStaticText;
class wxTextCtrl;
class ShuttleGui;
class WaveChannel;

class EffectRemoveDCOffset final : public StatefulEffect
{
public:
   static inline EffectRemoveDCOffset *
   FetchParameters(EffectRemoveDCOffset &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectRemoveDCOffset();
   virtual ~EffectRemoveDCOffset();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;
   bool IsInteractive() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;

private:
   // EffectRemoveDCOffset implementation

   bool ProcessOne(WaveChannel &track,
      const TranslatableString &msg, double& progress, float offset);
   using ProgressReport = std::function<bool(double fraction)>;
   static bool AnalyseTrack(const WaveChannel &track,
      const ProgressReport &report,
      double curT0, double curT1,
      float &offset, float &extent);
   static bool AnalyseTrackData(const WaveChannel &track,
      const ProgressReport &report, double curT0, double curT1,
      float &offset);
   static double AnalyseDataDC(float *buffer, size_t len, double sum);
   void ProcessData(float *buffer, size_t len, float offset);

private:
   wxWeakRef<wxWindow> mUIParent{};

   double mCurT0;
   double mCurT1;
   wxStaticText *mWarning;
   bool mCreating;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()
};

#endif
