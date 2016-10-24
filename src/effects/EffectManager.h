/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class EffectManager
\brief EffectManager is the class that handles effects and effect categories.

It maintains a graph of effect categories and subcategories,
registers and unregisters effects and can return filtered lists of
effects.

*//*******************************************************************/

#ifndef __AUDACITY_EFFECTMANAGER__
#define __AUDACITY_EFFECTMANAGER__

#include "../Experimental.h"

#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/listbox.h>
#include <wx/string.h>

#include "audacity/EffectInterface.h"
#include "../PluginManager.h"
#include "Effect.h"

WX_DEFINE_USER_EXPORTED_ARRAY(Effect *, EffectArray, class AUDACITY_DLL_API);
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(Effect *, EffectMap, class AUDACITY_DLL_API);
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(std::shared_ptr<Effect>, EffectOwnerMap, class AUDACITY_DLL_API);

#if defined(EXPERIMENTAL_EFFECTS_RACK)
class EffectRack;
#endif

class AUDACITY_DLL_API EffectManager
{
public:

   /** Get the singleton instance of the EffectManager. Probably not safe
       for multi-thread use. */
   static EffectManager & Get();

//
// public methods
//
// Used by the outside program to register the list of effects and retrieve
// them by index number, usually when the user selects one from a menu.
//
public:
   EffectManager();
   virtual ~EffectManager();

   /** (Un)Register an effect so it can be executed. */
   // Here solely for the purpose of Nyquist Workbench until
   // a better solution is devised.
   const PluginID & RegisterEffect(Effect *f);
   void UnregisterEffect(const PluginID & ID);

   /** Run an effect given the plugin ID */
   // Returns true on success.  Will only operate on tracks that
   // have the "selected" flag set to true, which is consistent with
   // Audacity's standard UI.
   bool DoEffect(const PluginID & ID,
                 wxWindow *parent,
                 double projectRate,
                 TrackList *list,
                 TrackFactory *factory,
                 SelectedRegion *selectedRegion,
                 bool shouldPrompt = true);

   wxString GetEffectName(const PluginID & ID);
   wxString GetEffectIdentifier(const PluginID & ID);
   wxString GetEffectDescription(const PluginID & ID);
   bool IsHidden(const PluginID & ID);

   /** Support for batch commands */
   bool SupportsAutomation(const PluginID & ID);
   wxString GetEffectParameters(const PluginID & ID);
   bool SetEffectParameters(const PluginID & ID, const wxString & params);
   bool PromptUser(const PluginID & ID, wxWindow *parent);
   bool HasPresets(const PluginID & ID);
   wxString GetPreset(const PluginID & ID, const wxString & params, wxWindow * parent);
   wxString GetDefaultPreset(const PluginID & ID);
   void SetBatchProcessing(const PluginID & ID, bool start);

   /** Allow effects to disable saving the state at run time */
   void SetSkipStateFlag(bool flag);
   bool GetSkipStateFlag();

   // Realtime effect processing
   bool RealtimeIsActive();
   bool RealtimeIsSuspended();
   void RealtimeAddEffect(Effect *effect);
   void RealtimeRemoveEffect(Effect *effect);
   void RealtimeSetEffects(const EffectArray & mActive);
   void RealtimeInitialize();
   void RealtimeAddProcessor(int group, unsigned chans, float rate);
   void RealtimeFinalize();
   void RealtimeSuspend();
   void RealtimeResume();
   void RealtimeProcessStart();
   size_t RealtimeProcess(int group, unsigned chans, float **buffers, size_t numSamples);
   void RealtimeProcessEnd();
   int GetRealtimeLatency();

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   void ShowRack();
#endif

   const PluginID & GetEffectByIdentifier(const wxString & strTarget);

private:
   /** Return an effect by its ID. */
   Effect *GetEffect(const PluginID & ID);

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   EffectRack *GetRack();
#endif

private:
   EffectMap mEffects;
   EffectOwnerMap mHostEffects;

   int mNumEffects;

   wxCriticalSection mRealtimeLock;
   EffectArray mRealtimeEffects;
   int mRealtimeLatency;
   bool mRealtimeSuspended;
   bool mRealtimeActive;
   std::vector<unsigned> mRealtimeChans;
   wxArrayDouble mRealtimeRates;

   // Set true if we want to skip pushing state 
   // after processing at effect run time.
   bool mSkipStateFlag;

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   EffectRack *mRack;

   friend class EffectRack;
#endif

};


#endif
