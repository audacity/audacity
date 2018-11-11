/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#ifndef __AUDACITY_EFFECTMANAGER__
#define __AUDACITY_EFFECTMANAGER__

#include "../Experimental.h"

#include <vector>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/listbox.h>

#include "audacity/EffectInterface.h"
#include "Effect.h"

#include <unordered_map>

class AudacityCommand;
class CommandContext;
class CommandMessageTarget;

using EffectArray = std::vector <Effect*> ;
using EffectMap = std::unordered_map<wxString, Effect *>;
using AudacityCommandMap = std::unordered_map<wxString, AudacityCommand *>;
using EffectOwnerMap = std::unordered_map< wxString, std::shared_ptr<Effect> >;

#if defined(EXPERIMENTAL_EFFECTS_RACK)
class EffectRack;
#endif
class AudacityCommand;


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

   wxString GetEffectFamilyName(const PluginID & ID);
   wxString GetVendorName(const PluginID & ID);

   /** Run a command given the plugin ID */
   // Returns true on success. 
   bool DoAudacityCommand(const PluginID & ID,
                         const CommandContext &,
                         wxWindow *parent,
                         bool shouldPrompt  = true );

   // Renamed from 'Effect' to 'Command' prior to moving out of this class.
   ComponentInterfaceSymbol GetCommandSymbol(const PluginID & ID);
   wxString GetCommandName(const PluginID & ID); // translated
   CommandID GetCommandIdentifier(const PluginID & ID);
   wxString GetCommandDescription(const PluginID & ID);
   wxString GetCommandUrl(const PluginID & ID);
   wxString GetCommandTip(const PluginID & ID);
   // flags control which commands are included.
   void GetCommandDefinition(const PluginID & ID, const CommandContext & context, int flags);
   bool IsHidden(const PluginID & ID);

   /** Support for batch commands */
   bool SupportsAutomation(const PluginID & ID);
   wxString GetEffectParameters(const PluginID & ID);
   bool SetEffectParameters(const PluginID & ID, const wxString & params);
   bool PromptUser(const PluginID & ID, wxWindow *parent);
   bool HasPresets(const PluginID & ID);
   wxString GetPreset(const PluginID & ID, const wxString & params, wxWindow * parent);
   wxString GetDefaultPreset(const PluginID & ID);

private:
   void SetBatchProcessing(const PluginID & ID, bool start);
   struct UnsetBatchProcessing {
      PluginID mID;
      void operator () (EffectManager *p) const
         { if(p) p->SetBatchProcessing(mID, false); }
   };
   using BatchProcessingScope =
      std::unique_ptr< EffectManager, UnsetBatchProcessing >;
public:
   // RAII for the function above
   BatchProcessingScope SetBatchProcessing(const PluginID &ID)
   {
      SetBatchProcessing(ID, true); return BatchProcessingScope{ this, {ID} };
   }

   /** Allow effects to disable saving the state at run time */
   void SetSkipStateFlag(bool flag);
   bool GetSkipStateFlag();

   // Realtime effect processing
   bool RealtimeIsActive();
   bool RealtimeIsSuspended();
   void RealtimeAddEffect(Effect *effect);
   void RealtimeRemoveEffect(Effect *effect);
   void RealtimeSetEffects(const EffectArray & mActive);
   void RealtimeInitialize(double rate);
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

   const PluginID & GetEffectByIdentifier(const CommandID & strTarget);

private:
   /** Return an effect by its ID. */
   Effect *GetEffect(const PluginID & ID);
   AudacityCommand *GetAudacityCommand(const PluginID & ID);

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   EffectRack *GetRack();
#endif

private:
   EffectMap mEffects;
   AudacityCommandMap mCommands;
   EffectOwnerMap mHostEffects;

   int mNumEffects;

   wxCriticalSection mRealtimeLock;
   EffectArray mRealtimeEffects;
   int mRealtimeLatency;
   bool mRealtimeSuspended;
   bool mRealtimeActive;
   std::vector<unsigned> mRealtimeChans;
   std::vector<double> mRealtimeRates;

   // Set true if we want to skip pushing state 
   // after processing at effect run time.
   bool mSkipStateFlag;

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   EffectRack *mRack;

   friend class EffectRack;
#endif

};


#endif
