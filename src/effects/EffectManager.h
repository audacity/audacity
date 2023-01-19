/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#ifndef __AUDACITY_EFFECTMANAGER__
#define __AUDACITY_EFFECTMANAGER__

#include <memory>
#include <vector>

#include <unordered_map>
#include "EffectInterface.h"
#include "EffectUIServices.h" // for EffectDialogFactory
#include "Identifier.h"

class AudacityCommand;
class AudacityProject;
class CommandContext;
class CommandMessageTarget;
class ComponentInterfaceSymbol;
class TrackList;
class SelectedRegion;
class wxString;
typedef wxString PluginID;

#include "EffectInterface.h"

struct EffectAndDefaultSettings{
   EffectPlugin *effect{};
   EffectSettings settings{};
};

using EffectMap = std::unordered_map<wxString, EffectAndDefaultSettings>;
using AudacityCommandMap = std::unordered_map<wxString, AudacityCommand *>;
using EffectOwnerMap = std::unordered_map< wxString, std::shared_ptr<EffectPlugin> >;

class AudacityCommand;


class AUDACITY_DLL_API EffectManager
{
public:

   enum : unsigned {
      // No flags specified
      kNone = 0x00,
      // Flag used to disable prompting for configuration parameteres.
      kConfigured = 0x01,
      // Flag used to disable saving the state after processing.
      kSkipState = 0x02,
      // Flag used to disable "Repeat Last Effect"
      kDontRepeatLast = 0x04,
      // Flag used to disable "Select All during Repeat Generator Effect"
      kRepeatGen = 0x08,
      // Flag used for repeating Nyquist Prompt
      kRepeatNyquistPrompt = 0x10,
   };

   /*! Find the singleton EffectInstanceFactory for ID. */
   static
   const EffectInstanceFactory *GetInstanceFactory(const PluginID &ID);

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

   //! Here solely for the purpose of Nyquist Workbench until a better solution is devised.
   /** Register an effect so it can be executed.
     uEffect is expected to be a self-hosting Nyquist effect */
   const PluginID & RegisterEffect(std::unique_ptr<EffectPlugin> uEffect);
   //! Used only by Nyquist Workbench module
   void UnregisterEffect(const PluginID & ID);

   TranslatableString GetEffectFamilyName(const PluginID & ID);
   TranslatableString GetVendorName(const PluginID & ID);

   /** Run a command given the plugin ID */
   // Returns true on success. 
   bool DoAudacityCommand(const PluginID & ID,
                         const CommandContext &,
                         wxWindow *parent,
                         bool shouldPrompt  = true );

   // Renamed from 'Effect' to 'Command' prior to moving out of this class.
   ComponentInterfaceSymbol GetCommandSymbol(const PluginID & ID);
   TranslatableString GetCommandName(const PluginID & ID);
   CommandID GetCommandIdentifier(const PluginID & ID);
   TranslatableString GetCommandDescription(const PluginID & ID);
   ManualPageID GetCommandUrl(const PluginID & ID);
   TranslatableString GetCommandTip(const PluginID & ID);
   // flags control which commands are included.
   void GetCommandDefinition(const PluginID & ID, const CommandContext & context, int flags);
   bool IsHidden(const PluginID & ID);

   /** Support for batch commands */
   bool SupportsAutomation(const PluginID & ID);
   wxString GetEffectParameters(const PluginID & ID);
   bool SetEffectParameters(const PluginID & ID, const wxString & params);
   bool PromptUser( const PluginID & ID, const EffectDialogFactory &factory,
      wxWindow &parent );
   bool HasPresets(const PluginID & ID);
   wxString GetPreset(const PluginID & ID, const wxString & params, wxWindow * parent);
   wxString GetDefaultPreset(const PluginID & ID);

private:
   void BatchProcessingOn(const PluginID & ID);
   void BatchProcessingOff(const PluginID & ID);
   //! A custom deleter for std::unique_ptr
   struct UnsetBatchProcessing {
      PluginID mID;
      void operator () (EffectManager *p) const
         { if(p) p->BatchProcessingOff(mID); }
   };
   using BatchProcessingScope =
      std::unique_ptr< EffectManager, UnsetBatchProcessing >;
public:
   //! Begin a scope that ends when the returned object is destroyed
   /*!
    Within this scope, "batch" (i.e. macro) processing happens, and
    Effects that are not yet stateless may change their state temporarily,
    but it is restored afterward
    */
   BatchProcessingScope SetBatchProcessing(const PluginID &ID)
   {
      BatchProcessingOn(ID); return BatchProcessingScope{ this, {ID} };
   }

   const PluginID & GetEffectByIdentifier(const CommandID & strTarget);

   /*! Return an effect by its ID. */
   EffectPlugin *GetEffect(const PluginID & ID);

   /*! Get default settings by effect ID.  May return nullptr */
   EffectSettings *GetDefaultSettings(const PluginID & ID);

   /*! Get effect and default settings by effect ID. */
   /*!
    @post `result: !result.first || result.second`
    (if first member is not null, then the second is not null)
    */
   std::pair<EffectPlugin *, EffectSettings *>
   GetEffectAndDefaultSettings(const PluginID & ID);

private:
   EffectAndDefaultSettings &DoGetEffect(const PluginID & ID);

   AudacityCommand *GetAudacityCommand(const PluginID & ID);

   EffectMap mEffects;
   AudacityCommandMap mCommands;
   EffectOwnerMap mHostEffects;

   int mNumEffects;
};

#endif
