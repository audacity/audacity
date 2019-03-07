/**********************************************************************

  Audacity: A Digital Audio Editor

  MacroCommands.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_COMMANDS_DIALOG__
#define __AUDACITY_BATCH_COMMANDS_DIALOG__

#include <wx/defs.h>
#include <wx/string.h>

#include "export/Export.h"

class Effect;
class CommandContext;
class AudacityProject;
class wxArrayStringEx;

class MacroCommandsCatalog {
public:
   // A triple of user-visible name, internal string identifier and type/help string.
   struct Entry {
      TranslatedInternalString name;
      wxString category;
   };
   using Entries = std::vector<Entry>;

   MacroCommandsCatalog( const AudacityProject *project );

   // binary search
   Entries::const_iterator ByFriendlyName( const wxString &friendlyName ) const;
   // linear search
   Entries::const_iterator ByCommandId( const wxString &commandId ) const;

   // Lookup by position as sorted by friendly name
   const Entry &operator[] ( size_t index ) const { return mCommands[index]; }

   Entries::const_iterator begin() const { return mCommands.begin(); }
   Entries::const_iterator end() const { return mCommands.end(); }

private:
   // Sorted by friendly name
   Entries mCommands;
};

// Stores information for one macro
class MacroCommands final {
 public:
   // constructors and destructors
   MacroCommands();
 public:
   bool ApplyMacro( const MacroCommandsCatalog &catalog,
      const wxString & filename = {});
   bool ApplyCommand( const wxString &friendlyCommand,
      const wxString & command, const wxString & params,
      CommandContext const * pContext=NULL );
   bool ApplyCommandInBatchMode( const wxString &friendlyCommand,
      const wxString & command, const wxString &params,
      CommandContext const * pContext = NULL);
   bool ApplySpecialCommand(
      int iCommand, const wxString &friendlyCommand,
      const wxString & command, const wxString & params);
   bool ApplyEffectCommand(
      const PluginID & ID, const wxString &friendlyCommand,
      const wxString & command,
      const wxString & params, const CommandContext & Context);
   bool ReportAndSkip( const wxString & friendlyCommand, const wxString & params );
   void AbortBatch();

   // Utility functions for the special commands.
   static wxString BuildCleanFileName(const FilePath &fileName,
      const FileExtension &extension);
   bool WriteMp3File( const wxString & Name, int bitrate );
   double GetEndTime();
   static bool IsMono();

   // These commands do not depend on the command list.
   static void MigrateLegacyChains();
   static wxArrayString GetNames();
   static wxArrayStringEx GetNamesOfDefaultMacros();

   static wxString GetCurrentParamsFor(const wxString & command);
   static wxString PromptForParamsFor(const wxString & command, const wxString & params, wxWindow *parent);
   static wxString PromptForPresetFor(const wxString & command, const wxString & params, wxWindow *parent);

   // These commands do depend on the command list.
   void ResetMacro();

   void RestoreMacro(const wxString & name);
   bool ReadMacro(const wxString & macro);
   bool WriteMacro(const wxString & macro);
   bool AddMacro(const wxString & macro);
   bool DeleteMacro(const wxString & name);
   bool RenameMacro(const wxString & oldmacro, const wxString & newmacro);

   void AddToMacro(const wxString & command, int before = -1);
   void AddToMacro(const wxString & command, const wxString & params, int before = -1);
   void DeleteFromMacro(int index);
   wxString GetCommand(int index);
   wxString GetParams(int index);
   int GetCount();
   wxString GetMessage(){ return mMessage;};
   void AddToMessage(const wxString & msgIn ){ mMessage += msgIn;};

   bool IsFixed(const wxString & name);

   void Split(const wxString & str, wxString & command, wxString & param);
   wxString Join(const wxString & command, const wxString & param);

   wxArrayString mCommandMacro;
   wxArrayString mParamsMacro;
   bool mAbort;
   wxString mMessage;

   Exporter mExporter;
   wxString mFileName;
};

#endif
