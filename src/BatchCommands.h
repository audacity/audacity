/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommands.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_COMMANDS_DIALOG__
#define __AUDACITY_BATCH_COMMANDS_DIALOG__

#include <wx/defs.h>
#include <wx/string.h>

#include "export/Export.h"

class Effect;

class BatchCommands {
 public:
   // constructors and destructors
   BatchCommands();
 public:
   bool ApplyChain(const wxString & filename = wxT(""));
   bool ApplyCommand( const wxString command, const wxString params );
   bool ApplyCommandInBatchMode(const wxString & command, const wxString &params);
   bool ApplySpecialCommand(int iCommand, const wxString command,const wxString params);
   bool ApplyEffectCommand(Effect * f, const wxString command, const wxString params);
   bool ReportAndSkip( const wxString command, const wxString params );
   void AbortBatch();

   // Utility functions for the special commands.
   bool WriteMp3File( const wxString Name, int bitrate );
   double GetEndTime();
   bool IsMono();

   // These commands do not depend on the command list.
   wxArrayString GetNames();
   static bool PromptForParamsFor( wxString command, wxWindow *parent );
   static wxString GetCurrentParamsFor( wxString command );
   static bool SetCurrentParametersFor( Effect * f, const wxString command, const wxString params);
   static wxArrayString GetAllCommands();

   // These commands do depend on the command list.
   void ResetChain();

   bool ReadChain(const wxString & chain);
   bool WriteChain(const wxString & chain);
   bool AddChain(const wxString & chain);
   bool DeleteChain(const wxString & name);
   bool RenameChain(const wxString & oldchain, const wxString & newchain);

   void AddToChain(const wxString & command, int before = -1);
   void AddToChain(const wxString & command, const wxString & params, int before = -1);
   void DeleteFromChain(int index);
   wxString GetCommand(int index);
   wxString GetParams(int index);
   int GetCount();

   void SetWavToMp3Chain();

   bool IsFixed(const wxString & name);

   void RestoreChain(const wxString & name);

   void Split(const wxString & str, wxString & command, wxString & param);
   wxString Join(const wxString & command, const wxString & param);

   wxArrayString mCommandChain;
   wxArrayString mParamsChain;
   bool mAbort;

   Exporter mExporter;
   wxString mFileName;
};

#endif
