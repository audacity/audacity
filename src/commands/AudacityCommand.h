/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityCommand.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_COMMAND__
#define __AUDACITY_COMMAND__



#include <set>

#include <wx/defs.h>

#include "wxPanelWrapper.h" // to inherit

#include "ComponentInterface.h"
#include "EffectAutomationParameters.h"
#include "EffectInterface.h" // for SettingsVisitor type alias
#include "IAudacityCommand.h"

#include "Registrar.h"

class ShuttleGui;

#define BUILTIN_GENERIC_COMMAND_PREFIX wxT("Built-in AudacityCommand: ")

class AudacityProject;
class CommandContext;
class ProgressDialog;


class AUDACITY_DLL_API AudacityCommand /* not final */ :
    public wxEvtHandler,
    public IAudacityCommand
{
 public:
   //std::unique_ptr<CommandOutputTargets> mOutput;
   //CommandOutputTargets * mOutput;
 public:
   AudacityCommand();
   virtual ~AudacityCommand();

   // ComponentInterface implementation

   //These four can be defaulted....
   PluginPath GetPath() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   //  virtual wxString GetFamily();

   //These two must be implemented by instances.
   ComponentInterfaceSymbol GetSymbol() const override = 0;
   virtual TranslatableString GetDescription() const override
   {wxFAIL_MSG( "Implement a Description for this command");return XO("FAIL");};

   virtual ManualPageID ManualPage() override { return {}; }
   virtual bool IsBatchProcessing() const { return mIsBatch; }
   virtual void SetBatchProcessing(bool start) override { mIsBatch = start; }

   virtual bool Apply(const CommandContext & WXUNUSED(context) ) { return false; }

   bool ShowInterface(wxWindow *parent, bool forceModal = false);

   wxDialog *CreateUI(wxWindow *parent, AudacityCommand *client);

   bool SaveSettingsAsString(wxString& parms) override;
   bool LoadSettingsFromString(const wxString& parms) override;

   bool DoAudacityCommand(
      const CommandContext& context, bool shouldPrompt = true) override;

   // Nonvirtual
   // Display a message box, using effect's (translated) name as the prefix
   // for the title.
   enum : long { DefaultMessageBoxStyle = wxOK | wxCENTRE };
   int MessageBox(const TranslatableString& message,
                  long style = DefaultMessageBoxStyle,
                  const TranslatableString& titleStr = {});

//
// protected virtual methods
//
// Each subclass of AudacityCommand overrides one or more of these methods to
// do its processing.
//
//protected:

   virtual bool Init() override;

   virtual bool PromptUser(AudacityProject& project) override;

   // Check whether command should be skipped
   // Typically this is only useful in macros, for example
   // detecting that zero noise reduction is to be done,
   // or that normalisation is being done without Dc bias shift
   // or amplitude modification
   virtual bool CheckWhetherSkipAudacityCommand() { return false; }

   // clean up any temporary memory, needed only per invocation of the
   // effect, after either successful or failed or exception-aborted processing.
   // Invoked inside a "finally" block so it must be no-throw.
   virtual void End(){;};
   virtual void PopulateOrExchange(ShuttleGui & WXUNUSED(S)){return;};
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

   virtual bool VisitSettings( SettingsVisitor & ) override;
   virtual bool VisitSettings( ConstSettingsVisitor & ) override;

protected:

   ProgressDialog *mProgress; // Temporary pointer, NOT deleted in destructor.
   // UI
   wxDialog       *mUIDialog;
   wxWindow       *mUIParent;

private:
   bool mIsBatch;
   bool mNeedsInit;
};


// Base dialog for command dialog.
class AUDACITY_DLL_API AudacityCommandDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   AudacityCommandDialog(wxWindow * parent,
                const TranslatableString & title,
                AudacityCommand * pCommand,
                int type = 0,
                int flags = wxDEFAULT_DIALOG_STYLE,
                int additionalButtons = 0);

   bool Init();// always returns true.  The bool is for the future...

   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;
   bool Validate() override;

   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual void OnOk(wxCommandEvent & evt);
   virtual void OnCancel(wxCommandEvent & evt);
   virtual void OnHelp(wxCommandEvent & evt);

private:
   int mType;
   int mAdditionalButtons;
   AudacityCommand * mpCommand;

   DECLARE_EVENT_TABLE()
   wxDECLARE_NO_COPY_CLASS(AudacityCommandDialog);
};



#endif
