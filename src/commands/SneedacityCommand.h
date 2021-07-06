/**********************************************************************

  Sneedacity: A Digital Audio Editor

  SneedacityCommand.h

  James Crook

**********************************************************************/

#ifndef __SNEEDACITY_COMMAND__
#define __SNEEDACITY_COMMAND__



#include <set>

#include <wx/defs.h>
#include <wx/event.h> // to inherit

#include "../widgets/wxPanelWrapper.h" // to inherit

#include "../include/sneedacity/ComponentInterface.h"
#include "../include/sneedacity/EffectAutomationParameters.h" // for command automation

#include "../Registrar.h"

class ShuttleGui;

#define BUILTIN_GENERIC_COMMAND_PREFIX wxT("Built-in SneedacityCommand: ")

class SneedacityCommand;
class SneedacityProject;
class CommandContext;
class EffectUIHostInterface;
class ProgressDialog;


class SNEEDACITY_DLL_API SneedacityCommand /* not final */ : public wxEvtHandler,
                                public ComponentInterface
{
 public:
   //std::unique_ptr<CommandOutputTargets> mOutput;
   //CommandOutputTargets * mOutput;
 public:
   SneedacityCommand();
   virtual ~SneedacityCommand();
   
   // Type of a registered function that, if it returns true,
   // causes ShowInterface to return early without making any dialog
   using VetoDialogHook = bool (*) ( wxDialog* );
   static VetoDialogHook SetVetoDialogHook( VetoDialogHook hook );

   // ComponentInterface implementation

   //These four can be defaulted....
   PluginPath GetPath() override;
   VendorSymbol GetVendor() override;
   wxString GetVersion() override;
   //  virtual wxString GetFamily();

   //These two must be implemented by instances.
   ComponentInterfaceSymbol GetSymbol() override = 0;
   virtual TranslatableString GetDescription() override
   {wxFAIL_MSG( "Implement a Description for this command");return XO("FAIL");};

   // Name of page in the Sneedacity alpha manual
   virtual ManualPageID ManualPage(){ return {}; }
   virtual bool IsBatchProcessing(){ return mIsBatch;}
   virtual void SetBatchProcessing(bool start){ mIsBatch = start;};
   
   virtual bool Apply(const CommandContext & WXUNUSED(context) ) {return false;};

   bool ShowInterface(wxWindow *parent, bool forceModal = false);
   virtual void SetHostUI(EffectUIHostInterface * WXUNUSED(host)){;};

   wxDialog *CreateUI(wxWindow *parent, SneedacityCommand *client);

   virtual bool GetAutomationParameters(wxString & parms);
   virtual bool SetAutomationParameters(const wxString & parms);

   bool DoSneedacityCommand(wxWindow *parent, const CommandContext & context,bool shouldPrompt = true);

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
// Each subclass of SneedacityCommand overrides one or more of these methods to
// do its processing.
//
//protected:

   // Called once each time an effect is called.  Perform any initialization;
   // make sure that the command can be performed and
   // return false otherwise
   virtual bool Init();

   // If necessary, open a dialog to get parameters from the user.
   // This method will not always be called (for example if a user
   // repeats a command using 'repeat last command') but if it is called, 
   // it will be called after Init.
   virtual bool PromptUser(wxWindow *parent);

   // Check whether command should be skipped
   // Typically this is only useful in automation, for example
   // detecting that zero noise reduction is to be done,
   // or that normalisation is being done without Dc bias shift
   // or amplitude modification
   virtual bool CheckWhetherSkipSneedacityCommand() { return false; }

   // clean up any temporary memory, needed only per invocation of the
   // effect, after either successful or failed or exception-aborted processing.
   // Invoked inside a "finally" block so it must be no-throw.
   virtual void End(){;};
   virtual void PopulateOrExchange(ShuttleGui & WXUNUSED(S)){return;};
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

protected:

   ProgressDialog *mProgress; // Temporary pointer, NOT deleted in destructor.
   // UI
   wxDialog       *mUIDialog;
   wxWindow       *mUIParent;
   int             mUIResultID;

private:
   bool mIsBatch;
   bool mUIDebug;
   bool mNeedsInit;
};


// Base dialog for command dialog.
class SNEEDACITY_DLL_API SneedacityCommandDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   SneedacityCommandDialog(wxWindow * parent,
                const TranslatableString & title,
                SneedacityCommand * pCommand,
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
   SneedacityCommand * mpCommand;

   DECLARE_EVENT_TABLE()
   wxDECLARE_NO_COPY_CLASS(SneedacityCommandDialog);
};



#endif
