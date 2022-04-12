/*!********************************************************************

Audacity: A Digital Audio Editor

@file wxWidgetsBasicUI.cpp

Paul Licameli

**********************************************************************/
#include "wxWidgetsBasicUI.h"
#include "wxWidgetsWindowPlacement.h"
#include "MemoryX.h" // for Destroy_ptr
#include "widgets/ErrorDialog.h"
#ifdef HAS_SENTRY_REPORTING
#include "widgets/ErrorReportDialog.h"
#endif
#include "widgets/AudacityMessageBox.h"
#include "ProgressDialog.h"
#include "MultiDialog.h"
#include <wx/app.h>
#include <wx/progdlg.h>
#include <wx/windowptr.h>

using namespace BasicUI;

wxWidgetsBasicUI::~wxWidgetsBasicUI() = default;

void wxWidgetsBasicUI::DoCallAfter(const Action &action)
{
   wxTheApp->CallAfter(action);
}

void wxWidgetsBasicUI::DoYield()
{
   wxTheApp->Yield();
}

void wxWidgetsBasicUI::DoShowErrorDialog(
   const BasicUI::WindowPlacement &placement,
   const TranslatableString &dlogTitle,
   const TranslatableString &message,
   const ManualPageID &helpPage,
   const BasicUI::ErrorDialogOptions &options)
{
   using namespace BasicUI;
   bool modal = true;
   auto parent = wxWidgetsWindowPlacement::GetParent(placement);
   switch (options.type) {
      case ErrorDialogType::ModalErrorReport: {
#ifdef HAS_SENTRY_REPORTING
         ErrorReportDialog dlog(parent, dlogTitle, message, helpPage,
            options.log, modal);

         dlog.CentreOnParent();
         dlog.ShowModal();
         return;
#else
         break;
#endif
      }
      case ErrorDialogType::ModelessError: {
         if (!parent)
            parent = wxTheApp->GetTopWindow();
         // To be nonmodal, either it needs a parent, to avoid leaks, or it must
         // guarantee eventual deletion of itself.  There might be no top window
         // on MacOS.  Let's just force it to be modal in that case.
         if (parent)
            modal = false;
         break;
      }
      default:
         break;
   }
   auto pDlog = Destroy_ptr<ErrorDialog>( safenew ErrorDialog{ parent,
      dlogTitle, message, helpPage, options.log,
      options.modalHelp, modal } );
   pDlog->CentreOnParent();
   if (modal)
      pDlog->ShowModal();
   else {
      pDlog->Show();
      pDlog.release(); // not a memory leak, because it has a parent
   }
}

BasicUI::MessageBoxResult
wxWidgetsBasicUI::DoMessageBox(
   const TranslatableString &message,
   MessageBoxOptions options)
{
   // Compute the style argument to pass to wxWidgets
   long style = 0;
   switch (options.iconStyle) {
      case Icon::Warning :
         style = wxICON_WARNING;
         break;
      case Icon::Error :
         style = wxICON_ERROR;
         break;
      case Icon::Question :
         style = wxICON_QUESTION;
         break;
      case Icon::Information :
         style = wxICON_INFORMATION;
         break;
      default:
         break;
   }
   switch (options.buttonStyle) {
      case Button::Ok :
         style |= wxOK;
         break;
      case Button::YesNo :
         style |= wxYES_NO;
         break;
      default:
         break;
   }
   if (!options.yesOrOkDefaultButton && options.buttonStyle == Button::YesNo)
      style |= wxNO_DEFAULT;
   if (options.cancelButton)
      style |= wxCANCEL;
   if (options.centered)
      style |= wxCENTER;

   // Preserving the default style AudacityMessageBox had,
   // when none of the above were explicitly specified
   if (!style)
      style = wxOK | wxCENTRE;

   // This calls through to ::wxMessageBox:
   auto wxResult =
      ::AudacityMessageBox(message, options.caption, style,
         options.parent
           ? wxWidgetsWindowPlacement::GetParent(*options.parent)
           : nullptr);
   // This switch exhausts all possibilities for the return from::wxMessageBox.
   // see utilscmn.cpp in wxWidgets.
   // Remap to our toolkit-neutral enumeration.
   switch (wxResult) {
   case wxYES:
      return MessageBoxResult::Yes;
   case wxNO:
      return MessageBoxResult::No;
   case wxOK:
      return MessageBoxResult::Ok;
   case wxCANCEL:
      return MessageBoxResult::Cancel;
   case wxHELP:
      // should not happen, because we don't ever pass wxHELP
   default:
      wxASSERT(false);
      return MessageBoxResult::None;
   }
}

namespace {
struct MyProgressDialog : ::ProgressDialog, BasicUI::ProgressDialog {
   using ::ProgressDialog::ProgressDialog;
   ~MyProgressDialog() override = default;
   ProgressResult Poll(
      unsigned long long numerator,
      unsigned long long denominator,
      const TranslatableString &message) override
   {
      return Update(numerator, denominator, message);
   }
   virtual void SetMessage(const TranslatableString & message) override
   {
      ::ProgressDialog::SetMessage(message);
   }
};
}

std::unique_ptr<BasicUI::ProgressDialog>
wxWidgetsBasicUI::DoMakeProgress(const TranslatableString & title,
   const TranslatableString &message,
   unsigned flags,
   const TranslatableString &remainingLabelText)
{
   unsigned options = 0;
   if (~(flags & ProgressShowStop))
      options |= pdlgHideStopButton;
   if (~(flags & ProgressShowCancel))
      options |= pdlgHideCancelButton;
   if ((flags & ProgressHideTime))
      options |= pdlgHideElapsedTime;
   if ((flags & ProgressConfirmStopOrCancel))
      options |= pdlgConfirmStopCancel;
   // Usually wxWindow objects should not be managed by std::unique_ptr
   // See https://docs.wxwidgets.org/3.0/overview_windowdeletion.html
   // But on macOS the use of wxWindowPtr for the progress dialog sometimes
   // causes hangs.
   return std::make_unique<MyProgressDialog>(
      title, message, options, remainingLabelText);
}

namespace {
struct MyGenericProgress : wxGenericProgressDialog, GenericProgressDialog {
   MyGenericProgress(const TranslatableString &title,
      const TranslatableString &message,
      wxWindow *parent = nullptr)
      : wxGenericProgressDialog{
         title.Translation(), message.Translation(),
         300000,     // range
         parent,
         wxPD_APP_MODAL | wxPD_ELAPSED_TIME | wxPD_SMOOTH
      }
   {}
   ~MyGenericProgress() override = default;
   void Pulse() override { wxGenericProgressDialog::Pulse(); }
};
}

std::unique_ptr<GenericProgressDialog>
wxWidgetsBasicUI::DoMakeGenericProgress(
   const BasicUI::WindowPlacement &placement,
   const TranslatableString &title,
   const TranslatableString &message)
{
   return std::make_unique<MyGenericProgress>(
      title, message, wxWidgetsWindowPlacement::GetParent(placement));
}

int wxWidgetsBasicUI::DoMultiDialog(const TranslatableString &message,
   const TranslatableString &title,
   const TranslatableStrings &buttons,
   const ManualPageID &helpPage,
   const TranslatableString &boxMsg, bool log)
{
   return ::ShowMultiDialog(message, title, buttons, helpPage, boxMsg, log);
}
