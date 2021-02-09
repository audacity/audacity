/*!********************************************************************

Audacity: A Digital Audio Editor

@file wxWidgetsBasicUI.cpp

Paul Licameli

**********************************************************************/
#include "wxWidgetsBasicUI.h"
#include "MemoryX.h" // for Destroy_ptr
#include "widgets/ErrorDialog.h"
#ifdef HAS_SENTRY_REPORTING
#include "widgets/ErrorReportDialog.h"
#endif
#include "widgets/AudacityMessageBox.h"
#include <wx/app.h>

using namespace BasicUI;

wxWidgetsWindowPlacement::~wxWidgetsWindowPlacement() = default;

wxWidgetsBasicUI::~wxWidgetsBasicUI() = default;

void wxWidgetsBasicUI::DoCallAfter(const Action &action)
{
   wxTheApp->CallAfter(action);
}

void wxWidgetsBasicUI::DoYield()
{
   wxTheApp->Yield();
}

namespace {
wxWindow *GetParent(const BasicUI::WindowPlacement &placement)
{
   if (auto *pPlacement =
       dynamic_cast<const wxWidgetsWindowPlacement*>(&placement))
      return pPlacement->pWindow;
   return nullptr;
}
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
   auto parent = GetParent(placement);
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
         options.parent ? GetParent(*options.parent) : nullptr);
   // This switch exhausts all possibilities for the return from::wxMessageBox.
   // see utilscmn.cpp in wxWidgets.
   // Remap to our toolkit-neutral enumeration.
   switch (wxResult) {
   case wxID_YES:
      return MessageBoxResult::Yes;
   case wxID_NO:
      return MessageBoxResult::No;
   case wxID_OK:
      return MessageBoxResult::Ok;
   case wxID_CANCEL:
      return MessageBoxResult::Cancel;
   case wxID_HELP:
      // should not happen, because we don't ever pass wxHELP
   default:
      wxASSERT(false);
      return MessageBoxResult::None;
   }
}
