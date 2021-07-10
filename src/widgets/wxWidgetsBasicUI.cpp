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
