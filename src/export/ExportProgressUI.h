/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportProgressUI.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <future>

#include "Export.h"
#include "ExportTypes.h"
#include "BasicUI.h"
#include "ExportPlugin.h"
#include "wxFileNameWrapper.h"

class ExportPluginDelegate;
class Exporter;

namespace ExportProgressUI
{
   ExportResult Show(ExportTask exportTask);

   template<typename Callable>
   void ExceptionWrappedCall(Callable callable)
   {
      try
      {
         return (void)callable();
      }
      
      catch(ExportErrorCodeException& e)
      {
         ShowExportErrorDialog({ e.What() });
      }
      catch(ExportDiskFullError& e)
      {
         ShowDiskFullExportErrorDialog(e.What());
      }
      catch(ExportErrorException& e)
      {
         BasicUI::ShowErrorDialog( { },
            XO("Error Exporting"), TranslatableString{e.What(), {}},
            e.GetHelpPage());
      }
      catch(ExportException& e)
      {
         BasicUI::ShowMessageBox(TranslatableString { e.What(), {}});
      }
      catch(...)
      {
         BasicUI::ShowMessageBox(XO("Unknown export error"));
      }
   }

}
