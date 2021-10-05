/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityLogger.cpp

******************************************************************//**

\class AudacityLogger
\brief AudacityLogger is a thread-safe logger class

Provides thread-safe logging based on the wxWidgets log facility.

*//*******************************************************************/


#include "AudacityLogger.h"

#include "Internat.h"
#include "MemoryX.h"

#include <memory>
#include <mutex>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/tokenzr.h>

//
// AudacityLogger class
//
// By providing an Audacity specific logging class, it can be made thread-safe and,
//     as such, can be used by the ever growing threading within Audacity.
//

AudacityLogger *AudacityLogger::Get()
{
   static std::once_flag flag;
   std::call_once( flag, []{
      // wxWidgets will clean up the logger for the main thread, so we can say
      // safenew.  See:
      // http://docs.wxwidgets.org/3.0/classwx_log.html#a2525bf54fa3f31dc50e6e3cd8651e71d
      std::unique_ptr < wxLog > // DELETE any previous logger
         { wxLog::SetActiveTarget(safenew AudacityLogger) };
   } );

   // Use dynamic_cast so that we get a NULL ptr in case our logger
   // is no longer the target.
   return dynamic_cast<AudacityLogger *>(wxLog::GetActiveTarget());
}

AudacityLogger::AudacityLogger()
:  wxEvtHandler(),
   wxLog()
{
   mUpdated = false;
}

AudacityLogger::~AudacityLogger()  = default;

void AudacityLogger::Flush()
{
   if (mUpdated && mListener && mListener())
      mUpdated = false;
}

auto AudacityLogger::SetListener(Listener listener) -> Listener
{
   auto result = std::move(mListener);
   mListener = std::move(listener);
   return result;
}

void AudacityLogger::DoLogText(const wxString & str)
{
   if (!wxIsMainThread()) {
      wxMutexGuiEnter();
   }

   if (mBuffer.empty()) {
      wxString stamp;

      TimeStamp(&stamp);

      mBuffer << stamp << _TS("Audacity ") << AUDACITY_VERSION_STRING << wxT("\n");
   }

   mBuffer << str << wxT("\n");

   mUpdated = true;

   Flush();

   if (!wxIsMainThread()) {
      wxMutexGuiLeave();
   }
}

bool AudacityLogger::SaveLog(const wxString &fileName) const
{
   wxFFile file(fileName, wxT("w"));

   if (file.IsOpened()) {
      file.Write(mBuffer);
      file.Close();
      return true;
   }

   return false;
}

bool AudacityLogger::ClearLog()
{
   mBuffer = wxEmptyString;
   DoLogText(wxT("Log Cleared."));

   return true;
}

wxString AudacityLogger::GetLog(int count)
{
   if (count == 0)
   {
      return mBuffer;
   }

   wxString buffer;

   auto lines = wxStringTokenize(mBuffer, wxT("\r\n"), wxTOKEN_RET_DELIMS);
   for (int index = lines.size() - 1; index >= 0 && count > 0; --index, --count)
   {
      buffer.Prepend(lines[index]);
   }

   return buffer;
}
