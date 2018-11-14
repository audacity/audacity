//
//  FileException.h
//  
//
//  Created by Paul Licameli on 11/22/16.
//
//

#ifndef __AUDACITY_FILE_EXCEPTION__
#define __AUDACITY_FILE_EXCEPTION__

#include "AudacityException.h"
#include <wx/filename.h> // wxFileName member variable below

class FileException /* not final */ : public MessageBoxException
{
public:
   enum class Cause { Open, Read, Write, Rename };

   explicit FileException
      ( Cause cause_, const wxFileName &fileName_,
        const wxString &caption = _("File Error"),
        const wxFileName &renameTarget_ = {})
   : MessageBoxException{ caption }
   , cause{ cause_ }, fileName{ fileName_ }, renameTarget{ renameTarget_ }
   {}

   FileException(FileException&& that)
      : MessageBoxException(std::move(that))
      , cause{ that.cause }
      , fileName{ that.fileName }
      , renameTarget{ that.renameTarget }
   {}

   FileException& operator= (FileException&&) PROHIBITED;

   ~FileException() override;

protected:
   // Format a default, internationalized error message for this exception.
   wxString ErrorMessage() const override;

public:
   Cause cause;
   wxFileName fileName;
   wxFileName renameTarget;
};

#endif
