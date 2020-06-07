/**********************************************************************

Audacity: A Digital Audio Editor

ImportPlugin.cpp

Paul Licameli split from Import.cpp

**********************************************************************/

#include "ImportPlugin.h"

#include <wx/filename.h>
#include "../widgets/ProgressDialog.h"

ImportFileHandle::ImportFileHandle(const FilePath & filename)
:  mFilename(filename)
{
}

ImportFileHandle::~ImportFileHandle()
{
}

void ImportFileHandle::CreateProgress()
{
   wxFileName ff( mFilename );

   auto title = XO("Importing %s").Format( GetFileDescription() );
   mProgress = std::make_unique< ProgressDialog >(
      title, Verbatim( ff.GetFullName() ) );
}

