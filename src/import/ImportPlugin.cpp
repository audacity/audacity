/**********************************************************************

Audacity: A Digital Audio Editor

ImportPlugin.cpp

Paul Licameli split from Import.cpp

**********************************************************************/

#include "ImportPlugin.h"

#include <wx/filename.h>
#include "ProgressDialog.h"

ImportPlugin::ImportPlugin(FileExtensions supportedExtensions):
   mExtensions( std::move( supportedExtensions ) )
{
}

ImportPlugin::~ImportPlugin() = default;

FileExtensions ImportPlugin::GetSupportedExtensions()
{
   return mExtensions;
}

bool ImportPlugin::SupportsExtension(const FileExtension &extension)
{
   // Case-insensitive check if extension is supported
   return mExtensions.Index(extension, false) != wxNOT_FOUND;
}

TranslatableString ImportPlugin::FailureHint() const
{
   return {};
}

ImportFileHandle::ImportFileHandle(const FilePath & filename)
:  mFilename(filename)
{
}

ImportFileHandle::~ImportFileHandle() = default;

void ImportFileHandle::CreateProgress()
{
   wxFileName ff( mFilename );

   auto title = XO("Importing %s").Format( GetFileDescription() );
   mProgress = std::make_unique< ProgressDialog >(
      title, Verbatim( ff.GetFullName() ) );
}
