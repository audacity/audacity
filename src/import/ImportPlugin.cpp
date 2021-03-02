/**********************************************************************

Audacity: A Digital Audio Editor

ImportPlugin.cpp

Paul Licameli split from Import.cpp

**********************************************************************/

#include "ImportPlugin.h"

#include <wx/filename.h>
#include "../WaveTrack.h"
#include "../widgets/ProgressDialog.h"
#include "QualitySettings.h"

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

sampleFormat ImportFileHandle::ChooseFormat(sampleFormat effectiveFormat)
{
   // Consult user preference
   auto defaultFormat = QualitySettings::SampleFormatChoice();

   // Don't choose format narrower than effective or default
   auto format = std::max(effectiveFormat, defaultFormat);

   // But also always promote 24 bits to float
   if (format > int16Sample)
      format = floatSample;

   return format;
}

std::shared_ptr<WaveTrack> ImportFileHandle::NewWaveTrack(
   WaveTrackFactory &trackFactory, sampleFormat effectiveFormat, double rate)
{
   return trackFactory.NewWaveTrack(ChooseFormat(effectiveFormat), rate);
}
