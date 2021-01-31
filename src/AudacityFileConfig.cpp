/**********************************************************************

Audacity: A Digital Audio Editor

AudacityFileConfig.cpp

Paul Licameli split from Prefs.cpp

**********************************************************************/

#include "Audacity.h"
#include "AudacityFileConfig.h"

AudacityFileConfig::AudacityFileConfig(
   const wxString& appName,
   const wxString& vendorName,
   const wxString& localFilename,
   const wxString& globalFilename,
   long style,
   const wxMBConv& conv
)
: FileConfig{ appName, vendorName, localFilename, globalFilename, style, conv }
{}

AudacityFileConfig::~AudacityFileConfig() = default;

std::unique_ptr<AudacityFileConfig> AudacityFileConfig::Create(
   const wxString& appName,
   const wxString& vendorName,
   const wxString& localFilename,
   const wxString& globalFilename,
   long style,
   const wxMBConv& conv
)
{
   // Private ctor means make_unique can't compile, so this verbosity:
   auto result = std::unique_ptr<AudacityFileConfig>{
      safenew AudacityFileConfig{
         appName, vendorName, localFilename, globalFilename, style, conv } };
   result->Init();
   return result;
}
