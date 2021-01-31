/**********************************************************************

Audacity: A Digital Audio Editor

@file AudacityFileConfig.h
@brief Extend FileConfig with application-specific behavior

Paul Licameli split from Prefs.h

**********************************************************************/

#ifndef __AUDACITY_FILE_CONFIG__
#define __AUDACITY_FILE_CONFIG__

#include "widgets/FileConfig.h" // to inherit

/// \brief Our own specialisation of FileConfig.  It is essentially a renaming.
class AUDACITY_DLL_API AudacityFileConfig final : public FileConfig
{
public:
   using FileConfig::FileConfig;

   ~AudacityFileConfig() override;
};
#endif
