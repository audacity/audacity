/**********************************************************************

Sneedacity: A Digital Audio Editor

@file SneedacityFileConfig.h
@brief Extend FileConfig with application-specific behavior

Paul Licameli split from Prefs.h

**********************************************************************/

#ifndef __SNEEDACITY_FILE_CONFIG__
#define __SNEEDACITY_FILE_CONFIG__

#include <memory>
#include "widgets/FileConfig.h" // to inherit

/// \brief Our own specialisation of FileConfig.
class SNEEDACITY_DLL_API SneedacityFileConfig final : public FileConfig
{
public:
   //! Require a call to this factory, to guarantee proper two-phase initialization
   static std::unique_ptr<SneedacityFileConfig> Create(
      const wxString& appName = {},
      const wxString& vendorName = {},
      const wxString& localFilename = {},
      const wxString& globalFilename = {},
      long style = wxCONFIG_USE_LOCAL_FILE | wxCONFIG_USE_GLOBAL_FILE,
      const wxMBConv& conv = wxConvAuto()
   );

   ~SneedacityFileConfig() override;

protected:
   void Warn() override;

private:
   //! Disallow direct constructor call, because a two-phase initialization is required
   SneedacityFileConfig(
      const wxString& appName,
      const wxString& vendorName,
      const wxString& localFilename,
      const wxString& globalFilename,
      long style,
      const wxMBConv& conv
   );
};
#endif
