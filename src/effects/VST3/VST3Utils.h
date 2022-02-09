/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VSTUtils.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <string>
#include <pluginterfaces/vst/vsttypes.h>

class wxString;
class wxWindow;

namespace Steinberg
{
   namespace Vst
   {
      class IEditController;
      class IComponentHandler;
      struct ParameterInfo;
   }
}

/**
 * \brief Provides a set of useful functions, used across the Audacity VST3 module
 */
class VST3Utils final
{
public:

   //Creates a plugin path string, which can be used to uniquely identify the effect.
   //modulePath - path to the vst3 file/catalog
   //effectUIDString - the effect's VST3::UID converted to a string
   static wxString MakePluginPathString(const wxString& modulePath, const std::string& effectUIDString);

   //Attempts to parse plugin path string, returns true if string is considered
   //to be a valid plugin path. modulePath and effectUIDString are written if
   //provided.
   static bool ParsePluginPath(const wxString& pluginPath, wxString* modulePath, std::string* effectUIDString);
};
