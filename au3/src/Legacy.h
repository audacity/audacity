/**********************************************************************

  Audacity: A Digital Audio Editor

  Legacy.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>

class wxFileName;

//! Update Audacity 1.0 file in-place to XML format
/*! @return true if successful, else no effect on the file
 @excsafety{Strong}
 */
bool ConvertLegacyProjectFile(const wxFileName& filename);
