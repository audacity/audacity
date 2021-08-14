/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIORegistry.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FILE_IO_REGISTRY__
#define __AUDACITY_PROJECT_FILE_IO_REGISTRY__

#include <wx/string.h>
#include <functional>
#include <unordered_map>

class AudacityProject;
class XMLTagHandler;

class AUDACITY_DLL_API ProjectFileIORegistry {
public:

//! Type of functions returning objects that interpret a part of the saved XML
/*!
 This "accessor" may or may not act as a "factory" that builds a new object and
 may return nullptr.  Caller of the accessor is not responsible for the object
 lifetime, which is assumed to outlast the project loading procedure.
 */
using ObjectAccessor =
   std::function< XMLTagHandler *( AudacityProject & ) >;

// Typically statically constructed
struct AUDACITY_DLL_API ObjectReaderEntry {
   ObjectReaderEntry( const wxString &tag, ObjectAccessor fn );
};

XMLTagHandler *CallObjectAccessor(
   const wxString &tag, AudacityProject & );

//! Get the unique instance
static ProjectFileIORegistry &Get();

private:
   
using TagTable =
   std::unordered_map< wxString, ObjectAccessor >;
TagTable mTagTable;

};

#endif
