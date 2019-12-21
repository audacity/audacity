#ifndef __AUDACITY_MISSING_ALIAS_FILES_DIALOG__
#define __AUDACITY_MISSING_ALIAS_FILES_DIALOG__

class AliasBlockFile;
class AudacityProject;
class TranslatableString;
class wxDialog;
class TranslatableString;

#include <memory>
#include <utility>
#include <wx/string.h>

namespace MissingAliasFilesDialog {

/** \brief Mark playback as having missing aliased blockfiles
  *
  * Playback will continue, but the missing files will be silenced
  * ShouldShow can be called to determine
  * if the user should be notified
  */
void Mark(const AliasBlockFile *b);

// Retrieve information left by Mark
std::pair< wxString, std::shared_ptr<AudacityProject> > Marked();

/** \brief Changes the behavior of missing aliased blockfiles warnings
  */
void SetShouldShow(bool b);

/** \brief Returns true if the user should be notified of missing alias warnings
  */
bool ShouldShow();

/// Displays a custom modeless error dialog for aliased file errors
void Show(AudacityProject *parent,
                     const TranslatableString &dlogTitle,
                     const TranslatableString &message,
                     const wxString &helpPage,
                     const bool Close = true);

wxDialog *Find( const AudacityProject &project );

}

#endif
