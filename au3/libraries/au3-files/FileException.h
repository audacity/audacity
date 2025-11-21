/*!
  @file FileException.h
  @brief MessageBoxException for failures of file operations


  Created by Paul Licameli on 11/22/16.

*/

#ifndef __AUDACITY_FILE_EXCEPTION__
#define __AUDACITY_FILE_EXCEPTION__

#include "AudacityException.h" // to inherit
#include <wx/filename.h> // wxFileName member variable

//! Thrown for failure of file or database operations in deeply nested places
class FILES_API FileException /* not final */ : public MessageBoxException
{
public:
    //! Identifies file operation that failed
    enum class Cause {
        Open,
        Read,
        Write, //!< most important to detect when storage space is exhausted
        Rename //!< involves two filenames
    };

    explicit FileException(
        Cause cause_, //!< What kind of file operation failed
        const wxFileName& fileName_, //!< Which file suffered a failure
        const TranslatableString& caption = XO("File Error"), //!< Shown in message box frame, not the main message
        const wxFileName& renameTarget_ = {} //!< A second file name, only for renaming failure
        )
    // DV: We consider a FileException to be internal for now.
    // We used to have some odd cases related to the read-only folder in 3.0.0,
    // so it is good to have a full picture here.
    // In case we see too many - we will tweak this behavior in the next release.
        : MessageBoxException{ExceptionType::Internal, caption}
        , cause{cause_}, fileName{fileName_}, renameTarget{renameTarget_}
    {}

    FileException(const FileException& that)
        : MessageBoxException(that)
        , cause{that.cause}
        , fileName{that.fileName}
        , renameTarget{that.renameTarget}
    {}

    FileException& operator=(FileException&&) = delete;

    ~FileException() override;

    static TranslatableString WriteFailureMessage(const wxFileName& fileName);

protected:
    //! %Format an error message appropriate for the @ref Cause.
    TranslatableString ErrorMessage() const override;
    wxString ErrorHelpUrl() const override;

public:
    Cause cause;
    wxFileName fileName;
    wxFileName renameTarget;
};

#endif
