/**********************************************************************

Audacity: A Digital Audio Editor

@file AudacityFileConfig.h
@brief Extend FileConfig with application-specific behavior

Paul Licameli split from Prefs.h

**********************************************************************/

#ifndef __AUDACITY_FILE_CONFIG__
#define __AUDACITY_FILE_CONFIG__

#include <memory>
#include <wx/fileconf.h>

/// \brief Our own specialisation of FileConfig.
class AUDACITY_DLL_API AudacityFileConfig final : public wxFileConfig
{
public:
    //! Require a call to this factory, to guarantee proper two-phase initialization
    static std::unique_ptr<AudacityFileConfig> Create(
        const wxString& appName = {}, const wxString& vendorName = {}, const wxString& localFilename = {},
        const wxString& globalFilename = {}, long style = wxCONFIG_USE_LOCAL_FILE | wxCONFIG_USE_GLOBAL_FILE,
        const wxMBConv& conv = wxConvAuto());

    bool Flush(bool bCurrentOnly) override;

    ~AudacityFileConfig() override;

    bool RenameEntry(const wxString& oldName, const wxString& newName) override;
    bool RenameGroup(const wxString& oldName, const wxString& newName) override;
    bool DeleteEntry(const wxString& key, bool bDeleteGroupIfEmpty) override;
    bool DeleteGroup(const wxString& key) override;
    bool DeleteAll() override;

protected:
    bool DoWriteString(const wxString& key, const wxString& szValue) override;
    bool DoWriteLong(const wxString& key, long lValue) override;
#if wxUSE_BASE64
    bool DoWriteBinary(const wxString& key, const wxMemoryBuffer& buf) override;
#endif

private:
    //! Disallow direct constructor call, because a two-phase initialization is required
    AudacityFileConfig(
        const wxString& appName, const wxString& vendorName, const wxString& localFilename, const wxString& globalFilename, long style,
        const wxMBConv& conv);

    void Init();
    void Warn() const;

    //wxFileConfig already has m_isDirty flag, but it's inaccessible
    bool mDirty{ false };
    const wxString mLocalFilename;
};
#endif
