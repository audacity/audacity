#pragma once

#include <wx/arrstr.h>

namespace audacity
{
    namespace update_manager
    {

        // Initialized of current build version by default.
        class VersionId final
        {
        public:
            VersionId() = default;
            VersionId(size_t version, size_t release, size_t revision);

            VersionId(const VersionId&) = default;
            VersionId(VersionId&&) = default;

            static wxString MakeString(size_t version, size_t release, size_t revision);
            static VersionId ParseFromString(wxString& versionString);

            wxString getString() const;
            bool isZero();

            bool operator== (const VersionId& other);
            bool operator!= (const VersionId& other);

            bool operator< (const VersionId& other);
            bool operator> (const VersionId& other);

        private:
            size_t mVersion{ AUDACITY_VERSION };
            size_t mRelease{ AUDACITY_RELEASE };
            size_t mRevision{ AUDACITY_REVISION };
        };

    }
}
