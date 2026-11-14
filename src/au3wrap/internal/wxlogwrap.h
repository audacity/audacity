#ifndef AU_AU3WRAP_WXLOGWRAP_H
#define AU_AU3WRAP_WXLOGWRAP_H

#include <wx/log.h>

namespace au::au3 {
class WxLogWrap : public wxLog
{
public:
    WxLogWrap();

private:
    void DoLogRecord(wxLogLevel level, const wxString& msg, const wxLogRecordInfo& info) override;
};
}

#endif // AU_AU3WRAP_WXLOGWRAP_H
