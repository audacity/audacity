#include "wxlogwrap.h"

#include "wxtypes_convert.h"

#define LOG_TAG "AU3"

#include "log.h"

using namespace au::au3;

WxLogWrap::WxLogWrap()
{
    SetLogLevel(wxLOG_Max);
}

void WxLogWrap::DoLogRecord(wxLogLevel level, const wxString& msg, const wxLogRecordInfo& info)
{
    std::string tag = std::string("[AU3] ") + info.func;
    switch (level) {
    case wxLOG_FatalError:
    case wxLOG_Error:
        LOGE_T(tag)() << wxToStdSting(msg);
        break;
    case wxLOG_Warning:
        LOGW_T(tag)() << wxToStdSting(msg);
        break;
    case wxLOG_Message:
    case wxLOG_Status:
    case wxLOG_Info:
        LOGI_T(tag)() << wxToStdSting(msg);
        break;
    default:
        LOGD_T(tag)() << wxToStdSting(msg);
        break;
    }
}
