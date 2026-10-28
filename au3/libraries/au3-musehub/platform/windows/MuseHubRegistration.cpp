/*
 * Audacity: A Digital Audio Editor
 */
#include "MuseHubService.h"

#include <windows.h>
#include <shlwapi.h>

namespace audacity::musehub {
bool IsMuseHubRegistered()
{
    DWORD size = 0;
    const HRESULT result = AssocQueryStringW(ASSOCF_IS_PROTOCOL, ASSOCSTR_EXECUTABLE, L"musehub", L"open", nullptr, &size);
    return result == S_FALSE && size > 0;
}
}
