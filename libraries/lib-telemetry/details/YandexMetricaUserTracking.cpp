/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file YandexMetricaUserTracking.cpp
 @brief Define a class for reporting session events to Yandex Metrica.

 Dmitry Vedenko
 **********************************************************************/

#include "YandexMetricaUserTracking.h"

#include <wx/file.h>
#include <wx/tokenzr.h>

#include "lib-network-manager/NetworkManager.h"
#include "lib-network-manager/IResponse.h"
#include "lib-network-manager/Request.h"
#include "lib-network-manager/IResponse.h"

#include "lib-string-utils/CodeConversions.h"

#include "CommonHeaders.h"

namespace audacity
{
namespace telemetry
{
namespace details
{
YandexMetricaUserTracking::YandexMetricaUserTracking (std::string cookiesPath, const std::string& trackingID, const CommonHeaders* commonHeaders)
    : mCookiesPath(std::move (cookiesPath)),
    mUrl ("https://mc.yandex.ru/watch/" + trackingID),
    mCommonHeaders (commonHeaders)
{
    loadCookies ();
}

void YandexMetricaUserTracking::reportAppStarted ()
{
    network_manager::Request request (mUrl);

    mCommonHeaders->setupHeaders (&request);

    {
        std::lock_guard<std::mutex> lock (mCookiesMutex);
        request.appendCookies (mCookieList);
    }

    auto response = network_manager::NetworkManager::GetInstance ().doGet (request);

    response->setRequestFinishedCallback ([response, this](auto) {
       if (response->getError() != network_manager::NetworkError::NoError)
           return ;

        mCookieList = response->getCookies ();
        storeCookies ();
    });
}

void YandexMetricaUserTracking::reportFinished ()
{
    // Session end is not reported
}

void YandexMetricaUserTracking::loadCookies ()
{
    if (!wxFileExists (mCookiesPath))
        return ;

    wxFile inputFile (mCookiesPath, wxFile::read);

    if (!inputFile.IsOpened ())
        return ;

    wxString content;

    if (!inputFile.ReadAll (&content) || content.empty ())
        return ;

    wxStringTokenizer tokenizer (content, "\r\n");

    while (tokenizer.HasMoreTokens ())
    {
        const wxString line = tokenizer.GetNextToken ();

        if (!line.empty ())
        {
            mCookieList.setCookie (network_manager::Cookie::Parse (
            ToUTF8 (line)
            ));
        }
    }
}

void YandexMetricaUserTracking::storeCookies () const
{
    wxFile outputFile (mCookiesPath, wxFile::write);

    if (!outputFile.IsOpened ())
        return ;

    std::lock_guard<std::mutex> lock (mCookiesMutex);

    for (const network_manager::Cookie& cookie : mCookieList)
    {
        if (!cookie.isSession ())
        {
            const std::string cookieString = cookie.toString (true);
            outputFile.Write (cookieString.data (), cookieString.length ());
            outputFile.Write ("\n");
        }
    }
}

}
}
}