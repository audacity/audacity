#include <gtest/gtest.h>
#include <thread>

#include <wx/app.h>
#include <wx/init.h>

#include "au3-import-export/ExportUtils.h"
#include "au3-cloud-audiocom/OAuthService.h"
#include "au3-cloud-audiocom/UserService.h"
#include "au3-preferences/Prefs.h"
#include "au3wrap/internal/au3commonsettings.h"

namespace au::au3wrap {
// Simple wx initializer for tests
class WxTestInitializer
{
public:
    WxTestInitializer()
    {
        // Initialize wxWidgets for console mode
        if (!wxInitialize()) {
            std::cerr << "Failed to initialize wxWidgets" << std::endl;
        }
    }

    ~WxTestInitializer()
    {
        wxUninitialize();
    }
};

class CloudTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        static WxTestInitializer wxInit;

        if (!gPrefs) {
            InitPreferences(std::make_unique<au::au3::Au3CommonSettings>());
        }
    }

    void TearDown() override
    {
    }
};

TEST_F(CloudTests, LoginWithPassword)
{
    std::cout << "\n*** TESTING LOGIN WITH PASSWORD ***\n" << std::endl;

    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    auto& userService = audacity::cloud::audiocom::GetUserService();

    std::cout << "Initial State:" << std::endl;
    std::cout << "  Has Access Token: " << oauthService.HasAccessToken() << std::endl;
    std::cout << "  Has Refresh Token: " << oauthService.HasRefreshToken() << std::endl;

    std::string email = "teste@example1.com";
    std::string password = "your_password";

    bool loginCompleted = false;
    bool loginSuccess = false;

    oauthService.Authorize(
        email, password,
        // Success callback
        [&](std::string_view token) {
        std::cout << "\n✓ Login Successful!" << std::endl;
        std::cout << "  Token received: " << token.substr(0, 20) << "..." << std::endl;
        loginSuccess = true;
        loginCompleted = true;

        // Request user data after successful login
        userService.UpdateUserData();
    },
        // Failure callback
        [&](unsigned statusCode, std::string_view error) {
        std::cout << "\n✗ Login Failed!" << std::endl;
        std::cout << "  Status Code: " << statusCode << std::endl;
        std::cout << "  Error: " << error << std::endl;
        loginSuccess = false;
        loginCompleted = true;
    },
        AudiocomTrace::ignore
        );

    int timeout = 30; // 30 seconds
    while (!loginCompleted && timeout > 0) {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        timeout--;
    }

    if (!loginCompleted) {
        std::cout << "\n✗ Login timed out!" << std::endl;
        return;
    }

    if (loginSuccess) {
        std::cout << "\nPost-Login State:" << std::endl;
        std::cout << "  Has Access Token: " << oauthService.HasAccessToken() << std::endl;
        std::cout << "  Has Refresh Token: " << oauthService.HasRefreshToken() << std::endl;

        // Wait a bit for user data to be fetched
        std::this_thread::sleep_for(std::chrono::seconds(2));
    }
}
}
