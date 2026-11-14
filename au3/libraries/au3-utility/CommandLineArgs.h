/**********************************************************************

 Audacity: A Digital Audio Editor

 @file CommandLineArgs.h

 @brief Avoid dependency on argc and argv in wxAppConsoleBase

 Paul Licameli

 **********************************************************************/
#ifdef _WIN32
#   include <cstddef>
#   include <vector>
#   include <string>
#endif

namespace CommandLineArgs {
//! A copy of argc; responsibility of application startup to assign it.
extern UTILITY_API int argc;
//! A copy of argv; responsibility of application startup to assign it.
extern UTILITY_API const char* const* argv;

#ifdef _WIN32
//! Parse process command line
/*!
 Convert wide strings to narrow, assuming all characters are ASCII,
 which is good enough for the sub-processes the application may spawn
 */
struct UTILITY_API MSWParser final {
    int argc{ 0 };
    std::vector<const char*> argv;

    MSWParser();
    ~MSWParser();

private:
    wchar_t** wideArgv{ nullptr };
    std::vector<std::string> narrowArgv;
};
#endif
}
