/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file CommandLineArgs.h

 @brief Avoid dependency on argc and argv in wxAppConsoleBase

 Paul Licameli
 
 **********************************************************************/

namespace CommandLineArgs {
//! A copy of argc; responsibility of application startup to assign it.
extern UTILITY_API int argc;
//! A copy of argv; responsibility of application startup to assign it.
extern UTILITY_API const char *const *argv;
}
