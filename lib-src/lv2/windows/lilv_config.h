/* WARNING! All changes made to this file will be lost! */

#ifndef W_LILV_CONFIG_H_WAF
#define W_LILV_CONFIG_H_WAF

#define HAVE_LV2 1
#define HAVE_SERD 1
#define HAVE_SORD 1
#define HAVE_SRATOM 1
#define LILV_NEW_LV2 1
/* #undef HAVE_FLOCK */
#define HAVE_FILENO 1
/* #undef HAVE_CLOCK_GETTIME */
#define LILV_VERSION "0.20.0"
#define LILV_PATH_SEP ";"
#define LILV_DIR_SEP "\\"
#define LILV_DEFAULT_LV2_PATH "%APPDATA%\\LV2;%COMMONPROGRAMFILES%\\LV2"

#include <windows.h>
#undef CreateSymbolicLink
inline BOOLEAN CreateSymbolicLink(LPCSTR lpSymlinkFileName,
                                  LPCSTR lpTargetFileName,
                                  DWORD dwFlags)
{
   typedef BOOLEAN (WINAPI *CSL)(LPCSTR lpSymlinkFileName,
                                 LPCSTR lpTargetFileName,
                                 DWORD dwFlags);
   
   CSL *symlink = (CSL *) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                                         TEXT("CreateSymbolicLinkA"));
   if (symlink)
   {
      return (*symlink)(lpSymlinkFileName, lpTargetFileName, dwFlags);
   }

   return FALSE;
}
#endif /* W_LILV_CONFIG_H_WAF */
