/***************************************************************************
    copyright            : (C) 2011 by Lukas Lalinsky
    email                : lalinsky@gmail.com
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License version   *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#include "tiostream.h"

using namespace TagLib;

#ifdef _WIN32

# include "tstring.h"
# include "tdebug.h"
# include <windows.h>

namespace 
{
  // Check if the running system has CreateFileW() function.
  // Windows9x systems don't have CreateFileW() or can't accept Unicode file names. 

  bool supportsUnicode()
  {
    const FARPROC p = GetProcAddress(GetModuleHandleA("kernel32"), "CreateFileW");
    return (p != NULL);
  }

  // Indicates whether the system supports Unicode file names.
  
  const bool SystemSupportsUnicode = supportsUnicode(); 

  // Converts a UTF-16 string into a local encoding.
  // This function should only be used in Windows9x systems which don't support 
  // Unicode file names.

  std::string unicodeToAnsi(const wchar_t *wstr)
  {
    if(SystemSupportsUnicode) {
      debug("unicodeToAnsi() - Should not be used on WinNT systems.");
    }

    const int len = WideCharToMultiByte(CP_ACP, 0, wstr, -1, NULL, 0, NULL, NULL);
    if(len == 0)
      return std::string();

    std::string str(len, '\0');
    WideCharToMultiByte(CP_ACP, 0, wstr, -1, &str[0], len, NULL, NULL);

    return str;
  }
}

// If WinNT, stores a Unicode string into m_wname directly.
// If Win9x, converts and stores it into m_name to avoid calling Unicode version functions.

FileName::FileName(const wchar_t *name) 
  : m_name (SystemSupportsUnicode ? "" : unicodeToAnsi(name))
  , m_wname(SystemSupportsUnicode ? name : L"")
{
}

FileName::FileName(const char *name) 
  : m_name(name) 
{
}

FileName::FileName(const FileName &name) 
  : m_name (name.m_name) 
  , m_wname(name.m_wname)
{
}

FileName::operator const wchar_t *() const 
{ 
  return m_wname.c_str(); 
}

FileName::operator const char *() const 
{ 
  return m_name.c_str(); 
}

const std::wstring &FileName::wstr() const 
{ 
  return m_wname; 
}

const std::string &FileName::str() const 
{ 
  return m_name; 
}  

String FileName::toString() const
{
  if(!m_wname.empty()) {
    return String(m_wname);
  } 
  else if(!m_name.empty()) {
    const int len = MultiByteToWideChar(CP_ACP, 0, m_name.c_str(), -1, NULL, 0);
    if(len == 0)
      return String::null;

    std::vector<wchar_t> buf(len);
    MultiByteToWideChar(CP_ACP, 0, m_name.c_str(), -1, &buf[0], len);

    return String(&buf[0]);
  }
  else {
    return String::null;
  }
}


#endif  // _WIN32

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

IOStream::IOStream()
{
}

IOStream::~IOStream()
{
}

void IOStream::clear()
{
}

