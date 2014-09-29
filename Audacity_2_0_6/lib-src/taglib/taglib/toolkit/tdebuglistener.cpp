/***************************************************************************
    copyright            : (C) 2013 by Tsuda Kageyu
    email                : tsuda.kageyu@gmail.com
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

#include "tdebuglistener.h"

#include <iostream>
#include <bitset>

#ifdef _WIN32
# include <windows.h>
#endif

using namespace TagLib;

namespace
{
  class DefaultListener : public DebugListener
  {
  public:
    virtual void printMessage(const String &msg)
    {
#ifdef _WIN32

      const wstring wstr = msg.toWString();
      const int len = WideCharToMultiByte(CP_ACP, 0, wstr.c_str(), -1, NULL, 0, NULL, NULL);
      if(len != 0) {
        std::vector<char> buf(len);
        WideCharToMultiByte(CP_ACP, 0, wstr.c_str(), -1, &buf[0], len, NULL, NULL);

        std::cerr << std::string(&buf[0]);
      }

#else

      std::cerr << msg;

#endif 
    }
  };

  DefaultListener defaultListener;
}

namespace TagLib
{
  DebugListener *debugListener = &defaultListener;

  DebugListener::DebugListener()
  {
  }

  DebugListener::~DebugListener()
  {
  }

  void setDebugListener(DebugListener *listener)
  {
    if(listener)
      debugListener = listener;
    else
      debugListener = &defaultListener;
  }
}
