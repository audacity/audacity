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

#ifndef TAGLIB_DEBUGLISTENER_H
#define TAGLIB_DEBUGLISTENER_H

#include "taglib_export.h"
#include "tstring.h"

namespace TagLib 
{
  //! An abstraction for the listener to the debug messages.

  /*!
   * This class enables you to handle the debug messages in your preferred 
   * way by subclassing this class, reimplementing printMessage() and setting 
   * your reimplementation as the default with setDebugListener().
   *
   * \see setDebugListener()
   */  
  class TAGLIB_EXPORT DebugListener
  {
  public:
    DebugListener();
    virtual ~DebugListener();

    /*!
     * When overridden in a derived class, redirects \a msg to your preferred
     * channel such as stderr, Windows debugger or so forth.
     */
    virtual void printMessage(const String &msg) = 0;

  private:
    // Noncopyable
    DebugListener(const DebugListener &);
    DebugListener &operator=(const DebugListener &);
  };

  /*!
   * Sets the listener that decides how the debug messages are redirected.
   * If the parameter \a listener is null, the previous listener is released 
   * and default stderr listener is restored.   
   *
   * \note The caller is responsible for deleting the previous listener
   * as needed after it is released.
   *
   * \see DebugListener
   */
  TAGLIB_EXPORT void setDebugListener(DebugListener *listener);
}

#endif
