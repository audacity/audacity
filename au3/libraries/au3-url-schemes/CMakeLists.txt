#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
Library, responsible for handling custom URL scemas.
]]

set( TARGET au3-url-schemes )

set( SOURCES
   URLSchemesRegistry.cpp
   URLSchemesRegistry.h

   $<$<PLATFORM_ID:Windows>:win/SchemeRegistrar.cpp>
   $<$<NOT:$<PLATFORM_ID:Windows>>:SchemeRegistrar.cpp>
)

set( LIBRARIES
   PRIVATE
   au3-string-utils-interface
   au3-utility-interface
   wxBase
)

set ( DEFINES INTERFACE -DHAS_CUSTOM_URL_HANDLING )

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "${DEFINES}" ""
)
