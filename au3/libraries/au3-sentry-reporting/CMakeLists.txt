#[[
A library, that allows sending error reports to a Sentry server
using Exception and Message interfaces.
]]

set( TARGET au3-sentry-reporting )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

if(${_OPT}has_sentry_reporting)
    set( SOURCES
        SentryHelper.h

        AnonymizedMessage.h
        AnonymizedMessage.cpp

        SentryReport.h
        SentryReport.cpp

        SentryRequestBuilder.h
        SentryRequestBuilder.cpp
    )


    set ( LIBRARIES PRIVATE
        au3-network-manager # Required for the networking
        au3-string-utils # ToUtf8
        au3-uuid # UUIDs are required as an event identifier.
        rapidjson::rapidjson # Protocol is JSON based
        wxBase # Required to retrieve the OS information
    )

    set ( DEFINES
        INTERFACE
            HAS_SENTRY_REPORTING=1
        PRIVATE
            # The variables below will be used to construct Sentry URL:
            # https://${SENTRY_DSN_KEY}@${SENTRY_HOST}/api/${SENTRY_PROJECT}/store
            SENTRY_DSN_KEY="${SENTRY_DSN_KEY}"
            SENTRY_HOST="${SENTRY_HOST}"
            SENTRY_PROJECT="${SENTRY_PROJECT}"
    )

    audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
else()
    audacity_header_only_library( ${TARGET} "SentryHelper.h" "" "" "" )
endif()
