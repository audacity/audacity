# This macro compares version numbers of the form "x.y.z"
# MACRO_ENSURE_VERSION( FOO_MIN_VERSION FOO_VERSION_FOUND FOO_VERSION_OK)
# will set FOO_VERSIN_OK to true if FOO_VERSION_FOUND >= FOO_MIN_VERSION
# where both have to be in a 3-part-version format, leading and trailing
# text is ok, e.g.
# MACRO_ENSURE_VERSION( "2.5.31" "flex 2.5.4a" VERSION_OK)
# which means 2.5.31 is required and "flex 2.5.4a" is what was found on the system

# Copyright (c) 2006, David Faure, <faure@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

MACRO(MACRO_ENSURE_VERSION requested_version found_version var_too_old)

    # parse the parts of the version string
    STRING(REGEX REPLACE "([0-9]+)\\.[0-9]+\\.[0-9]+" "\\1" req_major_vers "${requested_version}")
    STRING(REGEX REPLACE "[0-9]+\\.([0-9]+)\\.[0-9]+" "\\1" req_minor_vers "${requested_version}")
    STRING(REGEX REPLACE "[0-9]+\\.[0-9]+\\.([0-9]+)" "\\1" req_patch_vers "${requested_version}")

    STRING(REGEX REPLACE "[^0-9]*([0-9]+)\\.[0-9]+\\.[0-9]+.*" "\\1" found_major_vers "${found_version}")
    STRING(REGEX REPLACE "[^0-9]*[0-9]+\\.([0-9]+)\\.[0-9]+.*" "\\1" found_minor_vers "${found_version}")
    STRING(REGEX REPLACE "[^0-9]*[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" found_patch_vers "${found_version}")

    # compute an overall version number which can be compared at once
    MATH(EXPR req_vers_num "${req_major_vers}*10000 + ${req_minor_vers}*100 + ${req_patch_vers}")
    MATH(EXPR found_vers_num "${found_major_vers}*10000 + ${found_minor_vers}*100 + ${found_patch_vers}")

    if (found_vers_num LESS req_vers_num)
        set( ${var_too_old} FALSE )
    else (found_vers_num LESS req_vers_num)
        set( ${var_too_old} TRUE )
    endif (found_vers_num LESS req_vers_num)

ENDMACRO(MACRO_ENSURE_VERSION)


# This macro compares version numbers of the form "x.y"
# MACRO_ENSURE_VERSION( FOO_MIN_VERSION FOO_VERSION_FOUND FOO_VERSION_OK)
# will set FOO_VERSIN_OK to true if FOO_VERSION_FOUND >= FOO_MIN_VERSION
# where both have to be in a 2-part-version format, leading and trailing
# text is ok, e.g.
# MACRO_ENSURE_VERSION( "0.5" "foo 0.6" VERSION_OK)
# which means 0.5 is required and "foo 0.6" is what was found on the system

# Copyright (c) 2006, David Faure, <faure@kde.org>
# Copyright (c) 2007, Pino Toscano, <pino@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

MACRO(MACRO_ENSURE_VERSION2 requested_version found_version var_too_old)

    # parse the parts of the version string
    STRING(REGEX REPLACE "([0-9]+)\\.[0-9]+" "\\1" req_major_vers "${requested_version}")
    STRING(REGEX REPLACE "[0-9]+\\.([0-9]+)" "\\1" req_minor_vers "${requested_version}")

    STRING(REGEX REPLACE "[^0-9]*([0-9]+)\\.[0-9]+.*" "\\1" found_major_vers "${found_version}")
    STRING(REGEX REPLACE "[^0-9]*[0-9]+\\.([0-9]+).*" "\\1" found_minor_vers "${found_version}")

    # compute an overall version number which can be compared at once
    MATH(EXPR req_vers_num "${req_major_vers}*100 + ${req_minor_vers}")
    MATH(EXPR found_vers_num "${found_major_vers}*100 + ${found_minor_vers}")

    if (found_vers_num LESS req_vers_num)
        set( ${var_too_old} FALSE )
    else (found_vers_num LESS req_vers_num)
        set( ${var_too_old} TRUE )
    endif (found_vers_num LESS req_vers_num)

ENDMACRO(MACRO_ENSURE_VERSION2)
