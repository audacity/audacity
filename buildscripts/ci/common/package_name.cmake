
# Composes the file name of a package from a pattern, for the CI package
# scripts (cmake -P). Nothing in here is specific to an app: the app passes its
# name, its version and its patterns, so this file can move to the framework
# as it is.
#
#   muse_package_name(<out-var>
#       APP <name> VERSION <version> PATTERN <pattern>
#       [NIGHTLY_PATTERN <pattern>] [MODE <build mode>]
#       [OS <os>] [ARCH <arch>] [EXT <ext>] [ARTIFACTS_DIR <dir>])
#
# A pattern is the file name with placeholders:
#   {app} {version} {os} {arch} {ext}   the arguments of the same name
#   {build} {branch} {revision}         build_number.env, build_branch.env and
#                                       build_revision.env in <ARTIFACTS_DIR>/env
#                                       (ARTIFACTS_DIR defaults to build.artifacts)
# Every placeholder used must have a value. NIGHTLY_PATTERN, when given, is
# used instead of PATTERN for a nightly build; MODE defaults to build_mode.env.
#
#   muse_package_name(NAME APP audacity VERSION 4.0.1 OS linux ARCH x86_64 EXT AppImage
#       PATTERN "{app}-{os}-{version}-{arch}.{ext}")
#     -> audacity-linux-4.0.1-x86_64.AppImage
#   muse_package_name(NAME APP MuseScore-Studio VERSION 4.7.5 ARCH x86_64 EXT msi
#       PATTERN "{app}-{version}.{build}-{arch}.{ext}")
#     -> MuseScore-Studio-4.7.5.260831071-x86_64.msi

function(muse_package_name OUT_VAR)
    set(_options)
    set(_one_value APP VERSION PATTERN NIGHTLY_PATTERN MODE OS ARCH EXT ARTIFACTS_DIR)
    set(_multi_value)
    cmake_parse_arguments(PARSE_ARGV 1 ARG "${_options}" "${_one_value}" "${_multi_value}")

    if (ARG_UNPARSED_ARGUMENTS)
        message(FATAL_ERROR "muse_package_name: unknown arguments: ${ARG_UNPARSED_ARGUMENTS}")
    endif()
    foreach(_required APP VERSION PATTERN)
        if ("${ARG_${_required}}" STREQUAL "")
            message(FATAL_ERROR "muse_package_name: ${_required} not set")
        endif()
    endforeach()

    if ("${ARG_ARTIFACTS_DIR}" STREQUAL "")
        set(ARG_ARTIFACTS_DIR "build.artifacts")
    endif()
    set(_env_dir "${ARG_ARTIFACTS_DIR}/env")

    # The pattern: the nightly one for a nightly build
    set(_pattern "${ARG_PATTERN}")
    if (NOT "${ARG_NIGHTLY_PATTERN}" STREQUAL "")
        set(_mode "${ARG_MODE}")
        if ("${_mode}" STREQUAL "" AND EXISTS "${_env_dir}/build_mode.env")
            file(STRINGS "${_env_dir}/build_mode.env" _mode)
        endif()
        if ("${_mode}" STREQUAL "")
            message(FATAL_ERROR "muse_package_name: build mode not set (MODE or ${_env_dir}/build_mode.env)")
        endif()
        if ("${_mode}" STREQUAL "nightly")
            set(_pattern "${ARG_NIGHTLY_PATTERN}")
        endif()
    endif()

    # The placeholders: arguments, or env files written at the configure step
    set(_value_app "${ARG_APP}")
    set(_value_version "${ARG_VERSION}")
    set(_value_os "${ARG_OS}")
    set(_value_arch "${ARG_ARCH}")
    set(_value_ext "${ARG_EXT}")
    set(_env_file_build build_number.env)
    set(_env_file_branch build_branch.env)
    set(_env_file_revision build_revision.env)

    set(_name "${_pattern}")
    foreach(_field app version os arch ext build branch revision)
        string(FIND "${_pattern}" "{${_field}}" _pos)
        if (_pos EQUAL -1)
            continue()
        endif()
        if (DEFINED _env_file_${_field})
            set(_env_file "${_env_dir}/${_env_file_${_field}}")
            if (NOT EXISTS "${_env_file}")
                message(FATAL_ERROR "muse_package_name: {${_field}} needs ${_env_file}, which does not exist")
            endif()
            file(STRINGS "${_env_file}" _value_${_field})
        endif()
        if ("${_value_${_field}}" STREQUAL "")
            message(FATAL_ERROR "muse_package_name: no value for {${_field}} in pattern ${_pattern}")
        endif()
        string(REPLACE "{${_field}}" "${_value_${_field}}" _name "${_name}")
    endforeach()

    if (_name MATCHES "[{}]")
        message(FATAL_ERROR "muse_package_name: unknown placeholder in ${_name}")
    endif()

    set(${OUT_VAR} "${_name}" PARENT_SCOPE)
endfunction()
