# SPDX-FileCopyrightText: 2015 Alex Merry <alex.merry@kde.org>
#
# SPDX-License-Identifier: BSD-3-Clause

#[=======================================================================[.rst:
ECMUninstallTarget
------------------

Add an ``uninstall`` target.

By including this module, an ``uninstall`` target will be added to your CMake
project. This will remove all files installed (or updated) by a previous
invocation of the ``install`` target. It will not remove files created or
modified by an ``install(SCRIPT)`` or ``install(CODE)`` command; you should
create a custom uninstallation target for these and use ``add_dependency`` to
make the ``uninstall`` target depend on it:

.. code-block:: cmake

  include(ECMUninstallTarget)
  install(SCRIPT install-foo.cmake)
  add_custom_target(uninstall_foo COMMAND ${CMAKE_COMMAND} -P uninstall-foo.cmake)
  add_dependency(uninstall uninstall_foo)

The target will fail if the ``install`` target has not yet been run (so it is
not possible to run CMake on the project and then immediately run the
``uninstall`` target).

.. warning::

  CMake deliberately does not provide an ``uninstall`` target by default on
  the basis that such a target has the potential to remove important files
  from a user's computer. Use with caution.

Since 1.7.0.
#]=======================================================================]

if (NOT TARGET uninstall)
    configure_file(
        "${CMAKE_CURRENT_LIST_DIR}/ecm_uninstall.cmake.in"
        "${CMAKE_BINARY_DIR}/ecm_uninstall.cmake"
        IMMEDIATE
        @ONLY
    )

    add_custom_target(uninstall
        COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_BINARY_DIR}/ecm_uninstall.cmake"
        WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
    )
endif()
