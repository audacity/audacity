
set(MUSE_APP_NAME_HUMAN_READABLE "Audacity")
set(MUSE_APP_NAME_MACHINE_READABLE "Audacity")

set(MUSE_APP_NAME_HUMAN_READABLE_COMPAT "Audacity")
set(MUSE_APP_NAME_MACHINE_READABLE_COMPAT "Audacity")

set(MUSE_APP_VERSION_MAJOR "4")
set(MUSE_APP_VERSION_MINOR "0")
set(MUSE_APP_VERSION_PATCH "0")
set(MUSE_APP_VERSION_MAJ_MIN "${MUSE_APP_VERSION_MAJOR}.${MUSE_APP_VERSION_MINOR}")
set(MUSE_APP_VERSION "${MUSE_APP_VERSION_MAJ_MIN}.${MUSE_APP_VERSION_PATCH}")

set(MUSE_APP_VERSION_LABEL "beta.4")

# Must differ from Audacity 3's "org.audacityteam.audacity" by more than case: LaunchServices compares
# bundle identifiers case-insensitively and merges the two apps' records (icons, localizations).
# Kept equal to the QSettings/CFPreferences domain derived in src/app/main.cpp
# (organizationDomain "audacityteam.org" + applicationName "Audacity4"), so AppKit and Qt share one domain.
set(MUSE_APP_GUI_IDENTIFIER org.audacityteam.${MUSE_APP_NAME_MACHINE_READABLE_COMPAT}${MUSE_APP_VERSION_MAJOR})

set(MUSE_APP_TITLE "${MUSE_APP_NAME_HUMAN_READABLE}")
set(MUSE_APP_NAME "${MUSE_APP_NAME_MACHINE_READABLE_COMPAT}")
set(MUSE_APP_TITLE_VERSION "${MUSE_APP_TITLE} ${MUSE_APP_VERSION_MAJOR}")
set(MUSE_APP_NAME_VERSION "${MUSE_APP_NAME} ${MUSE_APP_VERSION_MAJOR}")

set(MUSE_APP_UNSTABLE ON)
set(MUSE_APP_IS_PRERELEASE ON)

# MUSE_APP_RELEASE_CHANNEL is assigned per-mode in SetupConfigure.cmake based on AU4_BUILD_MODE.

message(STATUS "MUSE_APP_VERSION ${MUSE_APP_VERSION}")
