#
# Audacity: A Digital Audio Editor
#

install (DIRECTORY
    workspaces
    DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}
    PATTERN ".*" EXCLUDE
    )

subdirs(locale autobotscripts)
