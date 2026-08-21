#
# Audacity: A Digital Audio Editor
#

install(DIRECTORY .
    DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}testflowscripts
    FILES_MATCHING PATTERN "*.js"
)
