#
# Audacity: A Digital Audio Editor
#

# Scripts
install(DIRECTORY .
    DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}testflowscripts
    FILES_MATCHING PATTERN "*.js"
)

# Steps
install(DIRECTORY steps/
    DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}testflowscripts/steps
    FILES_MATCHING PATTERN "*.js"
)
