#
# Audacity: A Digital Audio Editor
#

install (DIRECTORY
    workspaces
    DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}
    PATTERN ".*" EXCLUDE
    )

# Note: Nyquist runtime files (.lsp, .txt, .raw) are handled by au3/nyquist/CMakeLists.txt
# Note: Nyquist plug-in files (.ny) are handled by au3/plug-ins/CMakeLists.txt

subdirs(locale autobotscripts icons)
