#
# Audacity: A Digital Audio Editor
#

# Scripts
install(FILES
      TC1.1_BasicTest.js
      DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}autobotscripts
      )

# Steps
install(FILES
      steps/Navigation.js
      steps/Home.js
      DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}autobotscripts/steps
      )
