# qt_generate_deploy_qml_app_script unfortunately does not expose the NO_OVERWRITE option from qt6_deploy_runtime_dependencies.
# This is a pain because it slows down dramatically the execution of the install target (which typically gets executed before you debug the app...).
# Workaround: we modify the generated script to add this option.

file(READ "${SCRIPT_PATH}" content)
string(REPLACE "qt6_deploy_runtime_dependencies(" "qt6_deploy_runtime_dependencies(NO_OVERWRITE " content "${content}")
file(WRITE "${SCRIPT_PATH}" "${content}")
