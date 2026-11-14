#! /usr/bin/env bash

# If anything happens
# notify the user that cloud functions may be restricted
# but do not prevent from launching the app
error_handler() {
    local exit_code=$?
    local line_number=$1
    local command="$2"
    echo "Error: Command '${command}' failed with exit code ${exit_code} at line ${line_number}."
    echo "You may not be able to use some cloud functions"
    echo "Please create an issue at https://github.com/audacity/audacity/issues"
    exit 0
}

install_url_handler() {
    trap 'error_handler ${LINENO} "$BASH_COMMAND"' ERR
    # we are deliberately overwriting any existing file
    DESKTOP_FILENAME="audacity-url-handler.desktop"
    SOURCE_FILE="$(dirname "$0")/share/applications/${DESKTOP_FILENAME}"

    XDG_APP_PATH="${HOME}/.local/share/applications"
    DESTINATION="${XDG_APP_PATH}/${DESKTOP_FILENAME}"

    cp "${SOURCE_FILE}" "${DESTINATION}"

    # we need to fix up the path to the AppImage
    sed -i "s|^Exec=.*|Exec=${APPIMAGE} -u %u|" "${DESTINATION}"

    update-desktop-database "${XDG_APP_PATH}"
}

# APPIMAGE is an environment variable set by the runtime
# defining the absolute path to the .AppImage file
if [ -n "${APPIMAGE}" ]; then
    ( install_url_handler )
fi
