#! /usr/bin/env bash

COLOR_SCHEME="$(dbus-send --session --dest=org.freedesktop.portal.Desktop --type=method_call --print-reply --reply-timeout=1000 /org/freedesktop/portal/desktop org.freedesktop.portal.Settings.Read 'string:org.freedesktop.appearance' 'string:color-scheme' 2> /dev/null | tail -n1 | cut -b35- | cut -d' ' -f2 || printf '')"
if [ -z "$COLOR_SCHEME" ]; then
    COLOR_SCHEME="$(gsettings get org.gnome.desktop.interface color-scheme 2> /dev/null || printf '')"
fi
case "$COLOR_SCHEME" in
    "1"|"'prefer-dark'")  GTK_THEME_VARIANT="-dark";;
    "2"|"'prefer-light'") GTK_THEME_VARIANT="";;
    *)                    GTK_THEME_VARIANT="";;
esac
APPIMAGE_GTK2_THEME="${APPIMAGE_GTK2_THEME:-"Adwaita$GTK_THEME_VARIANT"}" # Allow user to override theme (discouraged)
export GTK2_RC_FILES="${APPDIR}/usr/share/themes/${APPIMAGE_GTK2_THEME}/gtk-2.0/gtkrc"
