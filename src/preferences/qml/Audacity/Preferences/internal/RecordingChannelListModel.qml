/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

ListModel {
    property var groups: []

    onGroupsChanged: {
        // Update existing rows instead of replacing the view's model on each toggle.
        // This preserves its delegates, scroll position, and keyboard focus.
        if (count > groups.length) {
            remove(groups.length, count - groups.length)
        }
        for (let i = 0; i < groups.length; ++i) {
            if (i < count) {
                set(i, groups[i])
            } else {
                append(groups[i])
            }
        }
    }
}
