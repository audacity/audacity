/*
 * Audacity: A Digital Audio Editor
 */

function doCheckControlIsActive(action) {
    if (api.navigation.activeControl() === "") {
        api.testflow.error("navigation error: no control after call: " + action)
    }
}
// Establish shortcuts for navigation in AU4
module.exports = {
    nextPanel: function () {
        api.navigation.nextPanel()
        doCheckControlIsActive("nextPanel")
    },
    prevPanel: function () {
        api.navigation.prevPanel()
        doCheckControlIsActive("prevPanel")
    },
    right: function () {
        api.navigation.right()
        doCheckControlIsActive("right")
    },
    left: function () {
        api.navigation.left()
        doCheckControlIsActive("left")
    },
    up: function () {
        api.navigation.up()
        doCheckControlIsActive("up")
    },
    down: function () {
        api.navigation.down()
        doCheckControlIsActive("down")
    },
    escape: function () {
        api.navigation.escape()
        doCheckControlIsActive("escape")
    },
    goToControl: function (section, panel, controlNameOrIndex) {
        if (!api.navigation.goToControl(section, panel, controlNameOrIndex)) {
            api.testflow.error("navigation error: not found control: " + controlNameOrIndex)
        }
    },
    trigger: function () {
        if (api.navigation.activeControl() === "") {
            api.testflow.error("navigation error: unable trigger, no active control")
            return
        }
        api.navigation.trigger()
    },
    triggerControl: function (section, panel, controlNameOrIndex) {
        if (!api.navigation.triggerControl(section, panel, controlNameOrIndex)) {
            api.testflow.error("navigation error: not found control: " + controlNameOrIndex)
        }
    },
    assertControlCount: function (sectionName, panelName, count) {
        var controls = api.navigation.controls(sectionName, panelName)
        if (controls.length !== count) {
            api.testflow.error("Control count is " + controls.length + ", expected " + count
                               + ", section: " + sectionName + ", panel: " + panelName)
        }
    },
    // Prefer this over assertControlCount: most toolbar controls are the digit
    // cells of the timecode fields, so the total says nothing about the toolbar
    assertControlsPresent: function (sectionName, panelName, names) {
        var controls = api.navigation.controls(sectionName, panelName)
        var present = []
        for (var i = 0; i < controls.length; i++) {
            present.push(controls[i].name)
        }
        for (var n = 0; n < names.length; n++) {
            if (present.indexOf(names[n]) === -1) {
                api.testflow.error("Control not found: " + names[n]
                                   + ", section: " + sectionName + ", panel: " + panelName
                                   + ", present: " + present.join(", "))
            }
        }
    },
    assertControlsAbsent: function (sectionName, panelName, names) {
        var controls = api.navigation.controls(sectionName, panelName)
        for (var i = 0; i < controls.length; i++) {
            if (names.indexOf(controls[i].name) !== -1) {
                api.testflow.error("Control should not be present: " + controls[i].name
                                   + ", section: " + sectionName + ", panel: " + panelName)
            }
        }
    },
    assertControlsEnabled: function (sectionName, panelName) {
        var controls = api.navigation.controls(sectionName, panelName)
        if (controls[0].enabled === false) {
            api.testflow.error("Control " + sectionName + ", panel: " + panelName + " is disabled.")
        }
    },
    assertControlsDisabled: function (sectionName, panelName) {
        var controls = api.navigation.controls(sectionName, panelName)
        if (controls[0].enabled === true) {
            api.testflow.error("Control " + sectionName + ", panel: " + panelName + " is enabled.")
        }
    },

    activeSection: api.navigation.activeSection,
    activePanel: api.navigation.activePanel,
    activeControl: api.navigation.activeControl,
}
