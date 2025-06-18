/*
 * Audacity: A Digital Audio Editor
 */

function doCheckControlIsActive(action)
{
    if (api.navigation.activeControl() === "") {
        api.autobot.error("navigation error: no control after call: " + action)
    }
}

module.exports = {
    nextPanel: function()
    {
        api.navigation.nextPanel()
        doCheckControlIsActive("nextPanel")
    },
    prevPanel: function()
    {
        api.navigation.prevPanel()
        doCheckControlIsActive("prevPanel")
    },
    right: function()
    {
        api.navigation.right()
        doCheckControlIsActive("right")
    },
    left: function()
    {
        api.navigation.left()
        doCheckControlIsActive("left")
    },
    up: function()
    {
        api.navigation.up()
        doCheckControlIsActive("up")
    },
    down: function()
    {
        api.navigation.down()
        doCheckControlIsActive("down")
    },
    escape: function()
    {
        api.navigation.escape()
        doCheckControlIsActive("escape")
    },
    goToControl: function(section, panel, controlNameOrIndex)
    {
        if (!api.navigation.goToControl(section, panel, controlNameOrIndex)) {
            api.autobot.error("navigation error: not found control: " + controlNameOrIndex)
        }
    },
    trigger: function()
    {
        if (api.navigation.activeControl() === "") {
            api.autobot.error("navigation error: unable trigger, no active control")
            return
        }
        api.navigation.trigger()
    },
    triggerControl: function(section, panel, controlNameOrIndex)
    {
        if (!api.navigation.triggerControl(section, panel, controlNameOrIndex)) {
            api.autobot.error("navigation error: not found control: " + controlNameOrIndex)
        }
    },
    activeSection: api.navigation.activeSection,
    activePanel: api.navigation.activePanel,
    activeControl: api.navigation.activeControl,
}
