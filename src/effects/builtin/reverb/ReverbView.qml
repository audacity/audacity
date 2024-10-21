import QtQuick 2.15

import Audacity.Effects

import "../common"

EffectBase {

    property string title: reverb.title
    property alias instanceId: reverb.instanceId

    width: 300
    height: 200

    ReverbViewModel {
        id: reverb
    }


}
