#include "pastebehaviorpanelmodel.h"
#include "global/translation.h"

namespace au::trackedit {
PasteBehaviorPanelModel::PasteBehaviorPanelModel(QObject* parent)
    : QObject(parent)
{
}

void PasteBehaviorPanelModel::init()
{
    uiConfiguration()->currentThemeChanged().onNotify(this, [this] {
        emit uiThemeChanged();
    });
}

int PasteBehaviorPanelModel::pasteBehavior() const
{
    return static_cast<int>(m_pasteBehavior);
}

void PasteBehaviorPanelModel::setPasteBehavior(int value)
{
    const auto newBehavior = static_cast<PasteBehavior>(value);
    if (m_pasteBehavior == newBehavior) {
        return;
    }
    m_pasteBehavior = newBehavior;
    emit pasteBehaviorChanged();
    emit userMustChoosePasteInsertBehaviorChanged();
}

bool PasteBehaviorPanelModel::addBorderToClipImageButtons() const
{
    return uiConfiguration()->isDarkMode();
}

int PasteBehaviorPanelModel::pasteInsertBehavior() const
{
    return static_cast<int>(m_pasteInsertBehavior);
}

void PasteBehaviorPanelModel::setPasteInsertBehavior(int value)
{
    const auto newBehavior = static_cast<PasteInsertBehavior>(value);
    if (m_pasteInsertBehavior == newBehavior) {
        return;
    }
    m_pasteInsertBehavior = newBehavior;
    emit pasteInsertBehaviorChanged();
}

bool PasteBehaviorPanelModel::userMustChoosePasteInsertBehavior() const
{
    return m_pasteBehavior == PasteBehavior::PasteInsert;
}

QVariantList PasteBehaviorPanelModel::pasteInsertBehaviors() const
{
    QVariantList behaviors;
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences", "Pasting audio pushes other clips on the same track") },
                         { "value", static_cast<int>(PasteInsertBehavior::PasteInsert) } });
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences", "Pasting audio pushes all clips on all tracks") },
                         { "value", static_cast<int>(PasteInsertBehavior::PasteInsertRipple) } });
    return behaviors;
}

QVariantList PasteBehaviorPanelModel::pasteBehaviors() const
{
    const bool dark = uiConfiguration()->isDarkMode();
    const auto pasteOverlapImage = dark ? "qrc:/resources/LightMode_PasteOverlap.gif" : "qrc:/resources/LightMode_PasteOverlap.gif";
    const auto pasteInsertImage = dark ? "qrc:/resources/LightMode_PasteInsert.gif" : "qrc:/resources/LightMode_PasteInsert.gif";

    QVariantList behaviors;
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences",
                                                        "Paste overlaps other clips") }, { "imageSource", pasteOverlapImage },
                         { "value", static_cast<int>(PasteBehavior::PasteOverlap) } });
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences",
                                                        "Paste pushes other clips") }, { "imageSource", pasteInsertImage },
                         { "value", static_cast<int>(PasteBehavior::PasteInsert) } });
    return behaviors;
}
}
