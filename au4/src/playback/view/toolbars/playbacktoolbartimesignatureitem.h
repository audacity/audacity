/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::playback {
class PlaybackToolBarTimeSignatureItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(int upper READ upper WRITE setUpper NOTIFY upperChanged FINAL)
    Q_PROPERTY(int lower READ lower WRITE setLower NOTIFY lowerChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;

public:
    explicit PlaybackToolBarTimeSignatureItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                              QObject* parent = nullptr);

    int upper() const;
    void setUpper(int newUpper);

    int lower() const;
    void setLower(int newLower);

signals:
    void upperChanged();
    void lowerChanged();

private:
    void onProjectChanged();

    void updateValues();

    int m_upper = 0;
    int m_lower = 0;
};
}
