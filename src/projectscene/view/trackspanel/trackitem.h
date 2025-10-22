/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QMetaType>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/iselectioncontroller.h"

#include "ui/view/iconcodes.h"

#include "trackedit/dom/track.h"
#include "trackedit/trackedittypes.h"

Q_DECLARE_METATYPE(au::trackedit::TrackType)

namespace au::projectscene {
class TrackItemType
{
    Q_GADGET

public:
    enum Type: int {
        Undefined = (int)au::trackedit::TrackType::Undefined,
        Mono = (int)au::trackedit::TrackType::Mono,
        Stereo = (int)au::trackedit::TrackType::Stereo,
        Label = (int)au::trackedit::TrackType::Label
    };

    Q_ENUM(Type)
};

class TrackItem : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QVariant trackId READ trackId_property CONSTANT)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged)
    Q_PROPERTY(int icon READ icon CONSTANT)
    Q_PROPERTY(au::trackedit::TrackType trackType READ trackType NOTIFY trackTypeChanged)

    Q_PROPERTY(bool isSelected READ isSelected NOTIFY isSelectedChanged)
    Q_PROPERTY(bool isFocused READ isFocused NOTIFY isFocusedChanged)

    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<trackedit::ISelectionController> selectionController;

public:
    explicit TrackItem(QObject* parent = nullptr);

    ~TrackItem() override;

    virtual Q_INVOKABLE void init(const trackedit::Track& track);

    trackedit::TrackId trackId() const;
    QVariant trackId_property() const;
    QString title() const;
    int icon() const;
    au::trackedit::TrackType trackType() const;

    bool isSelected() const;
    void setIsSelected(bool selected);

    bool isFocused() const;
    void setIsFocused(bool focused);

protected:
    virtual bool isAudible() const = 0;

public slots:
    void setTitle(QString title);

signals:
    void titleChanged(QString title);

    void trackTypeChanged();

    void isSelectedChanged();
    void isFocusedChanged();

protected:
    trackedit::TrackId m_trackId = -1;
    trackedit::TrackType m_trackType = trackedit::TrackType::Undefined;
    QString m_title;
    muse::ui::IconCode::Code m_icon = muse::ui::IconCode::Code::NONE;

    bool m_isSelected = false;
    bool m_isFocused = false;
};
}
