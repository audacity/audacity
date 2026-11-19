/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include <functional>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/itrackeditinteraction.h"

namespace au::projectscene {
//! Edits the fades of the selected clips, previewing changes live until applied or canceled.
//! A property that differs between the clips is "mixed": its length is negative, its type -1.
class ClipFadesEditModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory{ this };

    Q_PROPERTY(double fadeInLength READ fadeInLength WRITE setFadeInLength NOTIFY fadesChanged FINAL)
    Q_PROPERTY(int fadeInType READ fadeInType WRITE setFadeInType NOTIFY fadesChanged FINAL)
    Q_PROPERTY(double fadeOutLength READ fadeOutLength WRITE setFadeOutLength NOTIFY fadesChanged FINAL)
    Q_PROPERTY(int fadeOutType READ fadeOutType WRITE setFadeOutType NOTIFY fadesChanged FINAL)
    Q_PROPERTY(double maxFadeLength READ maxFadeLength NOTIFY fadesChanged FINAL)
    Q_PROPERTY(QString clipTitle READ clipTitle NOTIFY fadesChanged FINAL)
    Q_PROPERTY(int clipCount READ clipCount NOTIFY fadesChanged FINAL)

public:
    ClipFadesEditModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void apply();
    Q_INVOKABLE void cancel();

    double fadeInLength() const;
    void setFadeInLength(double length);

    int fadeInType() const;
    void setFadeInType(int type);

    double fadeOutLength() const;
    void setFadeOutLength(double length);

    int fadeOutType() const;
    void setFadeOutType(int type);

    double maxFadeLength() const;

    //! Empty if several clips are edited
    QString clipTitle() const;
    int clipCount() const;

signals:
    void fadesChanged();

private:
    trackedit::Clips clips() const;
    void change(const std::function<void(const trackedit::ClipKey&)>& changeClip);
    void endEdit();

    trackedit::ClipKeyList m_clipKeys;
    bool m_editing = false;
    bool m_changed = false;
};
}
