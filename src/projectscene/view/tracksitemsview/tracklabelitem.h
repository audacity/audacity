/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "trackedit/dom/label.h"

#include "viewtrackitem.h"

namespace au::projectscene {
class TrackLabelItem : public ViewTrackItem
{
    Q_OBJECT

    Q_PROPERTY(int level READ level WRITE setLevel NOTIFY levelChanged FINAL)

    Q_PROPERTY(int visualWidth READ visualWidth WRITE setVisualWidth NOTIFY visualWidthChanged FINAL)
    Q_PROPERTY(int visualHeight READ visualHeight WRITE setVisualHeight NOTIFY visualHeightChanged FINAL)

    Q_PROPERTY(bool isEditing READ isEditing WRITE setIsEditing NOTIFY isEditingChanged FINAL)

    Q_PROPERTY(bool isLeftLinked READ isLeftLinked WRITE setIsLeftLinked NOTIFY isLeftLinkedChanged FINAL)
    Q_PROPERTY(bool isRightLinked READ isRightLinked WRITE setIsRightLinked NOTIFY isRightLinkedChanged FINAL)
    Q_PROPERTY(bool isLinkedActive READ isLinkedActive WRITE setIsLinkedActive NOTIFY isLinkedActiveChanged FINAL)

    Q_PROPERTY(bool isPoint READ isPoint NOTIFY timeChanged FINAL)

public:
    explicit TrackLabelItem(QObject* parent);

    void setLabel(const trackedit::Label& label);

    int level() const;
    void setLevel(int level);

    int visualWidth() const;
    void setVisualWidth(int width);

    int visualHeight() const;
    void setVisualHeight(int height);

    bool isEditing() const;
    void setIsEditing(bool editing);

    bool isLeftLinked() const;
    void setIsLeftLinked(bool linked);

    bool isRightLinked() const;
    void setIsRightLinked(bool linked);

    bool isLinkedActive() const;
    void setIsLinkedActive(bool active);

    bool isPoint() const;

signals:
    void levelChanged();
    void visualWidthChanged();
    void visualHeightChanged();

    void isEditingChanged();

    void isLeftLinkedChanged();
    void isRightLinkedChanged();
    void isLinkedActiveChanged();

private:
    int m_level = 0;
    int m_visualWidth = 0;
    int m_visualHeight = 0;

    bool m_isEditing = false;
    bool m_isLeftLinked = false;
    bool m_isRightLinked = false;
    bool m_isLinkedActive = false;
};
}
