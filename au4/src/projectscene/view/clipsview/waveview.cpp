#include "waveview.h"

#include <QSGGeometryNode>
#include <QSGFlatColorMaterial>

#include "processing/dom/wave.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::processing;

WaveView::WaveView(QQuickItem* parent)
    : QQuickItem(parent)
{
    setFlag(ItemHasContents, true);

    setClip(true);
}

QSGNode* WaveView::updatePaintNode(QSGNode* oldNode, UpdatePaintNodeData* updatePaintNodeData)
{
    UNUSED(updatePaintNodeData);

    Wave wave = m_source.wave();
    size_t count = wave.size();

    QSGGeometryNode* node = nullptr;
    QSGGeometry* geometry = nullptr;

    if (!oldNode) {
        node = new QSGGeometryNode;
        geometry = new QSGGeometry(QSGGeometry::defaultAttributes_Point2D(), int(count));
        geometry->setLineWidth(1);
        geometry->setDrawingMode(QSGGeometry::DrawLineStrip);
        node->setGeometry(geometry);
        node->setFlag(QSGNode::OwnsGeometry);

        QSGFlatColorMaterial* material = new QSGFlatColorMaterial;
        material->setColor(QColor("#707DE5"));
        node->setMaterial(material);
        node->setFlag(QSGNode::OwnsMaterial);
    } else {
        node = static_cast<QSGGeometryNode*>(oldNode);
        geometry = node->geometry();
        geometry->allocate(int(count));
    }

    QSGGeometry::Point2D* vertices = geometry->vertexDataAsPoint2D();

    QSizeF sz = this->size();
    float scaleY = sz.height() / (32767 * 2);
    for (size_t i = 0; i < count; ++i) {
        int16_t v = wave[i];
        float x = i * 0.5;
        float y = (v * scaleY + sz.height() / 2);

        vertices[i].set(x, y);
    }
    node->markDirty(QSGNode::DirtyGeometry);

    return node;
}

WaveSource WaveView::source() const
{
    return m_source;
}

void WaveView::setSource(const WaveSource& newSource)
{
    m_source = newSource;
    emit sourceChanged();
}
