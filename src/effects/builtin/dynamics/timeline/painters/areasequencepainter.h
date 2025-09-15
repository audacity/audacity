/*
 * Audacity: A Digital Audio Editor
 */
#include "abstractsequencepainter.h"

#include <vector>

namespace au::effects {
class AreaSequencePainter : public AbstractSequencePainter
{
public:
    AreaSequencePainter(const std::atomic<double>& viewport, int maxNumSamples);

    void append(std::vector<SequenceSample> samples) override;

private:
    int numSamplesToDiscard() const;

    std::vector<QSGGeometry::Point2D> m_buffer;
};
} // namespace au::effects
