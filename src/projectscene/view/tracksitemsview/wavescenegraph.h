/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include <QColor>

#include "iwavepainter.h"
#include "au3/iminmaxrmspainter.h"
#include "types/projectscenetypes.h"

class QSGGeometry;
class QSGNode;

namespace au::projectscene {
//! The MinMaxRMS waveform of one clip view as Qt Quick scene graph geometry:
//! prepares it from the clip's column data (GUI thread) and builds the node from it (render thread)
class WaveSceneGraph
{
public:
    struct Input
    {
        trackedit::ClipKey clipKey;
        ClipTime clipTime;
        IWavePainter::Params params;
        WaveMetrics metrics;
        IMinMaxRMSPainter::ClipData clipData;
        double containerExactX = 0.0;
        bool dragGhost = false;
        bool recording = false;
        float cornerRadius = 0.0f;
        bool roundTopCorners = false;
    };

    struct Vertex
    {
        float x;
        float maxY;
        float minY;
        float rmsMaxY;
        float rmsMinY;
        uint8_t r, g, b, a;
        uint8_t rmsR, rmsG, rmsB, rmsA;
    };

    //! What the scene graph node is built from
    struct Frame
    {
        std::vector<Vertex> vertices;
        size_t channelSplitIndex = 0;
        std::vector<float> zeroLineYs;
        float height = 0.0f;
        bool showRMS = false;
        bool hasSelection = false;
        float selectionLeft = 0.0f;
        float selectionRight = 0.0f;
        QColor backgroundColor;
        QColor selectedBackgroundColor;
        QColor zeroLineColor;
    };

    //! Everything the prepared vertex data depends on. When a repaint request
    //! arrives with an equal signature (and the last preparation was complete),
    //! the data walk and vertex rebuild are skipped entirely.
    struct InputSignature
    {
        long trackId = -1;
        long itemId = -1;
        uint64_t dataVersion = 0;
        size_t channelCount = 0;
        double fromTime = 0.0;
        double toTime = 0.0;
        double placeholderToTime = 0.0;
        double zoom = 0.0;
        double height = 0.0;
        double channelHeightRatio = 0.0;
        double selectionStartTime = 0.0;
        double selectionEndTime = 0.0;
        double dbRange = 0.0;
        float displayMin = 0.0f;
        float displayMax = 0.0f;
        bool showRMS = false;
        bool showClipping = false;
        bool isLinear = false;
        float cornerRadius = 0.0f;
        bool roundTopCorners = false;
        QRgb background = 0;
        QRgb selectedBackground = 0;
        QRgb centerLine = 0;
        QRgb samplePen = 0;
        QRgb selectedSamplePen = 0;
        QRgb rmsPen = 0;
        QRgb rmsSelectedPen = 0;
        QRgb clippedPen = 0;

        bool operator==(const InputSignature& o) const
        {
            return trackId == o.trackId && itemId == o.itemId
                   && dataVersion == o.dataVersion && channelCount == o.channelCount
                   && fromTime == o.fromTime && toTime == o.toTime
                   && placeholderToTime == o.placeholderToTime && zoom == o.zoom
                   && height == o.height
                   && channelHeightRatio == o.channelHeightRatio
                   && selectionStartTime == o.selectionStartTime
                   && selectionEndTime == o.selectionEndTime
                   && dbRange == o.dbRange
                   && displayMin == o.displayMin && displayMax == o.displayMax
                   && showRMS == o.showRMS && showClipping == o.showClipping
                   && isLinear == o.isLinear
                   && cornerRadius == o.cornerRadius && roundTopCorners == o.roundTopCorners
                   && background == o.background && selectedBackground == o.selectedBackground
                   && centerLine == o.centerLine && samplePen == o.samplePen
                   && selectedSamplePen == o.selectedSamplePen && rmsPen == o.rmsPen
                   && rmsSelectedPen == o.rmsSelectedPen && clippedPen == o.clippedPen;
        }
    };

    //! Where the prepared columns lie, in pixels from the untrimmed sequence start
    struct Window
    {
        double fromPixelExact = 0.0;
        double fromPixelSnapped = 0.0;
        double toPixelExact = 0.0;
        double toPixelSnapped = 0.0;
        bool hasPlaceholder = false;
        double placeholderFromPixel = 0.0;
        double subpixelResidual = 0.0;
        double contentOffset = 0.0;
    };

    //! Prepares what the next node update draws; returns true when it should be prepared again shortly
    bool prepare(const Input& input, IMinMaxRMSPainter& painter);
    void reset();
    //! Leaves a complete frame for a view recreated for the same clip
    void storeForReuse(const trackedit::ClipKey& clipKey);

    bool active() const;
    bool hasNode() const;
    bool nodeUpToDate(float itemWidth) const;
    //! The selected span as drawn, in item space
    std::optional<std::pair<float, float> > selectionSpan(float itemWidth) const;
    void invalidateNode();

    QSGNode* updatePaintNode(QSGNode* oldNode, float itemWidth);

    static Window computeWindow(const ClipTime& clipTime, const IMinMaxRMSPainter::ClipData& clipData, double zoom, double containerExactX,
                                bool dragGhost, bool recording);
    static InputSignature makeSignature(const trackedit::ClipKey& clipKey, const IMinMaxRMSPainter::ClipData& clipData,
                                        const IWavePainter::Params& params, const WaveMetrics& metrics, const Window& window,
                                        float cornerRadius, bool roundTopCorners);

private:
    struct Rgb {
        uint8_t r, g, b;
    };

    struct ColumnColors {
        Rgb sample;
        Rgb selectedSample;
        Rgb rms;
        Rgb selectedRms;
        Rgb clipping;
    };

    static ColumnColors columnColors(const IWavePainter::Style& style);
    static Vertex toVertex(const WaveColumnVertex& col, float channelTop, float channelBottom, const ColumnColors& colors);
    static void clampToCorners(Vertex& v, float inset, bool roundTop, float viewHeight);

    //! Takes the last frame or one a previous view of the clip left, when prepared from the same inputs
    bool reusePreparedFrame(const trackedit::ClipKey& clipKey, const InputSignature& sig, double contentOffset);
    //! Builds the frame's vertices; returns false while some of the data is still being computed
    bool buildFrame(const Input& input, WaveMetrics waveMetrics, const Window& window, IMinMaxRMSPainter& painter);
    //! Keeps the previous frame if the new one came out empty mid-gesture; returns whether to retry
    bool acceptFrame(Frame previous, bool prevActive, const Input& input, const InputSignature& sig, const Window& window, bool complete);
    void fillSelectionGeometry(QSGGeometry* geometry, float itemWidth) const;

    Frame m_frame;
    bool m_active = false;
    bool m_dirty = true;
    int m_staleRetainCount = 0;
    InputSignature m_lastSignature;
    bool m_lastSignatureValid = false;
    bool m_lastComplete = false;
    double m_contentOffset = 0.0;
    float m_cornerRadius = 0.0f;
    bool m_roundTopCorners = false;

    //! What the current node shows. A preparation that would not change it skips the node update.
    bool m_nodeValid = false;
    double m_appliedContentOffset = 0.0;
    float m_appliedWidth = -1.0f;
};
}
