// Standalone validator for the seek -> flush -> decode-forward -> convert path
// that FFmpegSoftwareBackend will implement. Compiled against system FFmpeg so
// the algorithm can be checked before any of the Qt or au3 wiring exists.
//
// Self-checking: the M0 fixture lights a flash patch on exactly one frame per
// second, so "did the seek land on the frame it claimed" is a measurement, not
// an eyeball. Exit code is non-zero if any target frame is wrong.
//
//  g++ -O2 -o videoprobe videoprobe.cpp \
//      $(pkg-config --cflags --libs libavformat libavcodec libavutil libswscale)

extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/imgutils.h>
#include <libswscale/swscale.h>
}

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

// ---------------------------------------------------------------------------
// Colour conversion. This is the logic that becomes src/video/internal/ffmpeg/
// pixelconvert.cpp, kept deliberately in integer arithmetic and fused with the
// downscale so a 4K source is never converted at full size just to be shown in
// a 640x360 panel.
// ---------------------------------------------------------------------------

struct YuvCoeffs
{
    int yOffset;   // subtracted from Y before scaling
    int yGain;     // 8.8 fixed point
    int rCr, gCb, gCr, bCb;
    const char* name;
};

// 8.8 fixed point. Limited range maps 16..235 -> 0..255, full range is 1:1.
static const YuvCoeffs kBt601Limited = { 16, 298, 409, -100, -208, 516, "BT.601 limited" };
static const YuvCoeffs kBt709Limited = { 16, 298, 459,  -55, -136, 541, "BT.709 limited" };
static const YuvCoeffs kBt601Full    = {  0, 256, 359,  -88, -183, 454, "BT.601 full" };
static const YuvCoeffs kBt709Full    = {  0, 256, 403,  -48, -120, 475, "BT.709 full" };

static const YuvCoeffs& pickCoeffs(AVColorSpace space, AVColorRange range, int height)
{
    const bool full = (range == AVCOL_RANGE_JPEG);
    bool bt709;
    switch (space) {
    case AVCOL_SPC_BT709:                    bt709 = true;  break;
    case AVCOL_SPC_BT470BG:
    case AVCOL_SPC_SMPTE170M:                bt709 = false; break;
    default:
        // Unspecified is overwhelmingly common. Resolution is the conventional
        // tiebreak: SD is 601, HD and above is 709.
        bt709 = height > 576;
        break;
    }
    if (full) {
        return bt709 ? kBt709Full : kBt601Full;
    }
    return bt709 ? kBt709Limited : kBt601Limited;
}

static inline uint8_t clamp8(int v)
{
    return static_cast<uint8_t>(v < 0 ? 0 : (v > 255 ? 255 : v));
}

// Converts planar 8-bit YUV 4:2:0 to packed RGB at an arbitrary target size.
//
// Every destination pixel averages the full source rectangle that maps onto it
// rather than point-sampling it. Point sampling a 4K frame down to panel size
// aliases burnt-in timecode and slates into noise, which is exactly the content
// people scrub video to read.
static void yuv420ToRgb(const uint8_t* const src[3], const int stride[3],
                        int sw, int sh, std::vector<uint8_t>& dst,
                        int dw, int dh, const YuvCoeffs& c)
{
    dst.resize(static_cast<size_t>(dw) * dh * 3);

    for (int dy = 0; dy < dh; ++dy) {
        const int sy0 = static_cast<int>((int64_t)dy * sh / dh);
        int sy1 = static_cast<int>((int64_t)(dy + 1) * sh / dh);
        if (sy1 <= sy0) {
            sy1 = sy0 + 1;
        }

        for (int dx = 0; dx < dw; ++dx) {
            const int sx0 = static_cast<int>((int64_t)dx * sw / dw);
            int sx1 = static_cast<int>((int64_t)(dx + 1) * sw / dw);
            if (sx1 <= sx0) {
                sx1 = sx0 + 1;
            }

            int accY = 0, accU = 0, accV = 0, n = 0;
            for (int sy = sy0; sy < sy1; ++sy) {
                const uint8_t* rowY = src[0] + (size_t)sy * stride[0];
                const uint8_t* rowU = src[1] + (size_t)(sy >> 1) * stride[1];
                const uint8_t* rowV = src[2] + (size_t)(sy >> 1) * stride[2];
                for (int sx = sx0; sx < sx1; ++sx) {
                    accY += rowY[sx];
                    accU += rowU[sx >> 1];
                    accV += rowV[sx >> 1];
                    ++n;
                }
            }

            const int y = accY / n;
            const int u = accU / n - 128;
            const int v = accV / n - 128;
            // +128 rounds rather than truncates; matches pixelconvert.cpp
            const int yy = (y - c.yOffset) * c.yGain + 128;

            uint8_t* out = &dst[((size_t)dy * dw + dx) * 3];
            out[0] = clamp8((yy + c.rCr * v) >> 8);
            out[1] = clamp8((yy + c.gCb * u + c.gCr * v) >> 8);
            out[2] = clamp8((yy + c.bCb * u) >> 8);
        }
    }
}

// ---------------------------------------------------------------------------
// Decoder
// ---------------------------------------------------------------------------

struct Decoder
{
    AVFormatContext* fmt = nullptr;
    AVCodecContext* dec = nullptr;
    AVStream* st = nullptr;
    int idx = -1;
    AVPacket* pkt = nullptr;
    AVFrame* frame = nullptr;
    int64_t lastPts = AV_NOPTS_VALUE;

    ~Decoder()
    {
        if (frame) { av_frame_free(&frame); }
        if (pkt) { av_packet_free(&pkt); }
        if (dec) { avcodec_free_context(&dec); }
        if (fmt) { avformat_close_input(&fmt); }
    }

    bool open(const char* path)
    {
        if (avformat_open_input(&fmt, path, nullptr, nullptr) < 0) {
            fprintf(stderr, "cannot open %s\n", path);
            return false;
        }
        if (avformat_find_stream_info(fmt, nullptr) < 0) {
            fprintf(stderr, "no stream info\n");
            return false;
        }

        // First video stream that is not an attached cover image.
        for (unsigned i = 0; i < fmt->nb_streams; ++i) {
            AVStream* s = fmt->streams[i];
            if (s->codecpar->codec_type != AVMEDIA_TYPE_VIDEO) {
                continue;
            }
            if (s->disposition & AV_DISPOSITION_ATTACHED_PIC) {
                continue;
            }
            idx = static_cast<int>(i);
            st = s;
            break;
        }
        if (idx < 0) {
            fprintf(stderr, "no video stream\n");
            return false;
        }

        const AVCodec* codec = avcodec_find_decoder(st->codecpar->codec_id);
        if (!codec) {
            fprintf(stderr, "no decoder for codec %d\n", st->codecpar->codec_id);
            return false;
        }
        dec = avcodec_alloc_context3(codec);
        avcodec_parameters_to_context(dec, st->codecpar);
        dec->thread_count = 4;
        dec->thread_type = FF_THREAD_FRAME | FF_THREAD_SLICE;
        if (avcodec_open2(dec, codec, nullptr) < 0) {
            fprintf(stderr, "cannot open decoder\n");
            return false;
        }

        pkt = av_packet_alloc();
        frame = av_frame_alloc();
        return true;
    }

    double timeBase() const { return av_q2d(st->time_base); }

    int64_t toPts(double seconds) const
    {
        return static_cast<int64_t>(llround(seconds / av_q2d(st->time_base)));
    }

    void flush()
    {
        avcodec_flush_buffers(dec);
        lastPts = AV_NOPTS_VALUE;
    }

    // Decodes forward until the frame whose presentation interval contains
    // targetPts, using the half-open convention [pts, pts + duration).
    bool decodeUpTo(int64_t targetPts)
    {
        while (true) {
            const int r = avcodec_receive_frame(dec, frame);
            if (r == 0) {
                int64_t pts = frame->best_effort_timestamp;
                if (pts == AV_NOPTS_VALUE) {
                    pts = frame->pts;
                }
                lastPts = pts;

                int64_t dur = frame->duration;
                if (dur <= 0) {
                    dur = st->avg_frame_rate.num
                          ? static_cast<int64_t>(llround(
                                1.0 / av_q2d(st->avg_frame_rate)
                                / av_q2d(st->time_base)))
                          : 1;
                }
                if (pts + dur > targetPts) {
                    return true;  // this frame covers the target
                }
                continue;         // still behind, keep going
            }
            if (r == AVERROR(EAGAIN)) {
                // Feed the decoder another packet from our stream.
                bool fed = false;
                while (av_read_frame(fmt, pkt) >= 0) {
                    if (pkt->stream_index == idx) {
                        avcodec_send_packet(dec, pkt);
                        av_packet_unref(pkt);
                        fed = true;
                        break;
                    }
                    av_packet_unref(pkt);
                }
                if (!fed) {
                    avcodec_send_packet(dec, nullptr);  // drain
                }
                continue;
            }
            return false;  // EOF or hard error
        }
    }

    // Full seek path: keyframe seek backwards, flush, then decode forward.
    bool seekAndDecode(double seconds)
    {
        const int64_t target = toPts(seconds);
        if (av_seek_frame(fmt, idx, target, AVSEEK_FLAG_BACKWARD) < 0) {
            return false;
        }
        flush();
        return decodeUpTo(target);
    }
};

// ---------------------------------------------------------------------------
// Reference comparison against swscale. Development-time only; swscale is not
// a runtime dependency of the feature.
// ---------------------------------------------------------------------------

static void compareWithSwscale(const AVFrame* f, const YuvCoeffs& coeffs,
                               int dw, int dh, const char* label)
{
    const uint8_t* src[3] = { f->data[0], f->data[1], f->data[2] };
    const int stride[3] = { f->linesize[0], f->linesize[1], f->linesize[2] };

    std::vector<uint8_t> mine;
    yuv420ToRgb(src, stride, f->width, f->height, mine, dw, dh, coeffs);

    // SWS_AREA is the box-average equivalent of what the integer path does.
    SwsContext* sws = sws_getContext(
        f->width, f->height, static_cast<AVPixelFormat>(f->format),
        dw, dh, AV_PIX_FMT_RGB24,
        (dw == f->width && dh == f->height) ? SWS_POINT : SWS_AREA,
        nullptr, nullptr, nullptr);
    if (!sws) {
        printf("    %-34s swscale unavailable\n", label);
        return;
    }

    // Match the colour handling the frame declares, so we are comparing the
    // same conversion rather than two different intents.
    const int* table = sws_getCoefficients(
        f->colorspace == AVCOL_SPC_UNSPECIFIED
            ? (f->height > 576 ? SWS_CS_ITU709 : SWS_CS_ITU601)
            : f->colorspace);
    int srcRange, dstRange, brightness, contrast, saturation;
    const int* inv;
    sws_getColorspaceDetails(sws, const_cast<int**>(&inv), &srcRange,
                             const_cast<int**>(&table), &dstRange,
                             &brightness, &contrast, &saturation);
    sws_setColorspaceDetails(sws, table,
                             f->color_range == AVCOL_RANGE_JPEG ? 1 : 0,
                             table, 1, brightness, contrast, saturation);

    std::vector<uint8_t> ref((size_t)dw * dh * 3);
    uint8_t* dstData[4] = { ref.data(), nullptr, nullptr, nullptr };
    int dstStride[4] = { dw * 3, 0, 0, 0 };
    sws_scale(sws, src, stride, 0, f->height, dstData, dstStride);
    sws_freeContext(sws);

    long total = 0;
    int worst = 0;
    size_t over2 = 0;
    for (size_t i = 0; i < mine.size(); ++i) {
        const int diff = abs(static_cast<int>(mine[i]) - static_cast<int>(ref[i]));
        total += diff;
        if (diff > worst) { worst = diff; }
        if (diff > 2) { ++over2; }
    }
    const double mean = mine.empty() ? 0.0 : double(total) / mine.size();
    const double pctOver2 = mine.empty() ? 0.0 : 100.0 * over2 / mine.size();
    printf("    %-34s mean %.3f  max %d  over-2 %.3f%%\n",
           label, mean, worst, pctOver2);
}

// ---------------------------------------------------------------------------
// The check: the fixture lights a 360x360 patch at x=460,y=40 on marker frames.
// ---------------------------------------------------------------------------

static int patchLuma(const AVFrame* f)
{
    const int x0 = 460, y0 = 40, side = 360;
    long sum = 0;
    int n = 0;
    for (int y = y0; y < y0 + side && y < f->height; ++y) {
        const uint8_t* row = f->data[0] + (size_t)y * f->linesize[0];
        for (int x = x0; x < x0 + side && x < f->width; ++x) {
            sum += row[x];
            ++n;
        }
    }
    return n ? static_cast<int>(sum / n) : 0;
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        fprintf(stderr, "usage: videoprobe <file> [fps] [markerIntervalSec]\n");
        return 2;
    }
    const char* path = argv[1];
    const double fps = argc > 2 ? atof(argv[2]) : 25.0;
    const double markerEvery = argc > 3 ? atof(argv[3]) : 1.0;

    Decoder d;
    if (!d.open(path)) {
        return 2;
    }

    const YuvCoeffs& coeffs =
        pickCoeffs(d.dec->colorspace, d.dec->color_range, d.dec->height);

    printf("%s\n", path);
    printf("  %dx%d  %s  time_base %d/%d  start_pts %lld  fmt %s\n",
           d.dec->width, d.dec->height, coeffs.name,
           d.st->time_base.num, d.st->time_base.den,
           (long long)d.st->start_time,
           av_get_pix_fmt_name(d.dec->pix_fmt));

    if (d.dec->pix_fmt != AV_PIX_FMT_YUV420P
        && d.dec->pix_fmt != AV_PIX_FMT_YUVJ420P) {
        printf("  SKIP: prototype converter handles yuv420p/yuvj420p only\n");
        return 0;
    }

    // Walk the marker times in a deliberately awkward order. A sequential walk
    // would be served by decode-forward and would never exercise the seek path;
    // jumping backwards forces a real keyframe seek and decoder flush each time.
    std::vector<double> targets;
    for (int i = 0; i < 10; ++i) {
        targets.push_back(i * markerEvery);
    }
    std::vector<double> order = { targets[7], targets[1], targets[9],
                                  targets[0], targets[5], targets[2],
                                  targets[8], targets[3], targets[6],
                                  targets[4] };

    int failures = 0;
    std::vector<uint8_t> rgb;

    for (double t : order) {
        // Aim at the middle of the marker frame so rounding cannot put us on
        // the neighbour: the frame covers [t, t + 1/fps). seekAndDecode adds
        // the stream start time itself, so this stays content-relative.
        const double aim = t + 0.5 / fps;
        if (!d.seekAndDecode(aim)) {
            printf("  t=%6.3f  DECODE FAILED\n", t);
            ++failures;
            continue;
        }

        const int64_t pts = d.lastPts;
        const double got = pts * d.timeBase();
        const int frameNo = static_cast<int>(llround(got * fps));
        const int luma = patchLuma(d.frame);
        const bool shouldBeLit = (llround(t / markerEvery) * markerEvery == t);
        const bool isLit = luma > 200;
        const bool ok = (isLit == shouldBeLit)
                        && (fabs(got - t) < 0.5 / fps);

        // Exercise the converter at panel size on every hit.
        const uint8_t* src[3] = { d.frame->data[0], d.frame->data[1], d.frame->data[2] };
        const int stride[3] = { d.frame->linesize[0], d.frame->linesize[1], d.frame->linesize[2] };
        yuv420ToRgb(src, stride, d.frame->width, d.frame->height, rgb, 640, 360, coeffs);

        printf("  want t=%6.3f (frame %3d)  got t=%6.3f (frame %3d)  patch=%3d  %s\n",
               t, (int)llround(t * fps), got, frameNo, luma, ok ? "ok" : "WRONG");
        if (!ok) {
            ++failures;
        }
    }

    // ---- converter accuracy against swscale --------------------------------
    // The plan calls the hand-rolled converter the single biggest maintenance
    // risk, on the grounds that there is no reference to diff against. There
    // is one right here at development time, even though it will not be linked
    // at runtime. Comparing at native size isolates the colour matrix from the
    // scaler; comparing downscaled also exercises the box filter.
    if (d.frame->width > 0) {
        printf("\n  converter vs swscale\n");
        compareWithSwscale(d.frame, coeffs, d.frame->width, d.frame->height,
                           "native size (colour matrix only)");
        compareWithSwscale(d.frame, coeffs, 640, 360,
                           "640x360 (matrix + downscale)");
    }

    printf("\n  %s (%d/%zu wrong)\n\n",
           failures ? "FAILED" : "PASSED", failures, order.size());
    return failures ? 1 : 0;
}
