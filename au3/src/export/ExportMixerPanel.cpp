/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMixerPanel.cpp

  Dominic Mazzoni

**********************************************************************/

#include "ExportMixerPanel.h"

#include <wx/dcclient.h>
#include <wx/dcmemory.h>

#include "MixerOptions.h"
#include "AColor.h"

BEGIN_EVENT_TABLE(ExportMixerPanel, wxPanelWrapper)
EVT_PAINT(ExportMixerPanel::OnPaint)
EVT_MOUSE_EVENTS(ExportMixerPanel::OnMouseEvent)
END_EVENT_TABLE()

ExportMixerPanel::ExportMixerPanel(wxWindow* parent, wxWindowID id,
                                   MixerOptions::Downmix* mixerSpec,
                                   wxArrayString trackNames,
                                   const wxPoint& pos, const wxSize& size)
    : wxPanelWrapper(parent, id, pos, size)
    , mMixerSpec{mixerSpec}
    , mChannelRects{mMixerSpec->GetMaxNumChannels()}
    , mTrackRects{mMixerSpec->GetNumTracks()}
{
    mBitmap = NULL;
    mWidth = 0;
    mHeight = 0;
    mSelectedTrack = mSelectedChannel = -1;

    mTrackNames = trackNames;
}

ExportMixerPanel::~ExportMixerPanel()
{
}

//set the font on memDC such that text can fit in specified width and height
void ExportMixerPanel::SetFont(wxMemoryDC& memDC, const wxString& text, int width,
                               int height)
{
    int l = 0, u = 13, m, w, h;
    wxFont font = memDC.GetFont();
    while (l < u - 1)
    {
        m = (l + u) / 2;
        font.SetPointSize(m);
        memDC.SetFont(font);
        memDC.GetTextExtent(text, &w, &h);

        if (w < width && h < height) {
            l = m;
        } else {
            u = m;
        }
    }
    font.SetPointSize(l);
    memDC.SetFont(font);
}

void ExportMixerPanel::OnPaint(wxPaintEvent& WXUNUSED(event))
{
    wxPaintDC dc(this);

    int width, height;
    GetSize(&width, &height);

    if (!mBitmap || mWidth != width || mHeight != height) {
        mWidth = width;
        mHeight = height;
        mBitmap = std::make_unique<wxBitmap>(mWidth, mHeight, 24);
    }

    wxColour bkgnd = GetBackgroundColour();
    wxBrush bkgndBrush(bkgnd, wxBRUSHSTYLE_SOLID);

    wxMemoryDC memDC;
    memDC.SelectObject(*mBitmap);

    //draw background
    wxRect bkgndRect;
    bkgndRect.x = 0;
    bkgndRect.y = 0;
    bkgndRect.width = mWidth;
    bkgndRect.height = mHeight;

    memDC.SetBrush(*wxWHITE_BRUSH);
    memDC.SetPen(*wxBLACK_PEN);
    memDC.DrawRectangle(bkgndRect);

    //box dimensions
    mBoxWidth = mWidth / 6;

    mTrackHeight = (mHeight * 3) / (mMixerSpec->GetNumTracks() * 4);
    if (mTrackHeight > 30) {
        mTrackHeight = 30;
    }

    mChannelHeight = (mHeight * 3) / (mMixerSpec->GetNumChannels() * 4);
    if (mChannelHeight > 30) {
        mChannelHeight = 30;
    }

    static double PI = 2 * acos(0.0);
    double angle = atan((3.0 * mHeight) / mWidth);
    double radius = mHeight / (2.0 * sin(PI - 2.0 * angle));
    double totAngle = (asin(mHeight / (2.0 * radius)) * 2.0);

    //draw tracks
    memDC.SetBrush(AColor::envelopeBrush);
    angle = totAngle / (mMixerSpec->GetNumTracks() + 1);

    int max = 0, w, h;
    for ( unsigned int i = 1; i < mMixerSpec->GetNumTracks(); i++ ) {
        if (mTrackNames[ i ].length() > mTrackNames[ max ].length()) {
            max = i;
        }
    }

    SetFont(memDC, mTrackNames[ max ], mBoxWidth, mTrackHeight);

    for ( unsigned int i = 0; i < mMixerSpec->GetNumTracks(); i++ ) {
        mTrackRects[ i ].x = (int)(mBoxWidth * 2 + radius - radius
                                   * cos(totAngle / 2.0 - angle * (i + 1)) - mBoxWidth + 0.5);
        mTrackRects[ i ].y = (int)(mHeight * 0.5 - radius
                                   * sin(totAngle * 0.5 - angle * (i + 1.0))
                                   - 0.5 * mTrackHeight + 0.5);

        mTrackRects[ i ].width = mBoxWidth;
        mTrackRects[ i ].height = mTrackHeight;

        memDC.SetPen(mSelectedTrack == (int)i ? *wxRED_PEN : *wxBLACK_PEN);
        memDC.DrawRectangle(mTrackRects[ i ]);

        memDC.GetTextExtent(mTrackNames[ i ], &w, &h);
        memDC.DrawText(mTrackNames[ i ],
                       mTrackRects[ i ].x + (mBoxWidth - w) / 2,
                       mTrackRects[ i ].y + (mTrackHeight - h) / 2);
    }

    //draw channels
    memDC.SetBrush(AColor::playRegionBrush[ 0 ]);
    angle = (asin(mHeight / (2.0 * radius)) * 2.0)
            / (mMixerSpec->GetNumChannels() + 1);

    SetFont(memDC, wxT("Channel: XX"), mBoxWidth, mChannelHeight);
    memDC.GetTextExtent(wxT("Channel: XX"), &w, &h);

    for ( unsigned int i = 0; i < mMixerSpec->GetNumChannels(); i++ ) {
        mChannelRects[ i ].x = (int)(mBoxWidth * 4 - radius + radius
                                     * cos(totAngle * 0.5 - angle * (i + 1)) + 0.5);
        mChannelRects[ i ].y = (int)(mHeight * 0.5 - radius
                                     * sin(totAngle * 0.5 - angle * (i + 1))
                                     - 0.5 * mChannelHeight + 0.5);

        mChannelRects[ i ].width = mBoxWidth;
        mChannelRects[ i ].height = mChannelHeight;

        memDC.SetPen(mSelectedChannel == (int)i ? *wxRED_PEN : *wxBLACK_PEN);
        memDC.DrawRectangle(mChannelRects[ i ]);

        memDC.DrawText(wxString::Format(_("Channel: %2d"), i + 1),
                       mChannelRects[ i ].x + (mBoxWidth - w) / 2,
                       mChannelRects[ i ].y + (mChannelHeight - h) / 2);
    }

    //draw links
    memDC.SetPen(wxPen(*wxBLACK, mHeight / 200));
    for ( unsigned int i = 0; i < mMixerSpec->GetNumTracks(); i++ ) {
        for ( unsigned int j = 0; j < mMixerSpec->GetNumChannels(); j++ ) {
            if (mMixerSpec->mMap[ i ][ j ]) {
                AColor::Line(memDC, mTrackRects[ i ].x + mBoxWidth,
                             mTrackRects[ i ].y + mTrackHeight / 2, mChannelRects[ j ].x,
                             mChannelRects[ j ].y + mChannelHeight / 2);
            }
        }
    }

    dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
}

double ExportMixerPanel::Distance(wxPoint& a, wxPoint& b)
{
    return sqrt(pow(a.x - b.x, 2.0) + pow(a.y - b.y, 2.0));
}

//checks if p is on the line connecting la, lb with tolerance
bool ExportMixerPanel::IsOnLine(wxPoint p, wxPoint la, wxPoint lb)
{
    return Distance(p, la) + Distance(p, lb) - Distance(la, lb) < 0.1;
}

void ExportMixerPanel::OnMouseEvent(wxMouseEvent& event)
{
    if (event.ButtonDown()) {
        bool reset = true;
        //check tracks
        for ( unsigned int i = 0; i < mMixerSpec->GetNumTracks(); i++ ) {
            if (mTrackRects[ i ].Contains(event.m_x, event.m_y)) {
                reset = false;
                if (mSelectedTrack == (int)i) {
                    mSelectedTrack = -1;
                } else {
                    mSelectedTrack = i;
                    if (mSelectedChannel != -1) {
                        mMixerSpec->mMap[ mSelectedTrack ][ mSelectedChannel ]
                            =!mMixerSpec->mMap[ mSelectedTrack ][ mSelectedChannel ];
                    }
                }
                goto found;
            }
        }

        //check channels
        for ( unsigned int i = 0; i < mMixerSpec->GetNumChannels(); i++ ) {
            if (mChannelRects[ i ].Contains(event.m_x, event.m_y)) {
                reset = false;
                if (mSelectedChannel == (int)i) {
                    mSelectedChannel = -1;
                } else {
                    mSelectedChannel = i;
                    if (mSelectedTrack != -1) {
                        mMixerSpec->mMap[ mSelectedTrack ][ mSelectedChannel ]
                            =!mMixerSpec->mMap[ mSelectedTrack ][ mSelectedChannel ];
                    }
                }
                goto found;
            }
        }

        //check links
        for ( unsigned int i = 0; i < mMixerSpec->GetNumTracks(); i++ ) {
            for ( unsigned int j = 0; j < mMixerSpec->GetNumChannels(); j++ ) {
                if (mMixerSpec->mMap[ i ][ j ] && IsOnLine(wxPoint(event.m_x,
                                                                   event.m_y), wxPoint(mTrackRects[ i ].x + mBoxWidth,
                                                                                       mTrackRects[ i ].y + mTrackHeight / 2),
                                                           wxPoint(mChannelRects[ j ].x, mChannelRects[ j ].y
                                                                   + mChannelHeight / 2))) {
                    mMixerSpec->mMap[ i ][ j ] = false;
                }
            }
        }

found:
        if (reset) {
            mSelectedTrack = mSelectedChannel = -1;
        }
        Refresh(false);
    }
}
