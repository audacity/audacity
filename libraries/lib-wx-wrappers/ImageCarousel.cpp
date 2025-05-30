/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "ImageCarousel.h"
#include "Prefs.h"
#include "Theme.h"
#include "AllThemeResources.h"
#include "WindowAccessible.h"

ImageCarousel::ImageCarousel(wxWindow* parent, const std::vector<CarouselSnapshot>& snapshots, wxWindowID winid, const wxPoint& pos, const wxSize& size)
    : m_snapshots(snapshots), m_id(winid), wxPanel(parent, winid, pos, size) {
   SetBackgroundStyle(wxBG_STYLE_PAINT);
   SetBackgroundColour(theTheme.Colour(clrMedium));
   SetWindowStyleFlag(GetWindowStyleFlag() | wxTAB_TRAVERSAL);

   gPrefs->Read(wxT("/GUI/IntroOrderStart"), &m_currentIndex, 0);
   m_currentIndex = m_currentIndex % m_snapshots.size();
   int nextLaunchIndex = (m_currentIndex + 1) % m_snapshots.size();
   gPrefs->Write(wxT("/GUI/IntroOrderStart"), nextLaunchIndex);


#if defined (__WXOSX__) || defined(__WXMSW__)
   m_btnLeft = new ArrowButton(this, ArrowDirection::Left);
   m_btnMiddle = new GradientButton(this, m_id,
      m_snapshots[m_currentIndex].buttonText.Translation(), wxDefaultPosition, wxDefaultSize);
   m_btnRight = new ArrowButton(this, ArrowDirection::Right);
#if wxUSE_ACCESSIBILITY
   safenew WindowAccessible(m_btnLeft);
   safenew WindowAccessible(m_btnMiddle);
   safenew WindowAccessible(m_btnRight);
#endif
#else
   m_btnLeft = new wxButton(this, wxID_ANY, "<", wxDefaultPosition, FromDIP(wxSize(48, 48)));
   m_btnMiddle = new wxButton(this, m_id,
      m_snapshots[m_currentIndex].buttonText.Translation(), wxDefaultPosition, wxDefaultSize);
   m_btnRight = new wxButton(this, wxID_ANY, ">", wxDefaultPosition, FromDIP(wxSize(48, 48)));
#endif

#if defined (__WXOSX__) || defined(__WXMSW__)
   m_btnLeft->SetClickHandler([this] { OnLeftClicked(); });
   m_btnRight->SetClickHandler([this] { OnRightClicked(); });
#else
   m_btnLeft->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) {
      OnLeftClicked();
   });
   m_btnRight->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) {
      OnRightClicked();
   });
#endif
   m_btnMiddle->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) {
      OpenURL();
   });

   Bind(wxEVT_PAINT, &ImageCarousel::OnPaint, this);
   Bind(wxEVT_LEFT_DOWN, &ImageCarousel::OnMouseClick, this);
   Bind(wxEVT_SIZE, &ImageCarousel::OnResize, this);
}

ImageCarousel::~ImageCarousel()
{
   m_btnLeft->Destroy();
   m_btnRight->Destroy();
   m_btnMiddle->Destroy();
}

void ImageCarousel::OnPaint(wxPaintEvent& event) {
   wxAutoBufferedPaintDC dc(this);
   dc.Clear();

   if (m_snapshots.empty()) {
      return;
   }

   wxSize size = GetClientSize();

   DrawTitle(dc, size);

   wxBitmap& bmp = m_snapshots[m_currentIndex].bitmap;
   // center image
   int x = (size.GetWidth() - bmp.GetWidth()) / 2;
   int y = (size.GetHeight() - bmp.GetHeight()) / 2 - 20;

   dc.DrawBitmap(bmp, x, y, true);

   // save rect size for mouse-click purposes
   m_imageRect = wxRect(x, y, bmp.GetWidth(), bmp.GetHeight());

   DrawDots(dc, size);
}

void ImageCarousel::OnResize(wxSizeEvent& event) {
   if (!m_snapshots.empty()) {
      UpdateButtons();
   }

   event.Skip();
}

void ImageCarousel::DrawTitle(wxDC& dc, const wxSize& size) {
#if defined(__WXMSW__)
   wxFont titleFont(19, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD);
#elif defined(__WXOSX__)
   wxFont titleFont(22, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD);
#else
   wxFont titleFont(14, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD);
#endif
   dc.SetFont(titleFont);

   wxSize textSize = dc.GetTextExtent(m_snapshots[m_currentIndex].title.Translation());
   int textX = (size.GetWidth() - textSize.GetWidth()) / 2;
   int textY = 25;

   dc.SetTextForeground(theTheme.Colour(clrTrackPanelText));
   dc.DrawText(m_snapshots[m_currentIndex].title.Translation(), textX, textY);
}

void ImageCarousel::UpdateButtons()
{
   wxSize clientSize = GetClientSize();
   const wxBitmap& bmp = m_snapshots[m_currentIndex].bitmap;
   int x = (clientSize.GetWidth() - bmp.GetWidth()) / 2;
   int y = (clientSize.GetHeight() - bmp.GetHeight()) / 2 - 20;

   m_btnLeft->SetPosition(wxPoint(x - m_btnLeft->GetSize().GetWidth() - 36, y + bmp.GetHeight() / 2 - 24));
   m_btnRight->SetPosition(wxPoint(x + bmp.GetWidth() + 36, y + bmp.GetHeight() / 2 - 24));

   const auto translated = m_snapshots[m_currentIndex].buttonText.Translation();
   m_btnMiddle->SetLabel(translated);
#if defined(__WXMSW__)
   wxFont labelFont(11, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
#elif defined(__WXOSX__)
   wxFont labelFont(14, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
#endif

#if defined(__WXMSW__) || defined(__WXOSX__)
   m_btnMiddle->SetFont(labelFont);
#endif

   m_btnLeft->SetToolTip(_("Previous slide"));
   m_btnLeft->SetName(_("Previous slide"));     // for screen readers
   m_btnRight->SetToolTip(_("Next slide"));
   m_btnRight->SetName(_("Next slide"));        // for screen readers
   if (m_snapshots[m_currentIndex].imageText.empty()) {
      // for screen readers
      m_btnMiddle->SetName(wxString::Format(_("Slide %d of %d, %s. %s"),
                                               m_currentIndex + 1,
                                               static_cast<int>(m_snapshots.size()),
                                               m_snapshots[m_currentIndex].title.Translation(),
                                               translated));
   } else {
      // for screen readers
      m_btnMiddle->SetName(wxString::Format(_("Slide %d of %d, %s, %s. %s"),
                                               m_currentIndex + 1,
                                               static_cast<int>(m_snapshots.size()),
                                               m_snapshots[m_currentIndex].title.Translation(),
                                               m_snapshots[m_currentIndex].imageText.Translation(),
                                               translated));
   }

   wxSize btnSize = m_btnMiddle->GetBestSize();
#if defined(__WXMSW__)
   m_btnMiddle->SetSize(wxSize(btnSize.GetWidth() + 30, btnSize.GetHeight() + 20));
#else
   m_btnMiddle->SetSize(wxSize(btnSize.GetWidth() + 30, btnSize.GetHeight() + 15));
#endif

#if defined(__WXMSW__) || defined(__WXOSX__)
   m_btnMiddle->SetPosition(wxPoint(x + bmp.GetWidth() / 2 - m_btnMiddle->GetSize().GetWidth() / 2 , y + bmp.GetHeight() + 22));
#else
   m_btnMiddle->SetPosition(wxPoint(x + bmp.GetWidth() / 2 - m_btnMiddle->GetSize().GetWidth() / 2 , y + bmp.GetHeight() + FromDIP(10)));
#endif
}

void ImageCarousel::DrawDots(wxDC& dc, const wxSize& size)
{
   const int dotRadius = 6;
   const int spacing = 12;
   const int numDots = static_cast<int>(m_snapshots.size());

   int totalWidth = numDots * (dotRadius * 2) + (numDots - 1) * spacing;
   int startX = (size.GetWidth() - totalWidth) / 2;
#if defined(__WXMSW__) || defined(__WXOSX__)
   int y = size.GetHeight() - 38;
#else
   int y = m_btnMiddle->GetPosition().y + m_btnMiddle->GetSize().GetHeight() + FromDIP(5);
#endif

   for (int i = 0; i < numDots; ++i)
   {
      wxColour color = (i == m_currentIndex)
                        ? theTheme.Colour(clrTrackPanelText)
                        : theTheme.Colour(clrDark);

      dc.SetBrush(wxBrush(color));
      dc.SetPen(*wxTRANSPARENT_PEN);

      int x = startX + i * ((dotRadius * 2) + spacing);
      dc.DrawEllipse(x, y, dotRadius * 2, dotRadius * 2);
   }
}

void ImageCarousel::Advance(int direction)
{
   int n = m_snapshots.size();
   m_currentIndex = (m_currentIndex + direction + n) % n;
   Refresh();
   UpdateButtons();
}

void ImageCarousel::OnLeftClicked()
{
   Advance(-1);
}

void ImageCarousel::OnRightClicked()
{
   Advance(1);
}

void ImageCarousel::OnMouseClick(wxMouseEvent& event)
{
    wxPoint clickPos = event.GetPosition();
    if (m_imageRect.Contains(clickPos)) {
       OpenURL();
    }
}

void ImageCarousel::OpenURL()
{
   if (m_snapshots.empty()) {
      return;
   }

   const wxString& url = m_snapshots[m_currentIndex].url;
   wxLaunchDefaultBrowser(url);
}
