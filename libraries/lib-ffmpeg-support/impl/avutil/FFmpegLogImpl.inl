/**********************************************************************

  Audacity: A Digital Audio Editor

  FFmpegLogImpl.inl

  Dmitry Vedenko

**********************************************************************/

//! It is assumed that the lifetime of this is between loading and unloading
//! of ffmpeg libraries.
class FFmpegLogImpl : public FFmpegLog
{
public:
   using Callback = void (*)(void*, int, const char*, va_list);
   using CallbackSetter = void (*)(Callback);
   explicit FFmpegLogImpl(
      CallbackSetter av_log_set_callback, Callback oldCallback)
      : mCallbackSetter{ av_log_set_callback }
      , mOldCallback{ oldCallback }
   {
      if (mCallbackSetter)
         mCallbackSetter(LogCallback);
   }
   ~FFmpegLogImpl() override
   {
      if (mCallbackSetter)
         mCallbackSetter(mOldCallback);
   }

private:
   static void LogCallback(void* ptr, int level, const char* fmt, va_list vl)
   {
      if (level > AV_LOG_INFO)
         return;

      wxString message;

      if (ptr != nullptr)
      {
         AVClass* cls = *static_cast<AVClass**>(ptr);

         message = wxString::Format(
            wxT("[%s @ %p] "),
            wxString::FromUTF8(cls->item_name(ptr)),
            ptr
         );
      }

      message += wxString::FormatV(wxString::FromUTF8(fmt), vl);

      wxString cpt;
      switch (level)
      {
      case 0:
         cpt = wxT("Error");
         wxLogError(message);
         break;
      case 1:
         cpt = wxT("Info");
         wxLogInfo(message);
         break;
      case 2:
         cpt = wxT("Debug");
         wxLogInfo(message);
         break;
      default:
         cpt = wxT("Log");
         wxLogInfo(message);
         break;
      }

      wxLogDebug(wxT("%s: %s"), cpt, message);
   }

   CallbackSetter mCallbackSetter;
   Callback mOldCallback;
};

std::unique_ptr<FFmpegLog> CreateLogCallbackSetter(const FFmpegFunctions& ffmpeg)
{

   return std::make_unique<FFmpegLogImpl>(
      ffmpeg.av_log_set_callback, ffmpeg.av_log_default_callback);
}
