#!/usr/bin/env bash
# Checks if any dependency we build/bundle ourselves resolves to a system library,
set -euo pipefail

APP="${1:?usage: $0 <app-binary>}"
PREFIX="$(cd "$(dirname "$APP")/.." && pwd)"

# Libraries to check
OWNED='libvorbis|libvorbisenc|libvorbisfile|libogg|libFLAC|libopus|libopusfile|libsndfile|libmpg123|libmp3lame|libwavpack|libwx_base|libexpat|libportaudio|libfdk-aac|libfreetype|libharfbuzz|libz\.'

# Crashpad must be self-contained too
CRASHPAD_SSL='libssl\.so|libcrypto\.so'
CRASHPAD_CURL_DLOPEN='libcurl(-gnutls|-nss)?\.so'

mapfile -t executables < <(find "$PREFIX/bin" -type f -perm -111)

leaks=""
crashpad_ssl=""
crashpad_curl=""
for exe in "${executables[@]}"; do
    deps="$(LD_LIBRARY_PATH="$PREFIX/lib" ldd "$exe" 2>/dev/null || true)"
    exe_leaks="$(grep -E "$OWNED" <<< "$deps" | grep -E '=> /usr|=> /lib' || true)"
    if [ -n "$exe_leaks" ]; then
        leaks+="${exe}"$'\n'"${exe_leaks}"$'\n'
    fi
    if [ "$(basename "$exe")" = "crashpad_handler" ]; then
        exe_ssl="$(grep -E "$CRASHPAD_SSL" <<< "$deps" | grep -E '=> /usr|=> /lib|not found' || true)"
        if [ -n "$exe_ssl" ]; then
            crashpad_ssl+="${exe}"$'\n'"${exe_ssl}"$'\n'
        fi
        exe_curl="$(grep -aEo "$CRASHPAD_CURL_DLOPEN" "$exe" || true)"
        if [ -n "$exe_curl" ]; then
            crashpad_curl+="${exe}"$'\n'"${exe_curl}"$'\n'
        fi
    fi
done

if [ -n "$crashpad_ssl" ]; then
    echo "NOT self-contained, crashpad_handler OpenSSL libs resolve outside the bundle:" >&2
    echo "$crashpad_ssl" >&2
    exit 1
fi

if [ -n "$crashpad_curl" ]; then
    echo "NOT self-contained, crashpad_handler uses libcurl via dlopen:" >&2
    echo "$crashpad_curl" >&2
    exit 1
fi

if [ -n "$leaks" ]; then
    echo "NOT self-contained, these owned libraries resolve to system paths:" >&2
    echo "$leaks" >&2
    exit 1
fi

echo "OK: all owned dependencies resolve inside the build tree"
