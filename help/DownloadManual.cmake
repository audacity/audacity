message(STATUS
   "${VERSION} -> ${DESTINATION}"
)

file(
   DOWNLOAD
   "https://github.com/audacity/audacity-manual/releases/download/v${VERSION}/audacity-manual-${VERSION}.tar.gz"
   "${DESTINATION}/audacity-manual-${VERSION}.tar.gz"
)
