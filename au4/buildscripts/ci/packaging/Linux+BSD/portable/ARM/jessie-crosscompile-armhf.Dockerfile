FROM debian:jessie

ADD https://raw.githubusercontent.com/ericfont/MuseScore/compile-armhf/build/Linux%2BBSD/portable/RecipeDebian /

RUN chmod +x RecipeDebian
RUN ./RecipeDebian --fetch-build-dependencies-only armhf
RUN ./RecipeDebian --fetch-package-dependencies-only armhf
