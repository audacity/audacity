# Construyendo Audacity

## Requisitos Previos

* **python3** >= 3.8 (puedes encontrar una descarga oficial aquí: https://www.python.org/downloads/)
* **cmake** >= 3.16 (puedes encontrar una descarga oficial aquí: https://cmake.org/download/)
* Un compilador C++ 17 funcionando
* Graphviz (opcional)

Para Windows, vea abajo para la configuración importante del instalador.

Tenga en cuenta que el soporte de Xcode 14 en macOS requiere CMake 3.24.0 o posterior.

### Conan

Audacity utiliza [Conan 2](https://conan.io/) para administrar dependencias. Si conan no está ya instalado, Audacity descargará e instalará automáticamente.

Sin embargo, si desea instalar Conan manualmente, puede hacerlo siguiendo las instrucciones en el sitio web de [Conan sitio web](https://docs.conan.io/en/latest/installation.html).
La instalación manual puede ser útil si desea utilizar Conan para administrar dependencias para otros proyectos o si planea tener múltiples compilaciones de Audacity en la 
misma máquina.

### CMake

En Windows, por favor utilice los [binarios precompilados](https://cmake.org/download/). Asegúrese de seleccionar una de las opciones para agregar cmake al camino del sistema.

En macOS, la forma más fácil de instalar CMake es brew install cmake.

En Linux, cmake suele estar disponible desde el administrador de paquetes del sistema. Alternativamente, `sudo pip3 install cmake` se puede utilizar para instalar la última versión de CMake.

### Windows

Construimos Audacity utilizando [Microsoft Visual Studio](https://visualstudio.microsoft.com/vs/community/) 2019 y 2022. Para construir Audacity, se requiere la carga de trabajo de Desarrollo de escritorio con C++.

### macOS

Construimos Audacity utilizando versiones de XCode 12 y posteriores. Sin embargo, es probable que se pueda construir con XCode 7.

### Linux

Usamos GCC 9 y posteriores, pero cualquier compilador compatible con C++17 debería funcionar.

Aquí están las dependencias que necesita instalar en various familias de distribución.

#### Debian y Ubuntu

```
$ sudo apt-get update
$ sudo apt-get install -y build-essential cmake git python3-pip
$ sudo pip3 install conan
$ sudo apt-get install libgtk2.0-dev libasound2-dev libjack-jackd2-dev uuid-dev
```

#### openSUSE

```
$ sudo zypper refresh
$ sudo zypper install patterns-devel-C-C++-devel_C_C++ cmake git python3-pip \
                      gtk2-devel libjack-devel uuid-devel libSM-devel
$ sudo pip3 install conan
```

#### Fedora Workstation

```
$ sudo dnf update
$ sudo dnf install gcc-c++ cmake git python3-pip perl-core \
                   gtk2-devel gtk3-devel alsa-lib-devel jack-audio-connection-kit-devel uuid-devel libSM-devel
$ sudo pip3 install conan
```

### Graphviz

https://graphviz.org/download/

Esto no es necesario para construir y ejecutar Audacity. Genera diagramas que ayudan a comprender la estructura a gran escala del código fuente de Audacity.

Si instala Graphviz, entonces se generará un archivo de imagen modules.dot.svg en el directorio de construcción como un subproducto de la configuración. Muestra las dependencias entre el ejecutable de Audacity, sus módulos de extensión opcionales, sus bibliotecas compartidas y bibliotecas de terceros.

También podrá cambiar al directorio de scripts y ejecutar ./graph.pl para generar un diagrama de dependencias entre los archivos de código fuente dentro del ejecutable.

## Construyendo en Windows

1. Asegúrese de que la opción del instalador de Python Agregar Python 3.x a PATH esté marcada. Vaya a Configuración de Windows "Agregar o eliminar programas" y compruebe la opción Agregar Python a variables de entorno en la configuración de Python si Python no está en PATH.
  
2. Clone Audacity desde el proyecto de GitHub de Audacity.
  
   Por ejemplo, en **git-bash** ejecute:

    ```
    $ git clone https://github.com/audacity/audacity/
    ```

2. Abra la GUI de CMake.. 
   
   Establezca **Dónde está el código fuente** en la ubicación donde se clonó Audacity. 
   
   Establezca **Dónde construir los binarios** en la ubicación donde desea colocar su construcción. Es preferible que esta ubicación no esté dentro del directorio con el código fuente.

3. Presione **Configurar**. Puede elegir qué versión de Visual Studio usar y la plataforma para construir en la ventana emergente. Soportamos las plataformas x64 y Win32. La plataforma x64 es una opción predeterminada. Presione **Finalizar** para iniciar el proceso de configuración.

4. Después de la configuración exitosa, verá `Configuración realizada` en la última línea del registro. Presione **Generar** para generar el proyecto de Visual Studio.

5. Después de ver "Generación realizada", presione **Abrir proyecto** para abrir el proyecto en Visual Studio.
   
6. Seleccione "Compilar -> Compilar solución".
   
7. ¡Ahora puede ejecutar y depurar Audacity!
      
En general, los pasos 1-5 solo son necesarios la primera vez que configure. Luego, después de haber generado la solución, puede abrirlo en Visual Studio la próxima vez. Si la configuración del proyecto ha cambiado, el IDE invocará a CMake internamente. 

### Construyendo con soporte ASIO en Windows

Para habilitar el soporte ASIO, seleccione `audacity_has_asio_support=On` en CMake después de la configuración inicial y luego ejecute **Configurar** de nuevo como se describe anteriormente. ASIO solo es compatible con Windows y solo para construcciones de 64 bits.

## macOS

1. Clone Audacity desde el proyecto de GitHub de Audacity.
  
    ```
    $ git clone https://github.com/audacity/audacity/
    ```

2. Configurar Audacity usando CMake:
   ```
   $ mkdir build && cd build
   $ cmake -GXcode ../audacity
   ```

3. Abra el proyecto de Audacity XCode:
   ```
   $ open Audacity.xcodeproj
   ```
   y compila Audacity utilizando el IDE.

Los pasos 1 y 2 solo son necesarios para las construcciones por primera vez. 

Alternativamente, puede usar **CLion**. Si eliges hacerlo, abre el directorio donde has clonado Audacity usando CLion y listo.

Antes de Audacity 3.2 solo se admitían construcciones **x86_64** y el sistema de construcción "legacy" de XCode. Para compilar versiones antiguas, utilice:

```
   $ mkdir build && cd build
   $ cmake -GXcode -T buildsystem=1 ../audacity
```

Configurar Audacity. 

## Linux & Otros Sistemas Operativos

1. Clone Audacity desde el proyecto de GitHub de Audacity.
  
    ```
    $ git clone https://github.com/audacity/audacity/
    ```

2. Configurar Audacity con CMake:
   ```
   $ mkdir build && cd build
   $ cmake -G "Unix Makefiles" ../audacity
   ```
   Por defecto, se configurará la build de depuración. Para cambiar eso, pase `-DCMAKE_BUILD_TYPE=Release` a CMake.

3. Construir Audacity:
   ```
   $ make -j`nproc`
   ```

4. Probando la construcción:
   Agregar una carpeta "Configuración portátil" permite a Audacity ignorar la configuración de cualquier instalación existente de Audacity.
   ```
   $ cd Debug/bin
   $ mkdir "Configuración portátil"
   $ ./audacity
   ```

5. Instalando Audacity
   ```
   $ cd <build directory>
   $ sudo make install
   ```

## Avanzado

### Opciones de CMake

Puede utilizar `cmake -LH` para obtener una lista de las opciones disponibles (o utilice la GUI de CMake o `ccmake`). La lista incluirá documentación sobre cada opción.
### Construyendo utilizando bibliotecas del sistema

En Linux, es posible construir Audacity utilizando (casi) solo las bibliotecas proporcionadas por el administrador de paquetes. Por favor, consulte la lista de bibliotecas requeridas [here](linux/required_libraries.md).

```
$ mkdir build && cd build
$ cmake -G "Unix Makefiles" \
        -Daudacity_use_ffmpeg=loaded \
        -Daudacity_lib_preference=system \
        -Daudacity_obey_system_dependencies=On \
         ../audacity
```

Hay algunos casos en los que se prefiere la construcción de bibliotecas locales:

1. **wxWidgets**: Aunque Audacity en **Linux** utiliza la versión estándar de wxWidgets, **requerimos** que se utilice la versión **3.1.3**. Esta versión no está disponible en la mayoría de las distribuciones.
2. **portaudio-v19**: Audacity utiliza actualmente [algunas APIs privadas](https://github.com/audacity/audacity/issues/871), por lo que no es posible utilizar el portaudio del sistema.
3. **vamp-host-sdk**: Los paquetes de desarrollo no están disponibles en Ubuntu 20.04.
4. **libnyquist** & **portmixer**: Los paquetes de desarrollo no están disponibles en Ubuntu 20.04.
5. **sqlite3** & **libsmbs**: Las bibliotecas están muy desactualizadas en Ubuntu 20.04.

No se aconseja mezclar bibliotecas del sistema y locales, excepto en los casos mencionados anteriormente. ZLib es una dependencia muy común; es posible mezclar bibliotecas del sistema y locales en una construcción. Sin embargo, no se recomienda hacerlo.
Hay un archivo [`Dockerfile`](linux/build-environment/Dockerfile) que se puede utilizar como ejemplo de cómo construir Audacity utilizando bibliotecas del sistema:

```
$ docker build -t audacity_linux_env .\linux\build-environment\
$ docker run --rm -v ${pwd}:/audacity/audacity/ -v ${pwd}/../build/linux-system:/audacity/build -it audacity_linux_env
```

Para encontrar paquetes del sistema, dependemos de `pkg-config`. Hay varios paquetes que tienen archivos `*.pc` dañados o no utilizan `pkg-config` en absoluto. Para la imagen de Docker, manejamos este problema instalando los archivos [`pc` correctos](linux/build-environment/pkgconfig/).

### Deshabilitando Conan

Conan se puede deshabilitar completamente utilizando `-Daudacity_conan_enabled=Off` durante la configuración.
Esta opción implica `-Daudacity_obey_system_dependencies=On` y deshabilita `local` para los paquetes que se administran con Conan.

### Deshabilitando descargas de binarios preconstruidos para Conan

Es posible forzar a Conan a construir todas las dependencias desde el código fuente sin utilizar los binarios preconstruidos. Para hacerlo, pase `-Daudacity_conan_allow_prebuilt_binaries=Off` a CMake durante la configuración.

Además, pasar `-Daudacity_conan_force_build_dependencies=On`forzará a Conan a reconstruir *todos* los paquetes durante cada configuración. Esto puede ser útil para las compilaciones offline contra la caché de descargas de Conan.

### Perfiles de Conan personalizados

Audacity intentará detectar los perfiles de host y build automáticamente. Sin embargo, las opciones de CMake, `audacity_conan_host_profile` y `audacity_conan_build_profile` se pueden utilizar para especificar perfiles de Conan personalizados.

### Solucionando problemas de Conan

Para solucionar errores similares a los siguientes:

```
ERROR: HTTPSConnectionPool(host='center.conan.io', port=443): Se excedieron los intentos máximos con la URL: /v1/ping (Causado por SSLError(SSLCertVerificationError(1, '[SSL: CERTIFICATE_VERIFY_FAILED] verificación del certificado fallida: el certificado ha expirado (_ssl.c:1131)
```

por favor, actualice su versión de Conan. Alternativamente, el problema se puede solucionar utilizando:

```
$ conan config install https://github.com/conan-io/conanclientcert.git
```

Para errores como:

```
No se puede conectar a conan-center=https://conan.bintray.com
```

Por favor, ejecute

```
conan remote remove conan-center
```

Para errores como:

```
[paquete] nombre_paquete/versión_paquete: el paquete tiene 'exports_sources' pero no se encuentran las fuentes
```

Por favor, ejecute

```
conan remove nombre_paquete -c
```
### Reduciendo el tamaño de la caché de Conan

Para reducir el espacio utilizado por la caché de Conan, por favor ejecute:

```
$ conan cache clean "*"
```

### Seleccionando la arquitectura de destino en macOS

A partir de la versión 3.2.0, Audacity permite seleccionar la arquitectura de destino en macOS pasando `MACOS_ARCHITECTURE` a CMake durante la configuración.

Para compilar para Intel:

```
$ cmake -GXcode -DMACOS_ARCHITECTURE=x86_64 ../audacity
```

Para compilar para AppleSilicon:

```
$ cmake -GXcode -DMACOS_ARCHITECTURE=arm64 ../audacity

```

La arquitectura de compilación predeterminada se selecciona en base al valor de CMAKE_HOST_SYSTEM_PROCESSOR.

Al compilar cruzado desde Intel a AppleSilicon, o si *Rosetta 2* no está instalado en la Mac AppleSilicon,
se requiere un directorio de compilación de versión nativa de Audacity, ya que Audacity necesita un compilador de imágenes funcionando.

Por ejemplo, para compilar la versión ARM64 de Audacity en Mac Intel:

```
$ mkdir build.x64
$ cmake -GXcode -DMACOS_ARCHITECTURE=x86_64 -B build.x64 -S ../audacity
$ cmake --build build.x64 --config Release --target image-compiler
$ mkdir build.arm64
$ cmake -GXcode -DMACOS_ARCHITECTURE=arm64 -DIMAGE_COMPILER_EXECUTABLE=build.x64/utils/RelWithDebInfo/image-compiler -B build.arm64 -S ../audacity
$ cmake --build build.arm64 --config Release
```

Esto colocará la versión ARM64 en build.arm64/Release/.

### Compilando con VST3SDK sin Conan (solo Linux)

Establezca una de las siguientes variables de entorno en la ruta al SDK VST3 (es decir, la carpeta que contiene la carpeta `pluginterfaces`):

* `VST3_SDK_DIR`
* `VST3SDK_PATH`
* `VST3SDK`

o copie el SDK VST3 a la carpeta vst3sdk en el árbol de fuentes de Audacity.

Pase `-Daudacity_use_vst3sdk=system` a CMake. CMake compilará el SDK durante la configuración.
