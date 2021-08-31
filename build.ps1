# Build Script
# Derived from https://github.com/lockie/racket-engine/blob/master/build.ps1
# Thanks to @lockie!

Set-PSDebug -Trace 1

$ErrorActionPreference = "Stop"

& "C:\Program Files\Racket\raco.exe" exe --gui zodetrip.rkt
& "C:\Program Files\Racket\raco.exe" distribute ZT zodetrip.exe
# Copy-Item assets -Destination ZT\ -Recurse -Force

Invoke-WebRequest -OutFile SDL2.zip https://www.libsdl.org/release/SDL2-2.0.14-win32-x64.zip
expand-archive -path SDL2.zip -destinationpath ZT -Force
Move-Item -Path ZT\README-SDL.txt ZT\LICENSE.SDL2.txt -Force

# Invoke-WebRequest -OutFile SDL2_image.zip https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.4-win32-x86.zip
# expand-archive -path SDL2_image.zip -destinationpath ZT -Force
# Move-Item -Path ZT\SDL2_image.dll ZT\libSDL2_image.dll -Force
# Move-Item -Path ZT\README.txt ZT\LICENSE.SDL2_image.txt -Force

Invoke-WebRequest -OutFile SDL2_mixer.zip https://www.libsdl.org/projects/SDL_mixer/release/SDL2_mixer-2.0.4-win32-x64.zip
expand-archive -path SDL2_mixer.zip -destinationpath ZT -Force
# Move-Item -Path ZT\SDL2_mixer.dll ZT\libSDL2_mixer.dll -Force
Move-Item -Path ZT\README.txt ZT\LICENSE.SDL2_mixer.txt -Force

# Invoke-WebRequest -OutFile SDL2_ttf.zip https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-2.0.15-win32-x86.zip
# expand-archive -path SDL2_ttf.zip -destinationpath ZT -Force
# Move-Item -Path ZT\SDL2_ttf.dll ZT\libSDL2_ttf.dll -Force
# Move-Item -Path ZT\README.txt ZT\LICENSE.SDL2_ttf.txt -Force

compress-archive -path ZT -destinationpath zodetrip.zip -compressionlevel optimal -Force