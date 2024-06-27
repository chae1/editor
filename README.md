# editor
A GPU rendering client program and a server program for shared text editing.\
<img src="https://github.com/chae1/editor/assets/29856486/381a0c16-9729-460f-9a82-04df2de4760d" width="350">

## Prerequisite
Install vulkan sdk on your platform following docs in https://vulkan.lunarg.com/sdk/home\

## Client
### How to build
#### Windows
1. Install ucrt mingw toolchain in msys2 following https://code.visualstudio.com/docs/cpp/config-mingw
2. Move to client directory in cmd
```console
mkdir build
cd ./build
cmake .. -G "MinGW Makefiles"
cmake --build . -j %NUMBER_OF_PROCESSORS%
```
## Ubuntu
## How to build
build client\
move to client directory
```console
mkdir build
cd ./build
cmake .. -j $(nproc)
cmake --build . -j $(nproc)
```
build server
```console
```
## How to run
run client
```console
./client ip:port username
```
run server
```console
```

## Details
* Text rendering is implemented using the method shown in "Eric Lengyel, GPU-Centered Font Rendering Directly from Glyph Outlines, Journal of Computer Graphics Techniques (JCGT), vol. 6, no. 2, 31-47, 2017".
  
