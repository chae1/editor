# editor
A GPU rendering client program and a server program for shared text editing.\
<img src="https://github.com/chae1/editor/assets/29856486/381a0c16-9729-460f-9a82-04df2de4760d" width="350">

## Prerequisite
Install vulkan sdk on your platform following docs in https://vulkan.lunarg.com/sdk/home.

## Windows
### Client
#### How to run
```
client.exe 127.0.0.1:20741 YourName
```
#### How to build
1. Install ucrt mingw toolchain in msys2 following https://code.visualstudio.com/docs/cpp/config-mingw.
2. Move to client directory in console.
```console
mkdir build
cd ./build
cmake .. -G "MinGW Makefiles"
cmake --build . -j %NUMBER_OF_PROCESSORS%
```
3. client.exe should have been built in build directory.
### Server
#### How to run
```
server.exe 127.0.0.1:20741
```
#### How to build
1. Install sbcl https://www.sbcl.org/platform-table.html.
2. Install quicklisp https://www.quicklisp.org/beta/.
3. Move to server directory in console.
```console
sbcl --load load.lisp
```
4. server.exe should have been built in server directory.
## Ubuntu
### Client
#### How to build
move to client directory in shell
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
  
