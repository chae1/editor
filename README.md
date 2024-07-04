# editor
A GPU rendering text editor client and a server for simultaneous text editing.\
<img src="https://github.com/chae1/editor/assets/29856486/381a0c16-9729-460f-9a82-04df2de4760d" width="350">

## How to run
Clone this project, then you can run the prebuilt server and client programs placed in editor/server/ and editor/client/build/. You can also build yourself then run them if you want. Your gpu driver should support vulkan to run the client program.
### Windows
Following commands are for cmd. Use equivalent commands if you use other shells.
#### Client
```
client.exe 127.0.0.1:100 YourName
```
#### Server
```
server.exe 127.0.0.1:100
```
### Ubuntu
#### Client
```
./client 127.0.0.1:100 YourName
```
#### Server
```
./server 127.0.0.1:100
```

## How to build
### Prerequisite
1. Install cmake.
2. Install vulkan sdk https://vulkan.lunarg.com/sdk/home.
3. Install sbcl https://www.sbcl.org/platform-table.html.
4. Install quicklisp https://www.quicklisp.org/beta/.

### Windows
Following commands are for cmd. Use equivalent commands if you use other shells.
#### Client
1. Install ucrt mingw toolchain in msys2 following https://code.visualstudio.com/docs/cpp/config-mingw.
2. Move to client/build/ in cmd.
3. Build.
```console
cmake .. -G"MinGW Makefiles" -DCMAKE_BUILD_TYPE="Release"
```
or
```console
cmake .. -G"MinGW Makefiles" -DCMAKE_BUILD_TYPE="Debug"
```
then,
```console
cmake --build . -j %NUMBER_OF_PROCESSORS%
```
5. client.exe will be built in client/build/.
#### Server
1. Move to server/ in console.
```console
sbcl --load load.lisp
```
2. server.exe will be built in server/.
### Ubuntu
#### Client
1. Move to client/build/ in terminal.
2. Build.
```console
cmake .. -G"Unix Makefiles" -DCMAKE_BUILD_TYPE="Release"
```
or
```console
cmake .. -G"Unix Makefiles" -DCMAKE_BUILD_TYPE="Debug"
```
then,
```console
cmake --build . -j $(nproc)
```
4. client.exe will be built in client/build/.
#### Server
1. Move to server/ in terminal.
```console
sbcl --load load.lisp
```
2. server.exe will be built in server/.

## Details
* GPU text rendering is implemented using the method shown in "Eric Lengyel, GPU-Centered Font Rendering Directly from Glyph Outlines, Journal of Computer Graphics Techniques (JCGT), vol. 6, no. 2, 31-47, 2017".
  
