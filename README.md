# editor
A GPU rendering text editor client and a server for simultaneous text editing.\
<img src="https://github.com/chae1/editor/assets/29856486/381a0c16-9729-460f-9a82-04df2de4760d" width="350">

## How to run
### Windows
Following commands are for cmd. Use equivalent commands if you use other shells.
#### Client
```
client.exe 127.0.0.1:20741 YourName
```
#### Server

```
server.exe 127.0.0.1:20741
```
### Ubuntu
#### Client
```
./client 127.0.0.1:20741 YourName
```
#### Server
```
./server 127.0.0.1:20741
```

## How to build
### Prerequisite
1. Install vulkan sdk https://vulkan.lunarg.com/sdk/home.
2. Install sbcl https://www.sbcl.org/platform-table.html.
3. Install quicklisp https://www.quicklisp.org/beta/.

### Windows
Following commands are for cmd. Use equivalent commands if you use other shells.
#### Client
1. Install ucrt mingw toolchain in msys2 following https://code.visualstudio.com/docs/cpp/config-mingw.
2. Move to client/build/ in cmd.
```console
cmake .. -G "MinGW Makefiles"
cmake --build . -j %NUMBER_OF_PROCESSORS%
```
3. client.exe will be built in client/build/.
#### Server
1. Move to server/ in console.
```console
sbcl --load load.lisp
```
2. server.exe will be built in server/.
### Ubuntu
#### Client
1. move to client/build/ in terminal.
```console
cmake .. -G "Unix Makefiles"
cmake --build . -j $(nproc)
```
2. client.exe will be built in client/build/.
#### Server
1. Move to server/ in terminal.
```console
sbcl --load load.lisp
```
2. server.exe will be built in server/.

## Details
* GPU text rendering is implemented using the method shown in "Eric Lengyel, GPU-Centered Font Rendering Directly from Glyph Outlines, Journal of Computer Graphics Techniques (JCGT), vol. 6, no. 2, 31-47, 2017".
  
