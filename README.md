# editor
A GPU rendering client program and a server program for shared text editing.\
<img src="https://github.com/chae1/editor/assets/29856486/381a0c16-9729-460f-9a82-04df2de4760d" width="300">

## Prerequisite
c++ 20\
vulkan\
cmake

## How to build


### Ubuntu
run client
```console
sudo apt update
sudo apt-get install libglfw3-dev
sudo apt-get install libvulkan-dev
sudo apt-get install libfmt-dev
./client ip:port username
```

## How to develop
### Ubuntu
build client
```console
sudo apt install libglm-dev
```

## Details
* Text rendering is implemented using the method shown in "Eric Lengyel, GPU-Centered Font Rendering Directly from Glyph Outlines, Journal of Computer Graphics Techniques (JCGT), vol. 6, no. 2, 31-47, 2017".
  
