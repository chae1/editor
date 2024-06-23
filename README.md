# editor
A text editor

## prerequisites
c++ 20
install vulkan
install cmake

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
* Vulkan GPU text rendering is implemented using the method shown in "Eric Lengyel, GPU-Centered Font Rendering Directly from Glyph Outlines, Journal of Computer Graphics Techniques (JCGT), vol. 6, no. 2, 31-47, 2017".
* 
