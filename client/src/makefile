.PHONY : client.exe

all : client.exe client.cpp ./glfw/glfw_window.cpp ./glfw/glfw_window.h ./socket/socket_client.cpp ./socket/socket_client.h
	g++ -std=c++2a -Wall -g -o client.exe client.cpp ./glfw/glfw_window.cpp ./socket/socket_client.cpp -L /usr/lib/x86_64-linux-gnu/ -lglfw
