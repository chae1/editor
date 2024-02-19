.PHONY : glfw_test.exe

all : glfw_test.exe key_callback.cpp key_callback.h glfw_window.cpp glfw_window.h
	g++ -std=c++2a -Wall -g -o glfw_test.exe glfw_test.cpp glfw_window.cpp key_callback.cpp -L /usr/lib/x86_64-linux-gnu/ -lglfw
