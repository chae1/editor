CC = g++
CXXFLAGS = -std=c++2a -Wall -g
INCLUDES =
LIBRARIES =
LFLAGS = -lglfw -lvulkan

# SRCS = $(wildcard *.cpp)
# OBJS = $(SRCS:.cpp=.o)

all : main engine

main : main.cpp
	$(CC) $(CXXFLAGS) $< -o $@ $(INCLUDES) $(LIBRARIES) $(LFLAGS)

engine : engine_test.cpp engine_init.cpp engine.h
	g++ -std=c++2a -o engine_test engine_test.cpp engine_init.cpp ../glfw/glfw_window.cpp -lglfw -lvulkan
