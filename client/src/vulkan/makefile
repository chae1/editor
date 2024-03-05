CC = g++
CXXFLAGS = -std=c++2a -Wall
INCLUDES =
LIBRARIES =
LFLAGS = -lglfw -lvulkan -lfmt

# SRCS = $(wildcard *.cpp)
# OBJS = $(SRCS:.cpp=.o)

all: main engine_test

main: main.cpp
	$(CC) $(CXXFLAGS) $< -o $@ $(INCLUDES) $(LIBRARIES) $(LFLAGS)

engine_test: engine_test.cpp engine.cpp engine.h ../font/font.cpp ../font/font.h vert.spv frag.spv
	$(CC) $(CXXFLAGS) -g -o $@ engine_test.cpp engine.cpp engine.h ../font/font.cpp $(LFLAGS)

vert.spv: shader.vert
	glslc shader.vert -o vert.spv

frag.spv: shader.frag
	glslc shader.frag -o frag.spv
