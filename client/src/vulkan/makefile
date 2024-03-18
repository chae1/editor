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

engine_test: engine_test.cpp engine.cpp engine.h ../font/font.cpp ../font/font.h ../socket/socket_client.cpp ../socket/socket_client.h vert.spv frag.spv
	$(CC) $(CXXFLAGS) -g -o $@ -g engine_test.cpp -g engine.cpp -g ../font/font.cpp -g  ../socket/socket_client.cpp $(LFLAGS)

vert.spv: shader.vert
	glslc shader.vert -o vert.spv

frag.spv: shader.frag
	glslc shader.frag -o frag.spv
