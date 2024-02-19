CC = g++
CXXFLAGS = -std=c++2a -Wall -g
INCLUDES =
LIBRARIES =
LFLAGS = -lglfw -lvulkan -lfmt

# SRCS = $(wildcard *.cpp)
# OBJS = $(SRCS:.cpp=.o)

test : font_test.exe

font_test.exe : font_test.cpp font.cpp font.h
	$(CC) $(CXXFLAGS) -o font_test.exe font_test.cpp font.h font.cpp $(INCLUDES) $(LIBRARIES) $(LFLAGS)
