CC = g++
CXXFLAGS = -std=c++2a -Wall -g
INCLUDES =
LIBRARIES =
LFLAGS = -lfmt

SRCS = $(wildcard *.cpp)
OBJS = $(SRCS:.cpp=.exe)

all : $(OBJS)

%.exe : %.cpp
	$(CC) $(CXXFLAGS) $< -o $@ $(INCLUDES) $(LIBRARIES) $(LFLAGS)
