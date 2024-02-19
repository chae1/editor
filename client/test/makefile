CC = g++
CXXFLAGS = -std=c++2a
INCLUDES =
LIBRARIES =
LFLAGS =

SRCS = $(wildcard *.cpp)
OBJS = $(SRCS:.cpp=.exe)

all : $(OBJS)

%.exe : %.cpp
	$(CC) $(CXXFLAGS) $< -o $@ $(INCLUDES) $(LIBRARIES) $(LFLAGS)
