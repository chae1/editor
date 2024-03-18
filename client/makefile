CC = g++
CXXFLAGS = -std=c++2a -Wall
INCLUDES =
LIBRARIES =
LFLAGS = 

FONT_DIR = ./src/font
SOCKET_DIR = ./src/socket
ENGINE_DIR = ./src/vulkan

all : client $(ENGINE_DIR)/vert.spv $(ENGINE_DIR)/frag.spv

client : client.cpp $(FONT_DIR)/font.o $(SOCKET_DIR)/socket_client.o $(ENGINE_DIR)/engine.o
	$(CC) $(CXXFLAGS) -g $< -o $@ $(FONT_DIR)/font.o $(SOCKET_DIR)/socket_client.o $(ENGINE_DIR)/engine.o -lglfw -lvulkan -lfmt

$(FONT_DIR)/font.o : $(FONT_DIR)/font.cpp $(FONT_DIR)/font.h
	$(CC) $(CXXFLAGS) -g -c $< -o $(FONT_DIR)/font.o -lfmt

$(SOCKET_DIR)/socket_client.o : $(SOCKET_DIR)/socket_client.cpp $(SOCKET_DIR)/socket_client.h
	$(CC) $(CXXFLAGS) -g -c $< -o $(SOCKET_DIR)/socket_client.o -lfmt

$(ENGINE_DIR)/engine.o : $(ENGINE_DIR)/engine.cpp $(ENGINE_DIR)/engine.h
	$(CC) $(CXXFLAGS) -g -c $< -o $(ENGINE_DIR)/engine.o -lglfw -lvulkan -lfmt

$(ENGINE_DIR)/vert.spv : $(ENGINE_DIR)/shader.vert
	glslc $(ENGINE_DIR)/shader.vert -o $(ENGINE_DIR)/vert.spv

$(ENGINE_DIR)/frag.spv : $(ENGINE_DIR)/shader.frag
	glslc $(ENGINE_DIR)/shader.frag -o $(ENGINE_DIR)/frag.spv
