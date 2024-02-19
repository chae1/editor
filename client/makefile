CC = g++
CXXFLAGS = -std=c++17 -Wall -g
VULKAN_INCLUDE = -I"c:/Users/kcwch/Desktop/bin/VulkanSDK/1.3.224.1/Include/" -I"c:/Users/kcwch/Desktop/bin/glfw-3.3.8.bin.WIN64/include/" -I"c:/Users/kcwch/Desktop/bin/glm/" -I"c:/Users/kcwch/Desktop/bin/stb/" -I"c:/Users/kcwch/Desktop/bin/tinyobjloader/" -L"C:\Users\kcwch\Desktop\bin\VulkanSDK\1.3.224.1\Lib" -L"C:\Users\kcwch\Desktop\bin\glfw-3.3.8.bin.WIN64\lib-mingw-w64" -lglfw3 -lvulkan-1

SRC_DIR = ./src
SRC_NAMES = $(notdir $(SRCS))
SRCS = $(wildcard $(SRC_DIR)/*.cpp)

OBJ_DIR = ./obj
OBJ_NAMES = $(SRC_NAMES:.cpp=.o)
OBJS = $(patsubst %.o, $(OBJ_DIR)/%.o, $(OBJ_NAMES))
DEPS = $(OBJS:.o=.d)

VULKAN_OBJ_NAMES = renderer.o
VULKAN_OBJS = $(patsubst %.o, $(OBJ_DIR)/%.o, $(OBJ_NAMES))

OTHER_OBJS = $(filter-out $(VULKAN_OBJS), $(OBJS))

TARGET = client
TARGET_SRC = $(TARGET).cpp
TARGET_OBJ = $(OBJ_DIR)/$(TARGET).o
TARGET_EXE = $(TARGET).exe

.PHONY: all test
all: $(TARGET_EXE)

$(TARGET_EXE): $(TARGET_OBJ) $(OBJS)
	$(CC) $(CXXFLAGS) $^ -o $@

$(VULKAN_OBJS): $(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	$(CC) $(CXXFLAGS) -c $^ -o $@ -MD $(VULKAN_INCLUDE)

$(OTHER_OBJS): $(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	$(CC) $(CXXFLAGS) -c $^ -o $@ -MD

-include $(DEPS)

.PHONY: clean
clean:
	rm -f $(TARGET_EXE) $(OBJS) $(DEPS)

TEST_DIR = ./test
TEST_SRCS = $(wildcard $(TEST_DIR)/*.cpp)
TEST_EXES = $(TEST_SRCS:.cpp=.exe)
TEST_DEPS = $(TEST_SRCS:.cpp=.d)

.PHONY: test
test: $(TEST_EXES)

$(TEST_DIR)/%.exe: $(TEST_DIR)/%.cpp
	$(CC) $(CXXFLAGS) -o $@ -MD

-include $(TEST_DEPS)

.PHONY: clean_test
clean_test:
	rm -f $(TEST_EXES) $(TEST_DEPS)
