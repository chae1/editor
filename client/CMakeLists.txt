cmake_minimum_required(VERSION 3.25)

project(
  EditorClient
  VERSION 0.1
  DESCRIPTION "Vulkan 3d text rendering client for shared text editor."
  LANGUAGES CXX
)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall")

if(CMAKE_SYSTEM MATCHES Windows)
  message(STATUS "Target system is Windows")
  add_compile_definitions(WINDOWS)
endif()

if(CMAKE_HOST_SYSTEM MATCHES Linux)
  message(STATUS "Build host runs Linux")
  add_compile_definitions(LINUX)
endif()

# external libraries
include(FetchContent)
FetchContent_Declare(
  fmt
  GIT_REPOSITORY "https://github.com/fmtlib/fmt"
  GIT_TAG master
)
FetchContent_MakeAvailable(fmt)

FetchContent_Declare(
  glm
  GIT_REPOSITORY "https://github.com/g-truc/glm"
  GIT_TAG master
)
FetchContent_MakeAvailable(glm)

FetchContent_Declare(
  glfw
  GIT_REPOSITORY "https://github.com/glfw/glfw"
  GIT_TAG master
)
FetchContent_MakeAvailable(glfw)

add_subdirectory(lib/FontInfo)
add_subdirectory(lib/SocketClient)
add_subdirectory(lib/Window)
add_subdirectory(lib/RenderingEngine)

add_executable(client client.cpp)
target_link_libraries(client PRIVATE engine socketClient)

if(CMAKE_SYSTEM MATCHES Windows)
  target_link_options(client PRIVATE -static -static-libgcc -static-libstdc++)
  add_compile_definitions(WINDOWS)
endif()
