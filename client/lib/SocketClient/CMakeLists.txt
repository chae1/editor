message("${CMAKE_CURRENT_BINARY_DIR}")

add_library(socketClient STATIC socket_client.cpp)
target_include_directories(socketClient INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(socketClient PUBLIC fmt)

if(CMAKE_SYSTEM MATCHES Windows)
  target_link_libraries(socketClient PRIVATE wsock32 ws2_32)
endif()

if(CMAKE_HOST_SYSTEM MATCHES Linux)
  add_compile_definitions(LINUX)
endif()
