add_library(socketClient STATIC socket_client.cpp)
target_include_directories(socketClient INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(socketClient PUBLIC fmt)
