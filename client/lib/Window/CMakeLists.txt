add_library(window STATIC glfw_window.cpp)
target_include_directories(window INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(window PUBLIC glfw)
