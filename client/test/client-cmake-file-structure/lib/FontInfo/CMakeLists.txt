add_library(fontInfo STATIC fontInfo.cpp)
target_include_directories(fontInfo INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(fontInfo PUBLIC glm fmt)
