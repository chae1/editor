remove_definitions("-DNDEBUG")

add_library(engine STATIC engine.cpp)
target_include_directories(engine INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})

find_package(Vulkan REQUIRED)

# link personal libraries
target_link_libraries(engine PUBLIC fontInfo socketClient window fmt glfw)
target_link_libraries(engine PRIVATE Vulkan::Vulkan)

message("${CMAKE_CURRENT_BINARY_DIR}")

add_custom_command(
  OUTPUT
  ${CMAKE_CURRENT_BINARY_DIR}/vert.spv
  ${CMAKE_CURRENT_BINARY_DIR}/frag.spv

  COMMAND glslc -o ${CMAKE_CURRENT_BINARY_DIR}/vert.spv ${CMAKE_CURRENT_SOURCE_DIR}/shader.vert
  COMMAND glslc -o ${CMAKE_CURRENT_BINARY_DIR}/frag.spv ${CMAKE_CURRENT_SOURCE_DIR}/shader.frag

  DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/shader.vert
  ${CMAKE_CURRENT_SOURCE_DIR}/shader.frag

  COMMENT "Compiling shaders")

add_custom_target(compileShaders
  DEPENDS
  ${CMAKE_CURRENT_BINARY_DIR}/vert.spv
  ${CMAKE_CURRENT_BINARY_DIR}/frag.spv)

add_dependencies(engine compileShaders)
