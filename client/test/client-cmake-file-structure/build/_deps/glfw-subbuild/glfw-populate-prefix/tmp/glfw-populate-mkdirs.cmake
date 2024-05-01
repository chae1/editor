# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-src"
  "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-build"
  "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-subbuild/glfw-populate-prefix"
  "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-subbuild/glfw-populate-prefix/tmp"
  "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-subbuild/glfw-populate-prefix/src/glfw-populate-stamp"
  "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-subbuild/glfw-populate-prefix/src"
  "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-subbuild/glfw-populate-prefix/src/glfw-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-subbuild/glfw-populate-prefix/src/glfw-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/_deps/glfw-subbuild/glfw-populate-prefix/src/glfw-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
