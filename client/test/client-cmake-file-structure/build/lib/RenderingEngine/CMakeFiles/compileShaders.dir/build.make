# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.29

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /home/chaewon/Desktop/program/cmake-3.29.2-linux-x86_64/bin/cmake

# The command to remove a file.
RM = /home/chaewon/Desktop/program/cmake-3.29.2-linux-x86_64/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build

# Utility rule file for compileShaders.

# Include any custom commands dependencies for this target.
include lib/RenderingEngine/CMakeFiles/compileShaders.dir/compiler_depend.make

# Include the progress variables for this target.
include lib/RenderingEngine/CMakeFiles/compileShaders.dir/progress.make

lib/RenderingEngine/CMakeFiles/compileShaders: lib/RenderingEngine/vert.spv
lib/RenderingEngine/CMakeFiles/compileShaders: lib/RenderingEngine/frag.spv

lib/RenderingEngine/vert.spv: /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/RenderingEngine/shader.vert
lib/RenderingEngine/vert.spv: /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/RenderingEngine/shader.frag
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --blue --bold --progress-dir=/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Compiling shaders"
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/RenderingEngine && glslc -o /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/RenderingEngine/vert.spv /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/RenderingEngine/shader.vert
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/RenderingEngine && glslc -o /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/RenderingEngine/frag.spv /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/RenderingEngine/shader.frag

lib/RenderingEngine/frag.spv: lib/RenderingEngine/vert.spv
	@$(CMAKE_COMMAND) -E touch_nocreate lib/RenderingEngine/frag.spv

compileShaders: lib/RenderingEngine/CMakeFiles/compileShaders
compileShaders: lib/RenderingEngine/frag.spv
compileShaders: lib/RenderingEngine/vert.spv
compileShaders: lib/RenderingEngine/CMakeFiles/compileShaders.dir/build.make
.PHONY : compileShaders

# Rule to build all files generated by this target.
lib/RenderingEngine/CMakeFiles/compileShaders.dir/build: compileShaders
.PHONY : lib/RenderingEngine/CMakeFiles/compileShaders.dir/build

lib/RenderingEngine/CMakeFiles/compileShaders.dir/clean:
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/RenderingEngine && $(CMAKE_COMMAND) -P CMakeFiles/compileShaders.dir/cmake_clean.cmake
.PHONY : lib/RenderingEngine/CMakeFiles/compileShaders.dir/clean

lib/RenderingEngine/CMakeFiles/compileShaders.dir/depend:
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/RenderingEngine /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/RenderingEngine /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/RenderingEngine/CMakeFiles/compileShaders.dir/DependInfo.cmake "--color=$(COLOR)"
.PHONY : lib/RenderingEngine/CMakeFiles/compileShaders.dir/depend

