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

# Include any dependencies generated for this target.
include lib/Window/CMakeFiles/window.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include lib/Window/CMakeFiles/window.dir/compiler_depend.make

# Include the progress variables for this target.
include lib/Window/CMakeFiles/window.dir/progress.make

# Include the compile flags for this target's objects.
include lib/Window/CMakeFiles/window.dir/flags.make

lib/Window/CMakeFiles/window.dir/glfw_window.cpp.o: lib/Window/CMakeFiles/window.dir/flags.make
lib/Window/CMakeFiles/window.dir/glfw_window.cpp.o: /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/Window/glfw_window.cpp
lib/Window/CMakeFiles/window.dir/glfw_window.cpp.o: lib/Window/CMakeFiles/window.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object lib/Window/CMakeFiles/window.dir/glfw_window.cpp.o"
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/Window && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT lib/Window/CMakeFiles/window.dir/glfw_window.cpp.o -MF CMakeFiles/window.dir/glfw_window.cpp.o.d -o CMakeFiles/window.dir/glfw_window.cpp.o -c /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/Window/glfw_window.cpp

lib/Window/CMakeFiles/window.dir/glfw_window.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing CXX source to CMakeFiles/window.dir/glfw_window.cpp.i"
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/Window && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/Window/glfw_window.cpp > CMakeFiles/window.dir/glfw_window.cpp.i

lib/Window/CMakeFiles/window.dir/glfw_window.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling CXX source to assembly CMakeFiles/window.dir/glfw_window.cpp.s"
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/Window && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/Window/glfw_window.cpp -o CMakeFiles/window.dir/glfw_window.cpp.s

# Object files for target window
window_OBJECTS = \
"CMakeFiles/window.dir/glfw_window.cpp.o"

# External object files for target window
window_EXTERNAL_OBJECTS =

lib/Window/libwindow.a: lib/Window/CMakeFiles/window.dir/glfw_window.cpp.o
lib/Window/libwindow.a: lib/Window/CMakeFiles/window.dir/build.make
lib/Window/libwindow.a: lib/Window/CMakeFiles/window.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --bold --progress-dir=/home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX static library libwindow.a"
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/Window && $(CMAKE_COMMAND) -P CMakeFiles/window.dir/cmake_clean_target.cmake
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/Window && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/window.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
lib/Window/CMakeFiles/window.dir/build: lib/Window/libwindow.a
.PHONY : lib/Window/CMakeFiles/window.dir/build

lib/Window/CMakeFiles/window.dir/clean:
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/Window && $(CMAKE_COMMAND) -P CMakeFiles/window.dir/cmake_clean.cmake
.PHONY : lib/Window/CMakeFiles/window.dir/clean

lib/Window/CMakeFiles/window.dir/depend:
	cd /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/lib/Window /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/Window /home/chaewon/Desktop/chae1/editor/client/test/client-cmake-file-structure/build/lib/Window/CMakeFiles/window.dir/DependInfo.cmake "--color=$(COLOR)"
.PHONY : lib/Window/CMakeFiles/window.dir/depend

