#pragma once

#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

#include <vector>
#include <chrono>
#include <map>
#include <functional>
#include <mutex>
#include <atomic>
#include <memory>

namespace key_callback {
    typedef int Key;

    // Current key_combi will invoke a function to do corresponding action.
    // key_combi maps to a key_combi_code which will be used as a synonym for the corresponding key_combi.
    // Only key_combi_code will be accesed instead of key_combi.

    class Key_combi_code_map {
    public:
        int get_key_combi_code(const std::vector<Key>& key_combi);
        void init_key_combi_code(const std::vector<Key>& key_combi);
	
    private:
        std::map<std::vector<Key>, int> key_combi_code_map;
        int new_key_combi_code = 0;
    };
   
    class Shared_key_combi_code {
    public:
	void update_key_combi_code(const std::vector<Key>& key_combi); // atomically update key_combi_code
	int get_key_combi_code(); // atomically get key_combi_code
	
    private:
	Key_combi_code_map key_combi_code_map;
	std::atomic<int> key_combi_code; // key_combi_Code_map must be preceded.
	
	friend struct Key_callback_env;
    };

    template <typename K>
    class Function_map {
    public:
        void invoke_func(const K& key);
        void init_func(const K& key, const std::function<void()>& func);

    private:
        std::map<K, std::function<void()>> func_map;
    };

    template <typename K>
    class Predicate_map {
    public:
	bool get_predicate(K key);
	void set_predicate(K key, bool val);

    private:	
	std::map<K, std::unique_ptr<std::atomic<bool>>> predicate_ptr_map;
    };
    
    enum class State { normal, control, shift, alt };
    using enum State;
    
    struct Key_callback_env {
	static std::vector<Key> key_combi;	
	static Shared_key_combi_code shared_key_combi_code;

	static void update_key_combi_code();
	static int get_key_combi_code();
	static void print_state();
	
	static State state;
        static Key_combi_code_map& key_combi_code_map; // reference to Shared_key_combi member
	
	// key_combi and state will be modified by transition
        static Function_map<Key> key_press_invoked_transition_map;
        static Function_map<Key> key_release_invoked_transition_map;

	// function mapped with current_key_combi code as key will be invoked
	static Function_map<int> key_combi_invoked_function_map;
	static Predicate_map<std::vector<Key>> key_combi_thread_running_predicate_map;

        static void key_callback(GLFWwindow* window, Key key, int scancode, int action, int mods);	
	static void init_key_callback_env();
    };
}
