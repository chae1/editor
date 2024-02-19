#include "key_callback.h"
#include <chrono>
#include <thread>
#include <iostream>
#include <iterator>

using namespace key_callback;

inline int Key_combi_code_map::get_key_combi_code(const std::vector<Key> &key_combi) {
    if (key_combi_code_map.contains(key_combi)) {
        return key_combi_code_map[key_combi];
    } else {
	std::cout << "key_combi_code for key-combi doesn't exist." << std::flush;
        return -1;
    }
}

inline void Key_combi_code_map::init_key_combi_code(const std::vector<Key> &key_combi) {
    if (key_combi_code_map.contains(key_combi)) {
	std::cout << "key_combi_code for key_combi already exists." << std::flush;
    } else {
	key_combi_code_map[key_combi] = new_key_combi_code++;
    }
}

void Shared_key_combi_code::update_key_combi_code(const std::vector<Key>& key_combi) {
    key_combi_code.store(key_combi_code_map.get_key_combi_code(key_combi));
}

int Shared_key_combi_code::get_key_combi_code() {
    return key_combi_code.load();
}

template <typename K>
inline void Function_map<K>::invoke_func(const K& key) {
    if (func_map.contains(key)) {
        func_map[key]();
    } else {
        std::cout << "function not found" << std::flush;
    }
}

template <typename K>
inline void Function_map<K>::init_func(const K& key, const std::function<void()>& func) {
    if (func_map.contains(key)) {
        std::cout << "func for key already exists." << std::flush;
    } else {
	func_map[key] = func;
    }
}

template <typename K>
bool Predicate_map<K>::get_predicate(K key) {
    if (Predicate_map<K>::predicate_ptr_map.contains(key)) {
	return Predicate_map<K>::predicate_ptr_map[key]->load();
    } else {
	Predicate_map<K>::predicate_ptr_map[key] = std::unique_ptr<std::atomic<bool>>(new std::atomic<bool>(false));
	return false;
    }
}

// get_predicate must be preceded.
template <typename K>
void Predicate_map<K>::set_predicate(K key, bool val) {
    Predicate_map<K>::predicate_ptr_map[key]->store(false);
}

// Key_callback_env variable
std::vector<Key> Key_callback_env::key_combi = { };
Shared_key_combi_code Key_callback_env::shared_key_combi_code;
State Key_callback_env::state = normal;
Key_combi_code_map& Key_callback_env::key_combi_code_map = Key_callback_env::shared_key_combi_code.key_combi_code_map;
Function_map<Key> Key_callback_env::key_press_invoked_transition_map;
Function_map<Key> Key_callback_env::key_release_invoked_transition_map;
Function_map<int> Key_callback_env::key_combi_invoked_function_map;
Predicate_map<std::vector<Key>> Key_callback_env::key_combi_thread_running_predicate_map;

// helper
void Key_callback_env::update_key_combi_code() {
    Key_callback_env::shared_key_combi_code.update_key_combi_code(Key_callback_env::key_combi);
}

int Key_callback_env::get_key_combi_code() {
    return Key_callback_env::key_combi_code_map.get_key_combi_code(Key_callback_env::key_combi);
}

void Key_callback_env::print_state() {
    std::cout << "state : ";
    switch(Key_callback_env::state) {
    case normal:
	std::cout << "normal\n";
	break;

    case control:
	std::cout << "control\n";
	break;

    case shift:
	std::cout << "shift\n";
	break;

    case alt:
	std::cout << "alt\n";
	break;
    }

    std::cout <<"key_combi :";
    for (const auto& key : Key_callback_env::key_combi) {
	std::cout<< " " << key;
    }
    std::cout <<"\n\n";
}

void Key_callback_env::key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (action == GLFW_PRESS) {
        std::cout << "in key_callback, key: " << key << ", action: " << action << "\n" << std::flush;

	std::cout << "key press invoked transition " << std::flush;
	Key_callback_env::key_press_invoked_transition_map.invoke_func(key);
	std::cout << "\nget key_combi_code " << std::flush;
	int code = Key_callback_env::shared_key_combi_code.get_key_combi_code();
	std::cout << code;
	std::cout <<"\nkey_combi invoked function " << std::flush;
	Key_callback_env::key_combi_invoked_function_map.invoke_func(code);

	std::cout << "\n";
	Key_callback_env::print_state();
    } else if (action == GLFW_RELEASE) {
        std::cout << "in key_callback, key: " << key << ", action: " << action << "\n" << std::flush;

	std::cout << "key release invoked transition " << std::flush;
	Key_callback_env::key_release_invoked_transition_map.invoke_func(key);

	std::cout << "\n";
	Key_callback_env::print_state();
    }
}

void init_key_combi_code_map(const std::vector<Key>& key_combi) {
    Key_callback_env::key_combi_code_map.init_key_combi_code(key_combi);
}

void init_key_combi_code_map_by_keys(const std::vector<Key>& keys) {
    for (const auto& key : keys) {
	init_key_combi_code_map(std::vector<Key> { key });
    }
}

void init_key_press_invoked_transition_by_keys_keep_state(const std::vector<Key>& keys) { // { a, b, .. }, { caps_lock, esc, .. }
    for (const auto& key : keys) {
        auto func = [&, key]() {
            switch (Key_callback_env::state) {
                case normal:
                    Key_callback_env::key_combi.clear();
                    Key_callback_env::key_combi.push_back(key);
                    break;

                case control:
                    Key_callback_env::key_combi.push_back(key);
                    break;

                case shift:
                    Key_callback_env::key_combi.push_back(key);
                    break;

                case alt:
                    Key_callback_env::key_combi.push_back(key);
                    break;
            }

	    Key_callback_env::update_key_combi_code();
        };

	Key_callback_env::key_press_invoked_transition_map.init_func(key, func);
    }
}

void init_key_press_invoked_transition_by_keys_control(const std::vector<Key>& keys) { // { ctrl }
    for (const auto& key : keys) {
        auto func = [&, key] () {
            Key_callback_env::key_combi.clear();
            Key_callback_env::key_combi.push_back(key);
	    Key_callback_env::update_key_combi_code();           
            Key_callback_env::state = control;	    
	};

	Key_callback_env::key_press_invoked_transition_map.init_func(key, func);
    }
}

void init_key_press_invoked_transition_by_keys_shift(const std::vector<Key>& keys) { // { shift }
    for (const auto& key : keys) {
        auto func = [&, key] () {
            Key_callback_env::key_combi.clear();
            Key_callback_env::key_combi.push_back(key);
	    Key_callback_env::update_key_combi_code();	    
            Key_callback_env::state = shift;
        };

	Key_callback_env::key_press_invoked_transition_map.init_func(key, func);
    }
}

void init_key_press_invoked_transition_by_keys_alt(const std::vector<Key>& keys) { // { alt }
    for (const auto& key : keys) {
        auto func = [&, key] () {
            Key_callback_env::key_combi.clear();
            Key_callback_env::key_combi.push_back(key);
	    Key_callback_env::update_key_combi_code();
            Key_callback_env::state = alt;
        };

	Key_callback_env::key_press_invoked_transition_map.init_func(key, func);
    }
}

void init_key_combi_invoked_function_by_keys_loop(const std::vector<Key>& keys, auto send_key) {
    for (const auto& key : keys) {
	std::vector<Key> key_combi = { key };
	int code = Key_callback_env::key_combi_code_map.get_key_combi_code(key_combi);

        auto func = [&, key, key_combi, code]() {
	    auto is_thread_running = Key_callback_env::key_combi_thread_running_predicate_map.get_predicate(key_combi);

	    if (is_thread_running == false) {
		send_key(key);
	    
		std::thread([&, key_combi, code] {    
		    Key_callback_env::key_combi_thread_running_predicate_map.set_predicate(key_combi, true);
	    
		    constexpr auto loop_start_wait_duration = std::chrono::milliseconds(500);
		    constexpr auto loop_repeat_wait_duration = std::chrono::milliseconds(10);
                    std::this_thread::sleep_for(loop_start_wait_duration);
                    while (Key_callback_env::get_key_combi_code() == code) {
			send_key(key);
			std::this_thread::sleep_for(loop_repeat_wait_duration);
                    }
		    
		    Key_callback_env::key_combi_thread_running_predicate_map.set_predicate(key_combi, false);
		}).detach();
	    }
        };
	
	Key_callback_env::key_combi_invoked_function_map.init_func(code, func);
    }
}

void init_key_combi_invoked_function_by_keys_once(const std::vector<Key>& keys, auto send_key) {
    for (const auto& key : keys) {
	int code = Key_callback_env::key_combi_code_map.get_key_combi_code(std::vector<Key> { key });
	
        auto func = [&, key]() {
            send_key(key);
        };

	Key_callback_env::key_combi_invoked_function_map.init_func(code, func);
    }
}

void init_key_release_invoked_transition_by_keys_keep_state(const std::vector<Key>& keys) {
    for (const auto& key : keys) {
        auto func = [&, key]() {
            if (Key_callback_env::state == normal) {
		Key_callback_env::key_combi.clear();
            } else {
                for (auto rit = Key_callback_env::key_combi.rbegin(); rit != Key_callback_env::key_combi.rend(); rit++) {
                    int dist = std::distance(Key_callback_env::key_combi.begin(), rit.base());
                    if (dist == 1) break;
                    else if (*rit == key) {
			Key_callback_env::key_combi.pop_back();
                        break;
                    } else Key_callback_env::key_combi.pop_back();
                }
            }

            Key_callback_env::update_key_combi_code();
        };
	
	Key_callback_env::key_release_invoked_transition_map.init_func(key, func);
    }
}

void init_key_release_invoked_transition_by_keys_force_normal(const std::vector<Key>& keys) {
    // key_combi is cleared and state is transitioned to normal
    for (const auto& key : keys) {
        auto func = [&, key]() {
            Key_callback_env::key_combi.clear();
            Key_callback_env::update_key_combi_code();

            Key_callback_env::state = normal;
        };

	Key_callback_env::key_release_invoked_transition_map.init_func(key, func);
    }
}

void Key_callback_env::init_key_callback_env() {
    Key_callback_env::key_combi = { };
    Key_callback_env::state = normal;

    const std::vector<Key> keys_normal_loop  = { GLFW_KEY_A, GLFW_KEY_B, GLFW_KEY_C, GLFW_KEY_D, GLFW_KEY_E, GLFW_KEY_F, GLFW_KEY_G, GLFW_KEY_H, GLFW_KEY_I, GLFW_KEY_J, GLFW_KEY_K, GLFW_KEY_L, GLFW_KEY_M, GLFW_KEY_N, GLFW_KEY_O, GLFW_KEY_P, GLFW_KEY_Q, GLFW_KEY_R, GLFW_KEY_S, GLFW_KEY_T, GLFW_KEY_U, GLFW_KEY_V, GLFW_KEY_W, GLFW_KEY_X, GLFW_KEY_Y, GLFW_KEY_Z, GLFW_KEY_0, GLFW_KEY_1, GLFW_KEY_2, GLFW_KEY_3, GLFW_KEY_4, GLFW_KEY_5, GLFW_KEY_6, GLFW_KEY_7, GLFW_KEY_8, GLFW_KEY_9, GLFW_KEY_APOSTROPHE, GLFW_KEY_COMMA, GLFW_KEY_MINUS, GLFW_KEY_PERIOD, GLFW_KEY_SLASH, GLFW_KEY_SPACE, GLFW_KEY_ENTER, GLFW_KEY_SEMICOLON, GLFW_KEY_EQUAL, GLFW_KEY_LEFT_BRACKET, GLFW_KEY_RIGHT_BRACKET, GLFW_KEY_BACKSLASH, GLFW_KEY_GRAVE_ACCENT, GLFW_KEY_BACKSPACE, GLFW_KEY_DELETE, GLFW_KEY_RIGHT, GLFW_KEY_LEFT, GLFW_KEY_DOWN, GLFW_KEY_UP, GLFW_KEY_PAGE_UP, GLFW_KEY_PAGE_DOWN, GLFW_KEY_TAB };
    const std::vector<Key> keys_normal_once  = { GLFW_KEY_CAPS_LOCK, GLFW_KEY_INSERT, GLFW_KEY_ESCAPE, GLFW_KEY_NUM_LOCK, GLFW_KEY_PRINT_SCREEN };
    const std::vector<Key> keys_control = { GLFW_KEY_LEFT_CONTROL };
    const std::vector<Key> keys_shift = { GLFW_KEY_LEFT_SHIFT };
    const std::vector<Key> keys_alt = { GLFW_KEY_LEFT_ALT };

    auto send_key = [](Key key) {
	std::cout << "send key " << key << "\n" << std::flush;
    };

    init_key_combi_code_map_by_keys(keys_normal_loop);
    init_key_press_invoked_transition_by_keys_keep_state(keys_normal_loop);
    init_key_combi_invoked_function_by_keys_loop(keys_normal_loop, send_key);
    init_key_release_invoked_transition_by_keys_keep_state(keys_normal_loop);

    init_key_combi_code_map_by_keys(keys_normal_once);
    init_key_press_invoked_transition_by_keys_keep_state(keys_normal_once);
    init_key_combi_invoked_function_by_keys_once(keys_normal_once, send_key);
    init_key_release_invoked_transition_by_keys_keep_state(keys_normal_once);

    init_key_combi_code_map_by_keys(keys_control);
    init_key_press_invoked_transition_by_keys_control(keys_control);
    init_key_combi_invoked_function_by_keys_once(keys_control, send_key);
    init_key_release_invoked_transition_by_keys_force_normal(keys_control);

    init_key_combi_code_map_by_keys(keys_shift);
    init_key_press_invoked_transition_by_keys_shift(keys_shift);
    init_key_combi_invoked_function_by_keys_once(keys_shift, send_key);
    init_key_release_invoked_transition_by_keys_force_normal(keys_shift);

    init_key_combi_code_map_by_keys(keys_alt);
    init_key_press_invoked_transition_by_keys_alt(keys_alt);
    init_key_combi_invoked_function_by_keys_once(keys_alt, send_key);
    init_key_release_invoked_transition_by_keys_force_normal(keys_alt);
}


