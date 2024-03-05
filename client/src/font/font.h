#pragma once

#include <string>
#include <vector>
#include <unordered_map>

#define GLM_FORCE_RADIANS
#define GLM_FORCE_DEPTH_ZERO_TO_ONE
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#define GLM_ENABLE_EXPERIMENTAL
#include <glm/gtx/hash.hpp>

using namespace std;

namespace font {
    struct Curve {
	Curve() {}
	Curve(glm::vec2 p1, glm::vec2 p2, glm::vec2 p3) : p1{ p1 }, p2{ p2 }, p3{ p3 } {}
    
	glm::vec2 p1;
	glm::vec2 p2;
	glm::vec2 p3;

	void print() const;
    };

    struct RenderBox {
	RenderBox() {}
	RenderBox(float x_min, float y_min, float x_max, float y_max) : x_min{ x_min }, y_min{ y_min }, x_max{ x_max }, y_max{ y_max } {}

	float x_min;
	float y_min;
	float x_max;
	float y_max;

	void print() const;
    };

    struct GlyphInfo {
	char glyph;
	RenderBox render_box;
	float advance_width;
	vector<Curve> curves;
	
	void generate_normalized_split_curves(int split_size);
	bool operator< (const GlyphInfo&);

	vector<vector<Curve>> split_left_curves;
	vector<vector<Curve>> split_right_curves;
	vector<vector<Curve>> split_up_curves;
	vector<vector<Curve>> split_down_curves;

	vector<Curve> normalized_curves;
	
	void print() const;
    };

    class FontInfo {
    public:
	FontInfo() {}
	FontInfo(string path) {
	    load_font(path);
	}
    
	void load_font(string str);

	string name;
	string ttf_path;
	float line_height;
	float em;
	unordered_map<char, int> glyph_index;
	vector<GlyphInfo> glyph_infos;

	void generate_font_buffers();

	// ordered by glyph_index

	vector<int> split_left_offset_buffer;
	vector<int> split_left_size_buffer;
	vector<Curve> split_left_curve_buffer;

	vector<int> split_right_offset_buffer;
	vector<int> split_right_size_buffer;
	vector<Curve> split_right_curve_buffer;

	vector<int> split_up_offset_buffer;
	vector<int> split_up_size_buffer;
	vector<Curve> split_up_curve_buffer;

	vector<int> split_down_offset_buffer;
	vector<int> split_down_size_buffer;
	vector<Curve> split_down_curve_buffer;

	vector<int> offset_buffer;
	vector<int> size_buffer;
	vector<Curve> curve_buffer;
	
	void print_font_buffers();

    private:
	int glyph_size;
	int split_size = 16;
    };

    
}
