#include "fontInfo.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <filesystem>
#include <algorithm>

#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fmt/format.h>

namespace fs = std::filesystem;
using namespace font;

void Curve::print() const {
    cout << fmt::format("p1({}, {}), p2({}, {}), p3({}, {})\n", p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
}

void RenderBox::print() const {
    cout << fmt::format("x_min, y_min, x_max, y_max : {}, {}, {}, {}\n", x_min, y_min, x_max, y_max);
}

bool GlyphInfo::operator<(const GlyphInfo& glyph_info) {
    return glyph < glyph_info.glyph;
}

inline istringstream get_newline(ifstream& fin) {
    string str;
    getline(fin, str);
    return istringstream(str);
}

void FontInfo::load_font(string str) {
    fs::path p(str);
    if (!fs::is_directory(p)) {
	cerr << fmt::format("directory does not exist: {}\n", str);
	return;
    }

    for (auto& entry : fs::directory_iterator(p)) {
	ifstream fin(entry.path());

	GlyphInfo glyph_info;    
	while (fin.good()) {
	    istringstream line = get_newline(fin);
	
	    string currline_token;
	    line >> currline_token;

	    if (currline_token == "glyph") {
		line = get_newline(fin);	    
		line >> glyph_info.glyph;
	    
	    } else if (currline_token == "curves") {
		line = get_newline(fin);	    

		while (line.peek() != EOF) {
		    auto& curves = glyph_info.curves;
		    glm::vec2 p1, p2, p3;
		    line >> p1.x >> p1.y >> p2.x >> p2.y >> p3.x >> p3.y;		
		    curves.push_back(Curve(p1, p2, p3));
		    
		    line = get_newline(fin);
		}
	    
	    } else if (currline_token == "advance-width") {
		line = get_newline(fin);
		line >> glyph_info.advance_width;

	    } else if (currline_token == "render-box") {
		line = get_newline(fin);
		RenderBox& box = glyph_info.render_box;
		line >> box.x_min >> box.y_min >> box.x_max >> box.y_max;

	    } else if(currline_token == "glyph-info-end") {
		glyph_infos.push_back(glyph_info);
		
	    } else if (currline_token == "font") {
		line = get_newline(fin);
		line >> name;
	    
	    } else if (currline_token == "ttf-path") {
		line = get_newline(fin);
		line >> ttf_path;
	    
	    } else if (currline_token == "line-height") {
		line = get_newline(fin);
		line >> line_height;
		    
	    } else if (currline_token == "em") {
		line = get_newline(fin);
		line >> em;
		    
	    } else if (currline_token == "font-info-end") {}
	}
	
	if (fin.eof()) {}
	else if (fin.fail()) {
	    cerr << "failed\n";
	    return;
	}
    }

    sort(glyph_infos.begin(), glyph_infos.end());
    for (int i = 0; i < static_cast<int>(glyph_infos.size()); i++) {
	GlyphInfo glyph_info = glyph_infos[i];		    
	glyph_index.insert({ glyph_info.glyph, i });
    }

    glyph_size = glyph_infos.size();

    for (auto& glyph_info : glyph_infos) {
	glyph_info.generate_normalized_split_curves(split_size);
    }
}

inline bool is_curve_possibly_in_box(const Curve& curve, int x_min, int y_min, int x_max, int y_max) {
    for (const auto& p : { curve.p1, curve.p2, curve.p3 }) {
	if (x_min <= p.x && y_min <= p.y && p.x < x_max && p.y < y_max) {
	    return true;
	}
    }
    
    return false;
}

void GlyphInfo::generate_normalized_split_curves(int split_size) {
    float width = render_box.x_max;
    float height = render_box.y_max;

    int n = split_size;
    for (int i = 0; i < n; i++) {
	split_left_curves.push_back(vector<Curve>());
	split_right_curves.push_back(vector<Curve>());
	split_up_curves.push_back(vector<Curve>());
	split_down_curves.push_back(vector<Curve>());
	
	for (const auto& curve : curves) {
	    if (is_curve_possibly_in_box(curve, 0, height*i/n, width/2, height*(i+1)/n)) {
		split_left_curves[i].push_back(Curve{ { curve.p1.x/width, curve.p1.y/height }, { curve.p2.x/width, curve.p2.y/height }, { curve.p3.x/width, curve.p3.y/height } });
	    }
	    
	    if (is_curve_possibly_in_box(curve, width/2, height*i/n, width, height*(i+1)/n)) {
		split_right_curves[i].push_back(Curve{ { curve.p1.x/width, curve.p1.y/height }, { curve.p2.x/width, curve.p2.y/height }, { curve.p3.x/width, curve.p3.y/height } });
	    }

	    if (is_curve_possibly_in_box(curve, width*i/n, height/2, width*(i+1)/n, height)) {
		split_up_curves[i].push_back(Curve{ { curve.p1.x/width, curve.p1.y/height }, { curve.p2.x/width, curve.p2.y/height }, { curve.p3.x/width, curve.p3.y/height } });
	    }

	    if (is_curve_possibly_in_box(curve, width*i/n, 0, width*(i+1)/n, height/2)) {
		split_down_curves[i].push_back(Curve{ { curve.p1.x/width, curve.p1.y/height }, { curve.p2.x/width, curve.p2.y/height }, { curve.p3.x/width, curve.p3.y/height } });
	    }
	}
    }

    for (int i = 0; i < n; i++) {
	sort(split_left_curves[i].begin(), split_left_curves[i].end(), [](const Curve& c1, const Curve& c2) { return min({ c1.p1.x, c1.p2.x, c1.p3.x }) < min({ c2.p1.x, c2.p2.x, c2.p3.x }); });

	sort(split_right_curves[i].begin(), split_right_curves[i].end(), [](const Curve& c1, const Curve& c2) { return max({ c1.p1.x, c1.p2.x, c1.p3.x }) > max({ c2.p1.x, c2.p2.x, c2.p3.x }); });

	sort(split_up_curves[i].begin(), split_up_curves[i].end(), [](const Curve& c1, const Curve& c2) { return max({ c1.p1.y, c1.p2.y, c1.p3.y }) > max({ c2.p1.y, c2.p2.y, c2.p3.y }); });

	sort(split_down_curves[i].begin(), split_down_curves[i].end(), [](const Curve& c1, const Curve& c2) { return min({ c1.p1.y, c1.p2.y, c1.p3.y }) < min({ c2.p1.y, c2.p2.y, c2.p3.y }); });
    }

    for (auto& curve : curves) {
	normalized_curves.push_back(Curve{ { curve.p1.x/width, curve.p1.y/height }, { curve.p2.x/width, curve.p2.y/height }, { curve.p3.x/width, curve.p3.y/height } });
    }
}

void GlyphInfo::print() const {    
    float width = render_box.x_max;
    float height = render_box.y_max;

    int split_size = split_left_curves.size();
    int& n = split_size;
    
    cout << fmt::format("{}\n", glyph);
    cout << "left\n";
    for (int i = 0; i < split_size; i++) {
	cout << fmt::format("{} ", split_left_curves[i].size());
    }
    cout << endl;

    for (int i = 0; i < split_size; i++) {
	cout << fmt::format("\nbox i = {}, x_min : {}, y_min : {}, x_max : {}, y_max : {}\n", i, 0, height*i/n, width/2, height*(i+1)/n);
	for (const auto& curve : split_left_curves[i]) {
	    curve.print();
	    cout << fmt::format("p1({}, {}), p2({}, {}), p3({}, {})\n", curve.p1.x, curve.p1.y, curve.p2.x, curve.p2.y, curve.p3.x, curve.p3.y);
	}
    }
    
    cout << "right\n";
    for (int i = 0; i < split_size; i++) {
	cout << fmt::format("{} ", split_right_curves[i].size());
    }
    cout << endl;

    for (int i = 0; i < split_size; i++) {
	cout << fmt::format("\nbox i = {}, x_min : {}, y_min : {}, x_max : {}, y_max : {}\n", i, width/2, height*i/n, width, height*(i+1)/n);
	for (const auto& curve : split_right_curves[i]) {
	    cout << fmt::format("p1({}, {}), p2({}, {}), p3({}, {})\n", curve.p1.x, curve.p1.y, curve.p2.x, curve.p2.y, curve.p3.x, curve.p3.y);
	}
    }
    
    cout << "up\n";
    for (int i = 0; i < split_size; i++) {
	cout << fmt::format("{} ", split_up_curves[i].size());
    }
    cout << endl;

    for (int i = 0; i < split_size; i++) {
	cout << fmt::format("\nbox i = {}, x_min : {}, y_min : {}, x_max : {}, y_max : {}\n", i, width*i/n, height/2, width*(i+1)/n, height);
	for (const auto& curve : split_up_curves[i]) {
	    cout << fmt::format("p1({}, {}), p2({}, {}), p3({}, {})\n", curve.p1.x, curve.p1.y, curve.p2.x, curve.p2.y, curve.p3.x, curve.p3.y);
	}
    }
    
    cout << "down\n";
    for (int i = 0; i < split_size; i++) {
	cout << fmt::format("{} ", split_down_curves[i].size());
    }
    cout << endl;

    for (int i = 0; i < split_size; i++) {
	cout << fmt::format("\nbox i = {}, x_min : {}, y_min : {}, x_max : {}, y_max : {}\n", i, width*i/n, 0, width*(i+1)/n, height/2);
	for (const auto& curve : split_down_curves[i]) {
	    cout << fmt::format("p1({}, {}), p2({}, {}), p3({}, {})\n", curve.p1.x, curve.p1.y, curve.p2.x, curve.p2.y, curve.p3.x, curve.p3.y);
	}
    }
}

void FontInfo::generate_font_buffers() {
    split_left_offset_buffer.resize(glyph_size * split_size);
    split_left_size_buffer.resize(glyph_size * split_size);

    split_right_offset_buffer.resize(glyph_size * split_size);
    split_right_size_buffer.resize(glyph_size * split_size);

    split_up_offset_buffer.resize(glyph_size * split_size);
    split_up_size_buffer.resize(glyph_size * split_size);

    split_down_offset_buffer.resize(glyph_size * split_size);
    split_down_size_buffer.resize(glyph_size * split_size);
    
    int left_offset = 0;
    int right_offset = 0;
    int up_offset = 0;
    int down_offset = 0;
    
    for (int i = 0; i < glyph_size; i++) {
	for (int j = 0; j < split_size; j++) {
	    split_left_offset_buffer[i * split_size + j] = left_offset;
	    split_right_offset_buffer[i * split_size + j] = right_offset;
	    split_up_offset_buffer[i * split_size + j] = up_offset;
	    split_down_offset_buffer[i * split_size + j] = down_offset;

	    int left_curr_curve_size = glyph_infos[i].split_left_curves[j].size();
	    int right_curr_curve_size = glyph_infos[i].split_right_curves[j].size();
	    int up_curr_curve_size = glyph_infos[i].split_up_curves[j].size();
	    int down_curr_curve_size = glyph_infos[i].split_down_curves[j].size();

	    split_left_size_buffer[i * split_size + j] = left_curr_curve_size;
	    split_right_size_buffer[i * split_size + j] = right_curr_curve_size;
	    split_up_size_buffer[i * split_size + j] = up_curr_curve_size;
	    split_down_size_buffer[i * split_size + j] = down_curr_curve_size;
	    
	    left_offset += left_curr_curve_size;
	    right_offset += right_curr_curve_size;
	    up_offset += up_curr_curve_size;
	    down_offset += down_curr_curve_size;
	}
    }

    split_left_curve_buffer.resize(left_offset);
    split_right_curve_buffer.resize(right_offset);
    split_up_curve_buffer.resize(up_offset);
    split_down_curve_buffer.resize(down_offset);

    for (int i = 0; i < glyph_size; i++) {
	for (int j = 0; j < split_size; j++) {
	    auto& left_curves = glyph_infos[i].split_left_curves[j];
	    memcpy(split_left_curve_buffer.data() + split_left_offset_buffer[i * split_size + j], left_curves.data(), sizeof(Curve) * left_curves.size());
	    
	    auto& right_curves = glyph_infos[i].split_right_curves[j];
	    memcpy(split_right_curve_buffer.data() + split_right_offset_buffer[i * split_size + j], right_curves.data(), sizeof(Curve) * right_curves.size());

	    auto& up_curves = glyph_infos[i].split_up_curves[j];
	    memcpy(split_up_curve_buffer.data() + split_up_offset_buffer[i * split_size + j], up_curves.data(), sizeof(Curve) * up_curves.size());

	    auto& down_curves = glyph_infos[i].split_down_curves[j];
	    memcpy(split_down_curve_buffer.data() + split_down_offset_buffer[i * split_size + j], down_curves.data(), sizeof(Curve) * down_curves.size());
	}
    }

    int offset = 0;
    for (const auto& glyph_info : glyph_infos) {
	const auto& curves = glyph_info.normalized_curves;

	offset_buffer.push_back(offset);
	offset += curves.size();
	size_buffer.push_back(curves.size());
	
	for (const auto& curve : curves) {
	    curve_buffer.push_back(curve);
	}	
    }
}
	 
void FontInfo::print_font_buffers() {
    int i = 1;
    if (static_cast<int>(glyph_infos.size()) > i) {
	glyph_infos[i].render_box.print();
    }

    fmt::print("\nglyph {}\n", glyph_infos[i].glyph);
    int offset = offset_buffer[i];
    int size = size_buffer[i];

    for (int k = offset; k < offset + size; k++) {
	curve_buffer[k].print();
    }

    fmt::print("{}\n", size_buffer);

    fmt::print("\n");
    for (size_t i = 0; i < glyph_infos.size(); i++) {
	fmt::print("i {} glyph {}\n", i, glyph_infos[i].glyph);
    }
}

void FontInfo::generate_glyph_map() {
    for (size_t i = 0; i < glyph_infos.size(); i++) {
	glyph_map[glyph_infos[i].glyph] = i;
    }
}
