#version 450
#extension GL_EXT_debug_printf : enable

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) in flat int fragCharId;

struct Curve {
    vec2 p1;
    vec2 p2;
    vec2 p3;
};

layout(binding = 2) buffer splitLeftOffsetBuffer { int leftOffsets[]; };
layout(binding = 3) buffer splitLeftSizeBuffer { int leftSizes[]; };
layout(binding = 4) buffer splitLeftCurveBuffer { Curve leftCurves[]; };
layout(binding = 5) buffer splitRightOffsetBuffer { int rightOffsets[]; };
layout(binding = 6) buffer splitRightSizeBuffer { int rightSizes[]; };
layout(binding = 7) buffer splitRightCurveBuffer { Curve rightCurves[]; };
layout(binding = 8) buffer splitUpOffsetBuffer { int upOffsets[]; };
layout(binding = 9) buffer splitUpSizeBuffer { int upSizes[]; };
layout(binding = 10) buffer splitUpCurveBuffer { Curve upCurves[]; };
layout(binding = 11) buffer splitDownOffsetBuffer { int downOffsets[]; };
layout(binding = 12) buffer splitDownSizeBuffer { int downSizes[]; };
layout(binding = 13) buffer splitDownCurveBuffer { Curve downCurves[]; };

layout(binding = 14) buffer offsetBuffer { int offsets[]; };
layout(binding = 15) buffer sizeBuffer { int sizes[]; };
layout(binding = 16) buffer curveBuffer { Curve curves[]; };

layout(location = 0) out vec4 outColor;

// int splitSize = 16;   

const float kQuadraticEpsilon = 0.0001;

vec2 pixelsPerEm = vec2(1.0 / fwidth(fragTexCoord.x),
			1.0 / fwidth(fragTexCoord.y));

float get_coverage_right() {
    const int offset = offsets[fragCharId];
    const int size = sizes[fragCharId];

    float coverage = 0.0;
    
    for (int i = offset; i < offset + size; i++) {
	Curve curve = curves[i];
	
	vec2 p1 = curve.p1 - fragTexCoord;
	vec2 p2 = curve.p2 - fragTexCoord;
	vec2 p3 = curve.p3 - fragTexCoord;

	uint code = (0x2E74U >> ((p1.y > 0.0 ? 2U : 0U) +
				 (p2.y > 0.0 ? 4U : 0U) +
				 (p3.y > 0.0 ? 8U : 0U))) & 3U;

	// c(t) = (1 - t)^2 p1 + 2t(1 - t) p2 + t^2 p3
	// c(t) = a t^2 -2b t + c
	
	if (code != 0U) {
	    float ax = p1.x - 2.0 * p2.x + p3.x;
	    float ay = p1.y - 2.0 * p2.y + p3.y;
	    float bx = p1.x - p2.x;
	    float by = p1.y - p2.y;
	    float cx = p1.x;
	    float cy = p1.y;

	    float d = sqrt(max(by * by - ay * p1.y, 0.0));
	    float t1 = (by - d) / ay;
	    float t2 = (by + d) / ay;

	    if (abs(ay) < kQuadraticEpsilon) {
		t1 = t2 = p1.y * 0.5 / by;
	    }

	    float c1x = ax * t1 * t1 - 2.0 * bx * t1 + cx;
	    float c2x = ax * t2 * t2 - 2.0 * bx * t2 + cx;

	    c1x = clamp(c1x * pixelsPerEm.x + 0.5, 0.0, 1.0);
	    c2x = clamp(c2x * pixelsPerEm.x + 0.5, 0.0, 1.0);

	    if ((code & 1U) != 0U) coverage += c1x;
	    if (code > 1U) coverage -= c2x;
	}	
    }

    return coverage;
}

float get_coverage_up() {
    const int offset = offsets[fragCharId];
    const int size = sizes[fragCharId];

    float coverage = 0.0;
    
    for (int i = offset; i < offset + size; i++) {
	Curve curve = curves[i];
	
	vec2 p1 = curve.p1 - fragTexCoord;
	vec2 p2 = curve.p2 - fragTexCoord;
	vec2 p3 = curve.p3 - fragTexCoord;

	p1 = vec2(p1.y, -1.0 * p1.x);
	p2 = vec2(p2.y, -1.0 * p2.x);
	p3 = vec2(p3.y, -1.0 * p3.x);
		
	uint code = (0x2E74U >> ((p1.y > 0.0 ? 2U : 0U) +
				 (p2.y > 0.0 ? 4U : 0U) +
				 (p3.y > 0.0 ? 8U : 0U))) & 3U;

	// c(t) = (1 - t)^2 p1 + 2t(1 - t) p2 + t^2 p3
	// c(t) = a t^2 -2b t + c
	
	if (code != 0U) {
	    float ax = p1.x - 2.0 * p2.x + p3.x;
	    float ay = p1.y - 2.0 * p2.y + p3.y;
	    float bx = p1.x - p2.x;
	    float by = p1.y - p2.y;
	    float cx = p1.x;
	    float cy = p1.y;

	    float d = sqrt(max(by * by - ay * p1.y, 0.0));
	    float t1 = (by - d) / ay;
	    float t2 = (by + d) / ay;

	    if (abs(ay) < kQuadraticEpsilon) {
		t1 = t2 = p1.y * 0.5 / by;
	    }

	    float c1x = ax * t1 * t1 - 2.0 * bx * t1 + cx;
	    float c2x = ax * t2 * t2 - 2.0 * bx * t2 + cx;

	    c1x = clamp(c1x * pixelsPerEm.x + 0.5, 0.0, 1.0);
	    c2x = clamp(c2x * pixelsPerEm.x + 0.5, 0.0, 1.0);

	    if ((code & 1U) != 0U) coverage += c1x;
	    if (code > 1U) coverage -= c2x;
	}	
    }

    return coverage;
}

void main() {
    float coverage = 0.0;

    coverage += get_coverage_right();
    coverage += get_coverage_up();    
    coverage = sqrt(clamp(abs(coverage) / 2, 0.0, 1.0));

    outColor = vec4((vec3(1.0) * (1.0 - coverage) + fragColor.rgb * coverage) , fragColor.a);
    
    if (coverage == 0.0)
	debugPrintfEXT("fragTexcoord %1.2v2f\n outColor %1.2v4f\n coverage %f\n", fragTexCoord, outColor, coverage);
    
    // if (0.1 < fragTexCoord.x && fragTexCoord.x < 0.15 && 0.1 < fragTexCoord.y && fragTexCoord.y < 0.15)
    // 	debugPrintfEXT("fragTexcoord %1.2v2f\n fragColor %1.2v4f\n coverage %f\n", fragTexCoord, fragColor, coverage);
    
    // debugPrintfEXT("fwidth %1.2v2f\n", fwidth(fragTexCoord));    
    // debugPrintfEXT("fragTexcoord %1.2v2f\n", fragTexCoord);
    // debugPrintfEXT("fragCharId %d\n", fragCharId);
    // debugPrintfEXT("offset %d\n", offset);
    // debugPrintfEXT("curve p1 %1.2v2f\n", curve.p1);
}
