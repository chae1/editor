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

layout(location = 0) out vec4 outColor;

void main() {
    int leftOffset = leftOffsets[fragCharId];
    

    outColor = fragColor;

    // debugPrintfEXT("fragTexcoord %1.2v2f\n", fragTexCoord);
    // debugPrintfEXT("fragCharId %d\n", fragCharId);
    debugPrintfEXT("leftOffset %d\n", leftOffset);
}
