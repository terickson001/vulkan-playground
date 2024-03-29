#version 450

layout(location = 0) in vec4 frag_color;
layout(location = 1) in vec2 frag_uv;

layout(location = 0) out vec4 outColor;

layout(binding = 1) uniform sampler2D tex0;

void main()
{
	vec4 color = vec4(frag_color.rgb, texture(tex0, frag_uv).r);
	outColor = color;
}