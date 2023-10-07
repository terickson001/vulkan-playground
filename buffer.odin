package main

import "core:os"
import "core:fmt"
import "core:math"
import "shared:stb/stbtt"

Buffer :: struct
{
	text: []byte,
}

buffer_draw :: proc(ctx: ^Context, buffer: ^Buffer, pos: [2]f32)
{
	pos := pos;
	renderer := &ctx.renderers[ctx.curr_frame];
	//fmt.println(renderer.curr_job.font.ascent);
	pos.y += f32(renderer.curr_job.font.ascent);
	
	line_start := pos.x;
	start := 0;
	idx := 0;
	for c in buffer.text
	{
		if c == '\n'
		{
			line := buffer.text[start:idx];
			start = idx+1;
			renderer_draw_text_ft(renderer, string(line), pos, {1, 1, 1, 1});
			pos.y += f32(renderer.curr_job.font.height);
			pos.x = line_start;
		}
		idx += 1;
	}
	line := buffer.text[start:idx];
	renderer_draw_text_ft(renderer, string(line), pos, {1, 1, 1, 1});
	pos.y += f32(renderer.curr_job.font.height);
	pos.x = line_start;
}