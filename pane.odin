package main

import "core:os"
import "core:fmt"

Pane :: struct
{
	buffer: ^Buffer,
	scroll: [2]f32,
	cursors: []u64,
	font_size: u16,
	dims: [2]f32,
}

pane_attach_buffer :: proc(pane: ^Pane, buffer: ^Buffer)
{
	pane.buffer = buffer;
}

pane_draw :: proc(ctx: ^Context, pane: ^Pane)
{
	buffer_draw(ctx, pane.buffer, pane.scroll);
}