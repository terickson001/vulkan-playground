package main

import "core:os"
import "core:fmt"
import "core:strings"

import ft "shared:freetype"

Baked_Char :: struct
{
	x0, y0, x1, y1: u32,
	xoff, yoff, xadvance: u32,
}

Font :: struct
{
	image: Image,
	glyphs: []Baked_Char,
	height: u32,
	ascent: u32,
	descent: u32,
	line_gap: u32,
	size: u32,
	id_uv: [2]f32,
}

build_font :: proc(ctx: ^Context, path: string, size: int, oversampling := 1) -> Font
{
	lib: ft.Library;
	face: ft.Face;
	
	cpath := strings.clone_to_cstring(path, context.temp_allocator);
	fp_size := cast(ft.F26Dot6)(size << 6);
	
	fmt.println(ft.Init_FreeType(&lib));
	fmt.println(ft.New_Face(lib, cpath, 0, &face));
	fmt.println(ft.Set_Char_Size(face, 0, fp_size, 96, 96));
	
	font: Font;
	
	first_rune := 0;
	last_rune  := 255;
	font.glyphs = make([]Baked_Char, last_rune-first_rune+1);
	
	width := u32(256);
	out := make([]byte, width*width);
	defer delete(out);
	retry: for {
		pos: [2]u32;
		max_y: u32;
		for i in 0..=u32(last_rune - first_rune)
		{
			ft.Load_Char(face, i, nil);
			ft.Render_Glyph(face.glyph, .Normal);
			bmp := &face.glyph.bitmap;
			if pos.x + bmp.width >= width
			{
				pos.x = 0;
				pos.y += max_y + u32(oversampling);
				max_y = 0;
			}
			if pos.y + bmp.rows >= width
			{
				delete(out);
				width *= 2;
				out = make([]byte, width*width);
				continue retry;
			}
			
			max_y = max(max_y, bmp.rows);
			
			for row in 0..<bmp.rows
			{
				for col in 0..<bmp.width
				{
					x := pos.x + col;
					y := pos.y + row;
					p := bmp.buffer[row * u32(bmp.pitch) + col];
					out[y * width + x] = p;
				}
			}
			
			font.glyphs[i] = {
				x0 = pos.x,
				y0 = pos.y,
				x1 = pos.x + bmp.width,
				y1 = pos.y + bmp.rows,
				
				xoff = u32(face.glyph.bitmap_left),
				yoff = u32(face.glyph.bitmap_top),
				xadvance = u32(face.glyph.advance.x >> 6),
			};
			
			pos.x += bmp.width + u32(oversampling);
		}
		break;
	}
	font.glyphs['\t'].xadvance = font.glyphs[' '].xadvance * 4;
	
	font.size = u32(size);
	font.ascent = u32(face.size.metrics.ascender>>6);
	font.descent = u32(face.size.metrics.descender>>6);
	font.height = u32(face.size.metrics.height>>6);
	font.line_gap = font.height - (font.ascent + font.descent);
	fmt.printf("Loaded Font: %q: %dx%d\n", path, width, width);
	font.image = create_texture_from_mem(ctx, out, int(width), int(width), 1, .R8_SRGB);
	
	ft.Done_Face(face);
	ft.Done_FreeType(lib);
	
	return font;
}